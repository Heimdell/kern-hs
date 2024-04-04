{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -freverse-errors #-}

module Eval where

import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as Map
import Data.IntMap qualified as IntMap

import Machine
import Interop

import Debug.Trace

-- Исполнение кода.
--
eval :: Value -> LispM Value
eval val = do
  case val of
    Cons {car = fun, cdr = args} -> do
      fun <- eval fun
      apply fun args

    Atom (Name var) -> retrieve var
    other           -> pure other

match :: Value -> Value -> LispM Env
match fact form = case (fact, form) of
  (Cons left right, Cons val form) -> do
    ef   <- match left  val
    de   <- match right form
    return (ef <> de)

  (Atom (Name name), val) -> do
    addr <- alloc val
    return (Map.singleton name addr)

  (Nil, Nil) -> do
    return Map.empty

  _ -> do
    throwError PatternMatchFail {form, fact}

alloc :: Value -> LispM Int
alloc val = do
  mem <- get
  put mem
    { mem = IntMap.insert mem.ptr val mem.mem
    , ptr = mem.ptr + 1
    }
  return mem.ptr

withEnv :: (Env -> Env) -> LispM Value -> LispM Value
withEnv f ma = do
  old  <- gets (.env)
  _    <- modify \s -> (s :: Memory) { env = f old }
  res  <- ma
  _    <- modify \s -> (s :: Memory) { env = old }
  return res

-- Вызов функций.
--
apply :: Value -> Value -> LispM Value
apply fun args = case fun of
  Atom (Func func) -> func args
  Atom (Lam (Lambda e a b)) -> do
    args <- prebake args
    de   <- match a args `catchError` \case
      PatternMatchFail {} -> throwError PatternMatchFail {fact = args, form = a}
      e                   -> throwError e
    withEnv (const (de <> e)) do
      eval b

  Atom (Macro (M a b)) -> do
    de <- match a args `catchError` \case
      PatternMatchFail {} -> throwError PatternMatchFail {fact = args, form = a}
      e                   -> throwError e
    withEnv (de <>) do
      liftIO . print =<< get
      eval b

  _ ->
    case args of
      Nil -> pure fun
      _   -> eval args

-- Чтение переменной.
--
retrieve :: String -> LispM Value
retrieve name = do
  memory <- get
  case Map.lookup name memory.env of
    Just address -> do
      case IntMap.lookup address memory.mem of
        Just value -> pure value
        Nothing    -> error $ name <> "$" <> show address <> " has no allocated value"
    Nothing -> do
      throwError $ NoSuchBinding name

instance (HasTypeName res, HasTypeName arg) => HasTypeName (Fun arg res) where
  typename = typename @arg <> " -> " <> typename @res

instance (FromLisp a, ToLisp b) => ToLisp (Fun a b) where
  toLisp f = Atom $ Func \a -> do
    a <- prebake a  -- у встроенных функций нет доступа к env и аргументы предвычислены
    a <- eitherToLispM do fromLisp a
    b <- f.run a
    return (toLisp b)

-- Предвычислить агрументы.
--
prebake :: Value -> LispM Value
prebake = \case
  Cons {car, cdr} -> do
    car <- eval    car
    cdr <- prebake cdr
    return Cons {car, cdr}

  other -> eval other
