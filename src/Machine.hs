{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -freverse-errors #-}

module Machine where

import Data.Map    qualified as Map
import Data.IntMap qualified as IntMap
import Control.Monad.Except
import Control.Monad.State

-- Дерево программы.
--
data Value
  = Cons {car, cdr :: Value}
  | Nil
  | Atom Atom
  deriving (Eq)

-- Атомарное выражение.
--
data Atom
  = Name   String
  | String String
  | Number Double
  | Bool   Bool
  | Func   (Function Value Value)
  | Macro  Macro
  | Lam    Lambda
  | Env    Env
  deriving (Eq)

data Lambda = Lambda
  { env  :: Env
  , args :: Value
  , body :: Value
  }
  deriving (Eq)

data Macro = M
  { args :: Value
  , body :: Value
  }
  deriving (Eq)

-- Операция над лисп-машиной.
--
type Function a b = a -> LispM b

-- Обёртки для спецформ и функций. Функции не имеют доступа к памяти/энвайронменту.
--
newtype SF  a b = SF  { run :: Function a b }
newtype Fun a b = Fun { run :: Function a b }

instance Eq (Function a b) where
  _ == _ = False

instance Eq (Fun a b) where
  _ == _ = False

-- Обёртка для символа.
--
newtype Symbol = Symbol { name :: String }

-- Лисп-машина.
--
type LispM =
   StateT  Memory  -- память
  (ExceptT Error   -- выброс специфических ошибок
   IO)             -- общий ввод-вывод

data Memory = Memory
  { env :: Env  -- окружение (в стейте, поеребуется аккуратно работать в лямбдах)
  , mem :: Mem  -- память со значениями
  , ptr :: Int  -- конец кучи
  } deriving (Show)

-- Ошибки лисп-машины.
--
data Error
  = TypeError { ty_ :: String, value :: Value }
  | NoSuchBinding { name :: String}
  | PatternMatchFail { form, fact :: Value }
  | ParsingFailed { msg :: String }

type Env = Map.Map String Int   -- мапа адресов
type Mem = IntMap.IntMap Value  -- память как sparse array

-- Запуск лисп-машины.
--
runLispM :: Memory -> LispM a -> IO (Either Error a)
runLispM mem action = do
  runExceptT (evalStateT action mem )

instance Show Atom where
  show = \case
    Name   s     -> s
    String s     -> show s
    Bool   True  -> "#true"
    Bool   False -> "#false"
    Func   _     -> "<builtin>"
    Lam    l     -> show l
    Env    _     -> "<env>"
    Number d     -> if fromIntegral f == d then show f else show d
      where
        f = round d

instance Show Lambda where
  show = \case
    Lambda _ a b -> show (Cons (Atom (Name "lambda")) (Cons a (Cons b Nil)))

instance Show Value where
  show = \case
    x@Cons{} -> "(" <> show' x <> ")"
    other    -> show' other
    where
      show' = \case
        Cons {car, cdr} -> show car <>
          ( case cdr of
              Nil     -> ""
              Cons {} -> " "   <> show' cdr
              Atom {} -> " . " <> show' cdr
          )
        Nil -> "()"
        Atom atom -> show atom

instance Show Error where
  show = \case
    TypeError ty val -> do
      "expected type " <> show ty <> ", but got " <> show val

    NoSuchBinding name -> do
      "name " <> name <> " is not bound"

    PatternMatchFail {fact, form} ->
      "list " <> show fact <> " has failed to match against pattern " <> show form

    ParsingFailed {msg} -> do
      "parsing failed:\n" <> msg

{-
data Value
  = Cons {car, cdr :: Value}
  | Nil
  | Atom Atom

-- Атомарное выражение.
--
data Atom
  = Name   String
  | String String
  | Number Double
  | Bool   Bool
  | Func   (Function Value Value)
  | Lam    Lambda
  | Env    Env
-}