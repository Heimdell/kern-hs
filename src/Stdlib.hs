module Stdlib where

import Control.Monad.State
import Control.Monad.Except
import Data.Map    qualified as Map
import Data.IntMap qualified as IntMap
import Data.Function
import System.Console.Isocline

import Machine
import Interop
import Eval
import ParsingTools
import Parser
import Input

import Debug.Trace

-- Вернуть аргументы как список.
--
listF = toLisp @(Fun [Value] [Value]) $ Fun pure

-- Вернуть аргумент не-вычисленным.
--
quoteSF = toLisp @(SF (Value, ()) Value) $ SF \(a, ()) -> pure a

-- Связать имя со значением.
--
defSF = toLisp @(SF (Symbol, (Value, ())) ()) $ SF \(Symbol sym, (val, ())) -> do
  val <- eval val
  modify (define sym val)

-- Изменить значение, связанной с именем. Если имя не связано, ошибка.
--
setSF = toLisp @(SF (Symbol, (Value, ())) ()) $ SF \(Symbol sym, (val, ())) -> do
  val <- eval val
  mem <- get
  mem <- eitherToLispM (set sym val mem)
  put mem

getEnvSF = toLisp @(SF () Env) $ SF \() -> do
  gets (.env)

-- Форсировать вычисление.
--
evalSF = toLisp @(SF (Value, (Value, ())) Value) $ SF \(a, (env, ())) -> do
  a   <- eval a
  old <- gets (.env)
  env <- eval env
  env <- eitherToLispM do fromLisp env
  a   <- eval a
  _   <- modify \s -> (s :: Memory) {env = env}
  res <- eval a
  _   <- modify \s -> (s :: Memory) {env = old}
  return res

exitF = toLisp @(Fun () ()) $ Fun \() -> error "bye!"

readF = toLisp @(Fun (String, ()) Value) $ Fun \(prompt, ()) -> do
  txt <- liftIO do readline prompt
  case runParser value (fromString "<stdin>" txt) of
    Right val -> return val
    Left  msg -> throwError ParsingFailed {msg}

-- Распечатать термы.
--
printF = toLisp @(Fun [Value] ()) $ Fun \args -> do
  liftIO $ putStr $ concatMap show' args
  where
    show' = \case
      Atom (String s) -> s
      Cons a b        -> show' a ++ show' b
      s               -> show s

-- Вычислить аргументы последовательно, вернуть последний.
--
doF = toLisp @(Fun [Value] Value) $ Fun \args -> return (head (reverse args <> [Nil]))

-- Вычислить аргументы последовательно, вернуть последний.
--
geqF = toLisp @(Fun (Double, (Double, ())) Bool) $ Fun \(a, (b, ())) -> return (a >= b)
leqF = toLisp @(Fun (Double, (Double, ())) Bool) $ Fun \(a, (b, ())) -> return (a <= b)
gtF  = toLisp @(Fun (Double, (Double, ())) Bool) $ Fun \(a, (b, ())) -> return (a >  b)
eqF  = toLisp @(Fun (Value,  (Value,  ())) Bool) $ Fun \(a, (b, ())) -> return (a == b)

-- IF
--
condSF = toLisp @(SF (Value, (Value, (Value, ()))) Value) $ SF \(b, (y, (n, ()))) -> do
  b <- eval b
  b <- eitherToLispM (fromLisp b)
  if b then eval y else eval n

-- Арифметика.
--
plusF   = toLisp @(Fun (Double, (Double, ())) Double) $ Fun \(a, (b, ())) -> pure (a + b)
minusF  = toLisp @(Fun (Double, (Double, ())) Double) $ Fun \(a, (b, ())) -> pure (a - b)
divF    = toLisp @(Fun (Double, (Double, ())) Double) $ Fun \(a, (b, ())) -> pure (a / b)
modF    = toLisp @(Fun (Double, (Double, ())) Double) $ Fun \(a, (b, ())) -> pure (fromIntegral (mod (round a) (round b)))
appendF = toLisp @(Fun (String, (String, ())) String) $ Fun \(a, (b, ())) -> pure (a <> b)

lambdaSF = toLisp @(SF (Value, (Value, ())) Value) $ SF \(args, (body, ())) -> do
  e <- gets (.env)
  pure $ Atom $ Lam $ Lambda e args body

macroSF = toLisp @(SF (Value, (Value, ())) Value) $ SF \(args, (body, ())) -> do
  pure $ Atom $ Macro $ M args body

-- Реализация `def`.
--
define :: String -> Value -> Memory -> Memory
define name value memory = memory
  { env = Map.insert    name       memory.ptr memory.env
  , mem = IntMap.insert memory.ptr value      memory.mem
  , ptr = memory.ptr + 1
  }

-- Реализация `set!`.
--
set :: String -> Value -> Memory -> Either Error Memory
set name value memory =
  case Map.lookup name memory.env of
    Nothing      -> throwError $ NoSuchBinding name
    Just address -> return memory
      { mem = IntMap.insert address value memory.mem
      }

-- Пустая память.
--
tabulaRasa :: Memory
tabulaRasa = Memory mempty mempty 0

-- Стандартная библиотека.
--
stdlib :: Memory
stdlib = tabulaRasa
  & define "list"    listF
  & define "quote"   quoteSF
  & define "def"     defSF
  & define "set!"    setSF
  & define "eval-in" evalSF
  & define "print"   printF
  & define "do"      doF
  & define "cond"    condSF
  & define ">="      geqF
  & define "<="      leqF
  & define "="       eqF
  & define ">"       gtF
  & define "+"       plusF
  & define "/"       divF
  & define "-"       minusF
  & define "mod"     modF
  & define "++"      appendF
  & define "lambda"  lambdaSF
  & define "macro"   macroSF
  & define "read"    readF
  & define "get-env" getEnvSF
  & define "exit"    exitF
