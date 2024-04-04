{-# LANGUAGE RecursiveDo #-}

module LVM.Pass.Run where

import Control.Monad
import Data.Map qualified as Map
import Data.Fix
import Data.Traversable

import LVM.Phase.Raw
import LVM.Phase.Runtime
import LVM.Prog
import LVM.Name
import Input

import Polysemy
import Polysemy.State hiding (Get)
import Polysemy.Reader
import Polysemy.Error

retrieve :: VM m => Addr -> Sem m Thunk
retrieve addr = gets @Machine ((Map.! addr) . (.memory))

store :: VM m => Addr -> Thunk -> Sem m ()
store addr thunk = modify @Machine \m -> m { memory = Map.insert addr thunk m.memory}

addressOf :: VM m => Name -> Sem m Addr
addressOf var = do
  asks @(Map.Map Name Addr) (Map.lookup var) >>= \case
    Nothing   -> throw (var.pos, Undefined var)
    Just addr -> return addr

force :: VM m => Addr -> Sem m Value
force addr = do
  thunk <- retrieve addr
  case thunk of
    BLACKHOLE pos -> throw (pos, LOOP)
    Ready val -> return val
    Delayed env prog@(pos :> _) -> do
      store addr $ BLACKHOLE pos
      val <- local (const env) do
        eval prog
      store addr (Ready val)
      return val

eval :: VM m => Prog -> Sem m Value
eval = \case
  _ :> Var name -> do
    addr <- addressOf name
    force addr

  pos :> Lam args body -> do
    env <- ask
    return (pos, Closure env args body)

  pos :> App f xs0 -> do
    xs <- traverse delay xs0
    eval f >>= \case
      (_, Closure env args body) -> do
        local (const (Map.fromList (zip args xs) <> env)) do
          eval body

      (_, Builtin bif) -> do
        FFI ffi <- ask @FFI
        unroll =<< ffi pos bif =<< traverse (roll <=< force) xs

      other0 -> do
        other <- extract other0
        throw (pos, TypeMismatch
          { expected = "function/" <> show (length xs)
          , gotValue = other
          })

  _ :> Let decls body -> do
    rec
      decls' <- for decls \(name, prog) -> do
        addr <- local (Map.fromList decls' <>) do
          delay prog
        return (name, addr)

    local (Map.fromList decls' <>) do
      eval body

  pos :> Sym ctor x0 -> do
    x <- delay x0
    return (pos, Symbol ctor x)

  pos :> Match subj alts -> do
    eval subj >>= \case
      (_, Symbol ctor vals) -> do
        match pos ctor vals alts

      subj1 -> do
        subj2 <- extract subj1
        throw (pos, NotASymbol {subj = subj2})

  pos :> Rec fields0 -> do
    fields <- for fields0 \(field, value0) -> do
      value <- delay value0
      return (field, value)
    return (pos, Record (Map.fromList fields))

  pos :> Get subj field -> do
    eval subj >>= \case
      (_, Record fields) -> do
        case Map.lookup field fields of
          Nothing -> throw (pos, NoSuchField {field, fields = Map.keys fields})
          Just addr -> force addr

      other0 -> do
        other <- extract other0
        throw (pos, NotARecord {subj = other})

  pos :> BIF name -> return (pos, Builtin name)
  pos :> Num n -> return (pos, Number n)
  pos :> Str n -> return (pos, Text n)

  _ :> Do stmts res -> do
    withStmts stmts do
      eval res

withStmts :: VM m => [Stmt Prog] -> Sem m a -> Sem m a
withStmts [] ma = ma
withStmts (stmt : stmts) ma = do
  withStmt stmt \_ -> do
    withStmts stmts ma

withStmt :: VM m => Stmt Prog -> (Maybe Value -> Sem m a) -> Sem m a
withStmt stmt ma = do
  case stmt of
    Def decls -> do
      rec
        decls' <- for decls \(name, prog) -> do
          addr <- local (Map.fromList decls' <>) do
            delay prog
          return (name, addr)

      local (Map.fromList decls' <>) do
        ma Nothing

    Force prog -> do
      value <- eval prog
      ma (Just value)

delay :: VM m => Prog -> (Sem m) Addr
delay prog = do
  env <- ask
  alloc $ Delayed env prog

alloc :: VM m => Thunk -> Sem m Addr
alloc thunk = do
  addr <- gets @Machine (.hp)
  modify @Machine \m -> m
    { memory = Map.insert addr thunk m.memory
    , hp     = m.hp + 1
    }
  return addr

match :: VM m => Position -> Name -> Addr -> Map.Map Name (Name, Prog) -> Sem m Value
match pos ctor x alts = do
  case Map.lookup ctor alts of
    Nothing -> throw (pos, NoSuchCtor {ctor, ctors = Map.keys alts})
    Just (arg, body) -> do
      local (Map.insert arg x) do
        eval body

roll :: VM m => Value -> Sem m (CutValue)
roll = \case
  (pos, Closure {}) -> throw (pos, Can'tPassFunctionsToBIFs)
  (pos, Builtin {}) -> throw (pos, Can'tPassFunctionsToBIFs)
  (pos, Number  n)  -> return $ pos :>? Number n
  (pos, Text    n)  -> return $ pos :>? Text n
  (pos, Symbol ctor arg0) -> do
    arg <- force >=> roll $ arg0
    return $ pos :>? Symbol ctor arg

  (pos, Record fields0) -> do
    fields <- traverse (force >=> roll) fields0
    return $ pos :>? Record fields

unroll :: VM m => CutValue -> Sem m Value
unroll (pos :>? val) = case val of
  Closure {} -> throw (pos, Can'tPassFunctionsToBIFs)
  Builtin {} -> throw (pos, Can'tPassFunctionsToBIFs)
  Number  n  -> return (pos, Number n)
  Text    n  -> return (pos, Text n)
  Symbol ctor arg0 -> do
    arg <- alloc . Ready <=< unroll $ arg0
    return (pos, Symbol ctor arg)

  Record fields0 -> do
    fields <- traverse (alloc . Ready <=< unroll) fields0
    return (pos, Record fields)

unroll None = do
  error "builtin have returned malformed value"

extract :: VM m => Value -> Sem m CutValue
extract (pos, val) = case val of
  Closure env args prog -> return $ pos :>? Closure env args prog
  Builtin name          -> return $ pos :>? Builtin name
  Number  n             -> return $ pos :>? Number  n
  Text    s             -> return $ pos :>? Text    s
  Symbol ctor arg0 -> do
    arg <- extractArg arg0
    return $ pos :>? Symbol ctor arg

  Record fields0 -> do
    fields <- traverse extractArg fields0
    return $ pos :>? Record fields

  where
    extractArg addr = do
      retrieve addr >>= \case
        Ready val1 -> extract val1
        _          -> return None

class Interop a where
  to   ::                            Position -> a        -> CutValue
  from :: Member (Error Report) m =>        CutValue -> Sem m a

instance Interop Double where
  to pos n = pos :>? Number n
  from = \case
    _ :>? Number n    -> return n
    other@(pos :>? _) -> throw (pos, TypeMismatch {expected = "number", gotValue = other})
    None              -> throw (nowhere, TypeMismatch {expected = "number", gotValue = None})

dispatch :: FFI
dispatch = FFI \pos -> \case
  "+" -> \xs0 -> do
    xs <- traverse (from @Double) xs0
    return $ to pos $ sum xs

  "*" -> \xs0 -> do
    xs <- traverse (from @Double) xs0
    return $ to pos $ product xs

  "<" -> \[x, y] -> do
    x <- from @Double x
    y <- from @Double y
    return if x < y
      then pos :>? Symbol Name {raw = "True", pos = pos, index = 0} (pos :>? Record mempty)
      else pos :>? Symbol Name {raw = "False", pos = pos, index = 0} (pos :>? Record mempty)

  other -> \_ -> do
    throw (pos, NoSuchBuiltin other)

nowhere :: Position
nowhere = Position
  { line     = 1
  , column   = 1
  , filename = ""
  , offset   = 0
  }

test :: IO ()
test = do
  res <- runVM dispatch do
    eval (nowhere :> App (nowhere :> BIF "*") [nowhere :> Num 3, nowhere :> Num 4])

  print res