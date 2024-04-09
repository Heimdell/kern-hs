{-# LANGUAGE RecursiveDo #-}

module LVM.Pass.Run where

import Control.Applicative
import Control.Monad
import Data.Map qualified as Map
import Data.Fix
import Data.Traversable

import LVM.Phase.Raw
import LVM.Phase.Runtime
import LVM.Prog
import LVM.Name
import Input

import Effectful
import Effectful.State.Static.Local hiding (Get)
import Effectful.Reader.Static
import Effectful.Error.Static
import Effectful.NonDet

import Debug.Trace

retrieve :: VM m => Addr -> Eff m Thunk
retrieve addr = gets @Machine ((Map.! addr) . (.memory))

store :: VM m => Addr -> Thunk -> Eff m ()
store addr thunk = modify @Machine \m -> m { memory = Map.insert addr thunk m.memory}

addressOf :: VM m => Name -> Eff m Addr
addressOf var = do
  asks @(Map.Map Name Addr) (Map.lookup var) >>= \case
    Nothing   -> throwError (var.pos, Undefined var)
    Just addr -> return addr

force :: VM m => Addr -> Eff m Value
force addr = do
  thunk <- retrieve addr
  case thunk of
    BLACKHOLE pos -> throwError (pos, LOOP)
    Ready val -> return val
    Delayed env prog@(pos :> _) -> do
      store addr $ BLACKHOLE pos
      val <- local (const env) do
        eval prog
      store addr (Ready val)
      return val

eval :: VM m => Prog -> Eff m Value
eval = \case
  _ :> Var name -> do
    addr <- addressOf name
    force addr

  pos :> Lam arg body -> do
    env <- ask
    return (pos, Closure env arg body)

  pos :> App f xs0 -> do
    xs <- delay xs0
    eval f >>= \case
      (_, Closure env arg body) -> do
        local (const (Map.insert arg xs env)) do
          eval body

      (pos1, Builtin bif args stack) -> do
        case args of
          1 -> do
            FFI ffi <- ask @FFI
            unroll =<< ffi pos bif =<< traverse (roll <=< force) (reverse (xs : stack))

          n -> do
            return (pos1, Builtin bif (n - 1) (xs : stack))

      other0 -> do
        other <- extract other0
        throwError (pos, TypeMismatch
          { expected = "function"
          , gotValue = other
          })

  pos :> Sym ctor x0 -> do
    x <- delay x0
    return (pos, Symbol ctor x)

  pos :> Match subj alts -> do
    subj' <- delay subj
    match pos subj' alts

  pos :> Rec fields0 -> do
    fields <- for fields0 \(field, value0) -> do
      value <- delay value0
      return (field, value)
    return (pos, Record (Map.fromList fields))

  pos :> Get subj field -> do
    eval subj >>= \case
      (_, Record fields) -> do
        case Map.lookup field fields of
          Nothing -> throwError (pos, NoSuchField {field, fields = Map.keys fields})
          Just addr -> force addr

      other0 -> do
        other <- extract other0
        throwError (pos, NotARecord {subj = other})

  pos :> BIF name index -> return (pos, Builtin name index [])
  pos :> Num n -> return (pos, Number n)
  pos :> Str n -> return (pos, Text n)

  _ :> Do stmts res -> do
    withStmts stmts do
      eval res

withStmts :: VM m => [Stmt Prog] -> Eff m a -> Eff m a
withStmts [] ma = ma
withStmts (stmt : stmts) ma = do
  withStmt stmt \_ -> do
    withStmts stmts ma

withStmt :: VM m => Stmt Prog -> (Maybe Value -> Eff m a) -> Eff m a
withStmt stmt ma = do
  case stmt of
    Let decls -> do
      env <- ask
      decls' <- for decls \(name, _) -> do
        addr <- alloc $ BLACKHOLE nowhere
        return (name, addr)

      let env' = Map.fromList decls' <> env

      for (zip decls decls') \((name, prog), (_, addr)) -> do
        store addr $ Delayed env' prog

      local (const env') do
        ma Nothing

    Force prog -> do
      value <- eval prog
      ma (Just value)

delay :: VM m => Prog -> (Eff m) Addr
delay prog = do
  env <- ask
  alloc $ Delayed env prog

alloc :: VM m => Thunk -> Eff m Addr
alloc thunk = do
  addr <- gets @Machine (.hp)
  modify @Machine \m -> m
    { memory = Map.insert addr thunk m.memory
    , hp     = m.hp + 1
    }
  return addr

match :: forall m. VM m => Position -> Addr -> [Alt Prog] -> Eff m Value
match pos value alts = do
  runNonDet OnEmptyKeep (scan value alts <|> complain pos value) >>= \case
    Left _ -> do
      throwError (pos, NoCaseFor (nowhere :>? Record mempty))

    Right res -> do
      return res
  where
    complain :: Position -> Addr -> Eff (NonDet : m) Value
    complain pos addr = do
      value0 <- force addr
      value <- extract value0
      throwError (pos, NoCaseFor value)

    scan :: Addr -> [Alt Prog] -> Eff (NonDet : m) Value
    scan val []           = empty
    scan val (alt : alts) = scanOne val alt <|> scan val alts

    scanOne :: Addr -> Alt Prog -> Eff (NonDet : m) Value
    scanOne val (Alt pat body) = do
      withPattern (val, pat) do
        eval body

withPattern :: (VM m, NonDet :> m) => (Addr, Pattern) -> Eff m a -> Eff m a
withPattern (val, pat) k = do
  case pat of
    PSym ctor' b ->
      force val >>= \case
        (pos, Symbol ctor p) | ctor == ctor' ->
          withPattern (p, b) k

        _ -> empty

    PRec fs -> do
      force val >>= \case
        (pos, Record fs') | Just subtasks <- matchFields fs' fs ->
          withMany withPattern subtasks do
            k

        _ -> empty

    PNum n -> do
      force val >>= \case
        (pos, Number m) | n == m -> do
          k

        _ -> empty

    PStr n -> do
      force val >>= \case
        (pos, Text m) | n == m -> do
          k

        _ -> empty

    PVar n -> do
      local (Map.insert n val) do
        k

matchFields :: Map.Map String Addr -> [(String, Pattern)] -> Maybe [(Addr, Pattern)]
matchFields fs tasks = do
  for tasks \(name, pat)  -> do
    addr <- Map.lookup name fs
    return (addr, pat)

withMany :: (a -> b -> b) -> [a] -> b -> b
withMany f [] k = k
withMany f (x : xs) k = do
  f x do
    withMany f xs k

roll :: VM m => Value -> Eff m (CutValue)
roll = \case
  (pos, Closure {}) -> throwError (pos, Can'tPassFunctionsToBIFs)
  (pos, Builtin {}) -> throwError (pos, Can'tPassFunctionsToBIFs)
  (pos, Number  n)  -> return $ pos :>? Number n
  (pos, Text    n)  -> return $ pos :>? Text n
  (pos, Symbol ctor arg0) -> do
    arg <- force >=> roll $ arg0
    return $ pos :>? Symbol ctor arg

  (pos, Record fields0) -> do
    fields <- traverse (force >=> roll) fields0
    return $ pos :>? Record fields

unroll :: VM m => CutValue -> Eff m Value
unroll (pos :>? val) = case val of
  Closure {} -> throwError (pos, Can'tPassFunctionsToBIFs)
  Builtin {} -> throwError (pos, Can'tPassFunctionsToBIFs)
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

extract :: VM m => Value -> Eff m CutValue
extract (pos, val) = case val of
  Closure env args prog -> return $ pos :>? Closure env args prog
  Number  n             -> return $ pos :>? Number  n
  Text    s             -> return $ pos :>? Text    s

  Builtin name ix stack0 -> do
    stack <- traverse extractArg stack0
    return $ pos :>? Builtin name ix stack

  Symbol ctor arg0 -> do
    arg <- extractArg arg0
    return $ pos :>? Symbol ctor arg

  Record fields0 -> do
    fields <- traverse extractArg fields0
    return $ pos :>? Record fields

extractArg addr = do
  retrieve addr >>= \case
    Ready val1 -> extract val1
    _          -> return None

class Interop a where
  to   ::                            Position -> a        -> CutValue
  from :: Error Report :> m =>        CutValue -> Eff m a

instance Interop Double where
  to pos n = pos :>? Number n
  from = \case
    _ :>? Number n    -> return n
    other@(pos :>? _) -> throwError (pos, TypeMismatch {expected = "number", gotValue = other})
    None              -> throwError (nowhere, TypeMismatch {expected = "number", gotValue = None})

dispatch :: FFI
dispatch = FFI \pos -> \case
  "+" -> \xs0 -> do
    xs <- traverse (from @Double) xs0
    return $ to pos $ sum xs

  "-" -> \[x, y] -> do
    x <- from @Double x
    y <- from @Double y
    return $ to pos $ x - y

  "*" -> \xs0 -> do
    xs <- traverse (from @Double) xs0
    return $ to pos $ product xs

  "<" -> \[x, y] -> do
    x <- from @Double x
    y <- from @Double y
    return if x < y
      then pos :>? Symbol "True" (pos :>? Record mempty)
      else pos :>? Symbol "False" (pos :>? Record mempty)

  other -> \_ -> do
    throwError (pos, NoSuchBuiltin other)

nowhere :: Position
nowhere = Position
  { line     = 1
  , column   = 1
  , filename = ""
  , offset   = 0
  }
