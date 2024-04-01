{-# LANGUAGE RecursiveDo #-}

module LVM where

import Control.Monad
import Data.Map qualified as Map
import Data.Traversable

import Polysemy
import Polysemy.State
import Polysemy.Reader
import Polysemy.Error
import Polysemy.Fixpoint

data LVM
  = Var Name

  | Lam [Name] LVM
  | App LVM [LVM]

  | Let [(Name, LVM)] LVM

  | Sym String [LVM]
  | Match LVM (Map.Map Name ([Name], LVM))

  | BIF String
  | Num Double
  | Str String
  deriving stock (Show)

data Alt = Alt Name [Name] LVM
  deriving stock (Show)

type Name = String
type Addr = Int

data Thunk
  = Forced Value
  | Delayed (Map.Map Name Addr) LVM
  | BLACKHOLE
  deriving stock (Show)

data Value
  = Closure (Map.Map Name Addr) [Name] LVM
  | Symbol  String [Addr]
  | Number  Double
  | Text    String
  | Builtin String
  deriving stock (Show)

data Err
  = TypeMismatch {expected :: String, gotValue :: Value}
  | NoAlt {name :: String, gotValue :: Value}
  | NotAStructure {gotValue :: Value}
  | LOOP
  deriving stock (Show)

type VM m = Members
  '[ State  (Map.Map Addr Thunk, Addr)
   , Reader (Map.Map Name Addr)
   , Error   Err
   , Fixpoint
   , Final IO
   ] m

eval :: VM m => Addr -> LVM -> Sem m Addr
eval back = \case
  Var var -> do
    asks @(Map.Map Name Addr) (Map.lookup var) >>= \case
      Nothing -> error $ "undefined " <> show var
      Just addr -> pure addr

  Lam args body -> do
    env <- ask
    alloc $ Forced $ Closure env args body

  App f xs -> do
    xs <- traverse delay xs
    whnf f >>= \case
      Closure env args body -> do
        local (const (Map.fromList (zip args xs) <> env)) do
          eval body

      Builtin name -> do
        error "builtin"

      other -> do
        throw TypeMismatch
          { expected = "function/" <> show (length xs)
          , gotValue = other
          }

  Let fs k -> do
    env <- ask @(Map.Map Name Addr)
    rec
      addrs <- for fs \(name, f) -> do
        addr <- alloc $ Delayed new f
        return (name, addr)

      let new = Map.fromList addrs <> env

    local (const new) do
      eval k

  Sym name xs -> do
    xs <- traverse delay xs
    alloc $ Forced $ Symbol name xs

  Match subj alts -> do
    whnf subj >>= \case
      value@(Symbol name xs) -> do
        case Map.lookup name alts of
          Nothing -> throw NoAlt {name, gotValue = value}
          Just (args, body) -> do
            local (Map.fromList (zip args xs) <>) do
              eval body
      value -> do
        throw NotAStructure {gotValue = value}

  BIF s -> alloc $ Forced $ Builtin s
  Num s -> alloc $ Forced $ Number s
  Str s -> alloc $ Forced $ Text s

alloc :: VM m => Thunk -> Sem m Addr
alloc thunk = do
  (mem, hp) <- get
  put (Map.insert hp thunk mem, hp + 1)
  return hp

force :: VM m => Addr -> Sem m Value
force addr = do
  gets @(Map.Map Addr Thunk, Addr) ((Map.! addr) . fst) >>= \case
    Forced val -> pure val
    Delayed env lvm -> do
      local (const env) do
        whnf lvm

whnf :: VM m => LVM -> Sem m Value
whnf lvm = eval lvm >>= force

delay :: VM m => LVM -> Sem m Addr
delay lvm = do
  env <- ask
  alloc $ Delayed env lvm

runVM ::
  Sem
    ( State  (Map.Map Addr Thunk, Addr)
    : Reader (Map.Map Name Addr)
    : Error   Err
    : Fixpoint
    : Final IO
    : '[]
    ) a
  -> IO (Either Err a)
runVM
  = runFinal
  . fixpointToFinal @IO
  . runError
  . runReader mempty
  . evalState (mempty, 0)