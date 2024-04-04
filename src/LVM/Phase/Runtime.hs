
module LVM.Phase.Runtime where

import Data.Map qualified as Map
import Data.Fix

import LVM.Phase.Raw
import LVM.Name
import Input

import Polysemy
import Polysemy.State
import Polysemy.Reader
import Polysemy.Error
import Polysemy.Fixpoint

data Thunk
  = Ready      Value
  | Delayed   (Map.Map Name Addr) Prog
  | BLACKHOLE  Position
  deriving stock (Show)

data Value_ self
  = Closure (Map.Map Name Addr) [Name] Prog
  | Symbol   Name self
  | Record  (Map.Map Name self)
  | Number   Double
  | Text     String
  | Builtin  String
  deriving stock (Show, Functor, Foldable, Traversable)

type Value = (Position, Value_ Int)
type CutValue = Fix (Compose Maybe (Compose ((,) Position) Value_))

data Err
  = TypeMismatch             {expected :: String, gotValue :: CutValue}
  | NotASymbol               {subj :: CutValue}
  | NotARecord               {subj :: CutValue}
  | NoSuchCtor               {ctor  :: Name, ctors  :: [Name]}
  | NoSuchField              {field :: Name, fields :: [Name]}
  | NoSuchBuiltin            {bif :: String}
  | Can'tPassFunctionsToBIFs
  | LOOP
  | Undefined                {name :: Name}
  deriving stock (Show)

type Report = (Position, Err)

data Machine = Machine
  { memory :: Map.Map Addr Thunk
  , hp     :: Addr
  }

data FFI = FFI
  { run
      :: forall n
      .  Members '[Error Report, Embed IO] n
      => Position
      -> String
      -> [CutValue]
      -> Sem n (CutValue)
  }

type VM m = Members
  '[ State  Machine
   , Reader (Map.Map Name Addr)
   , Reader  FFI
   , Error   Report
   , Fixpoint
   , Embed IO
   ] m

runVM
  :: FFI
  -> Sem
    [ State   Machine
    , Reader (Map.Map Name Addr)
    , Reader  FFI
    , Error   Report
    , Fixpoint
    , Embed IO
    , Final IO
    ] a
  -> IO (Either Report a)
runVM dispatch
  = runFinal
  . embedToFinal
  . fixpointToFinal @IO
  . runError
  . runReader dispatch
  . runReader mempty
  . evalState Machine { memory = mempty, hp = 0 }
