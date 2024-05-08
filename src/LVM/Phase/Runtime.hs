
module LVM.Phase.Runtime where

import Data.Map.Strict qualified as Map
import Data.Fix

import LVM.Phase.Raw
import LVM.Name
import Input

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

data Thunk
  = Ready      Value
  | Delayed   (Map.Map Name Addr) Prog
  | BLACKHOLE  Position
  deriving stock (Show)

data Value_ self
  = Closure (Map.Map Name Addr) Name Prog
  | Symbol   String self
  | Record  (Map.Map String self)
  | Number   Double
  | Text     String
  | Builtin  String Int [self]
  deriving stock (Functor, Foldable, Traversable)

type Value = (Position, Value_ Int)
type CutValue = Fix (Compose Maybe (Compose ((,) Position) Value_))

data Err
  = TypeMismatch             {expected :: String, gotValue :: CutValue}
  | NotASymbol               {subj :: CutValue}
  | NotARecord               {subj :: CutValue}
  -- | NoSuchCtor               {ctor  :: String, ctors  :: [Name]}
  | NoSuchField              {field :: String, fields :: [String]}
  | NoSuchBuiltin            {bif :: String}
  | NoCaseFor                {gotValue :: CutValue}
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
      :: Position
      -> String
      -> [CutValue]
      -> ExceptT Report IO CutValue
  }

data Env = Env
  { bindings :: Map.Map Name Addr
  , ffi      :: FFI
  }

type VM =
    StateT  Machine
  ( ReaderT Env
  ( ExceptT Report
    IO
  ) )

runVM
  :: FFI
  -> VM a
  -> IO (Either Report a)
runVM dispatch
  = runExceptT
  . flip runReaderT Env {bindings = mempty, ffi = dispatch}
  . flip evalStateT Machine { memory = mempty, hp = 0 }

instance Show self => Show (Value_ self) where
  show = \case
    Closure _ arg prog -> "\\" <> show arg <> " => " <> show prog
    Symbol ctor arg -> "%" <> show ctor <> " " <> show arg
    Record fs -> "{" <> concat (map ((<> ",") . (\(n, f) -> show n <> "=" <> show f)) (Map.toList fs)) <> "}"
    Number n -> show n
    Text   t -> show t
    Builtin n ix args -> "$" <> show n <> "/" <> show ix <> show args
