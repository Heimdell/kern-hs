
module LVM.Name where

import Control.Arrow
import Control.Monad.State
import Data.Function
import Data.String
import Unifier.Rename

import Input

data Name = Name
  { pos   :: Position
  , raw   :: String
  , index :: Int
  }

instance Eq  Name where (==)    = (==)    `on` ((.raw) &&& (.index))
instance Ord Name where compare = compare `on` ((.raw) &&& (.index))

instance Show Name where
  show name = name.raw

instance (Monad m) => MonadRename Name (RenameT Name m) where
  rename name = RenameT do
    modify (+ 1)
    index <- get
    return name {index}

instance IsString Name where
  fromString raw = Name {index = 0, raw, pos = nowhere}