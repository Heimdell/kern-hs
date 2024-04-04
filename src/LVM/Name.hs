
module LVM.Name where

import Control.Arrow
import Data.Function

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