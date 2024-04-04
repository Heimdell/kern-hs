
module LVM.Phase.Raw where

import Data.Fix

import LVM.Prog
import Input

type Prog = Tree Prog_ Position

type Addr = Int
