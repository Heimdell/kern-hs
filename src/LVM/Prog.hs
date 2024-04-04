
module LVM.Prog where

import Data.Map qualified as Map

import LVM.Name

data Prog_ self
  = Var Name

  | Lam [Name] self
  | App self [self]

  | Let [(Name, self)] self

  | Sym Name self
  | Match self (Map.Map Name (Name, self))

  | Rec [(Name, self)]
  | Get self Name

  | BIF String
  | Num Double
  | Str String

  | Do [Stmt self] self
  deriving stock (Show, Functor, Foldable, Traversable)

data Stmt prog
  = Force prog
  | Def [(Name, prog)]
  deriving stock (Show, Functor, Foldable, Traversable)
