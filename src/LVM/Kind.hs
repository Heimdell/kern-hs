
module LVM.Kind where

import GHC.Generics
import Unifier.Matchable
import Unifier.Term
import LVM.Name

data Kind_ self
  = Star_
  | KArrow_ self self
  deriving stock (Functor, Foldable, Traversable, Generic1)
  deriving anyclass (Matchable)

type Kind = Term Kind_ Name

instance (Show k) => Show (Kind_ k) where
  show = \case
    Star_ -> "*"
    KArrow_ d c -> "(" <> show d <> " => " <> show c <> ")"