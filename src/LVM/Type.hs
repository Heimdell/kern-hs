
module LVM.Type where

import Data.Map qualified as Map
import Data.List
import Data.Functor.Compose
import GHC.Generics
import Unifier.Matchable
import Unifier.Term
import Unifier.Scheme
import LVM.Name

data Type_ self
  = TArrow_ self self
  | TApply_ self self
  | TConst_ Name
  deriving stock (Functor, Foldable, Traversable, Generic1)
  deriving anyclass (Matchable)

type Type = Term Type_ Name

data TypeExpr_ ty
  = Union  { ctors :: Map.Map String ty }
  | Record { ctors :: Map.Map String ty }
  deriving stock (Functor, Foldable, Traversable)

type TypeExpr  = Scheme (Compose TypeExpr_ (Term Type_)) Name
type TypeExpr' =         TypeExpr_ Type

instance (Show ty) => Show (Type_ ty) where
  show = \case
    TApply_ f x -> "(" <> show f <> " "    <> show x <> ")"
    TArrow_ d c -> "(" <> show d <> " -> " <> show c <> ")"
    TConst_ n   -> show n

instance (Show ty) => Show (TypeExpr_ ty) where
  show = \case
    Union fs -> "union " <> showMap fs
    Record fs -> "record " <> showMap fs

showMap :: (Show k, Show v) => Map.Map k v -> String
showMap m = "{" <> intercalate ", " (map (\(k, v) -> show k <> " = " <> show v) (Map.toList m)) <> "}"