
module LVM.Prog where

import Data.Map qualified as Map

import LVM.Name

data Prog_ self
  = Var Name

  | Lam Name self
  | App self self

  | Sym Name self
  | Match self (Map.Map Name (Name, self))

  | Rec [(Name, self)]
  | Get self Name

  | BIF String Int
  | Num Double
  | Str String

  | Do [Stmt self] self
  deriving stock (Functor, Foldable, Traversable)

data Stmt prog
  = Force prog
  | Let [(Name, prog)]
  deriving stock (Show, Functor, Foldable, Traversable)

instance Show self => Show (Prog_ self) where
  show = \case
    Var n -> show n

    Lam n b -> "\\" <> show n <> " => " <> show b
    App f x -> show f <> " " <> show x

    Sym n a -> "%" <> show n <> " " <> show a
    Match s as -> "match " <> show s <> " with {" <> concat (map ((<> ",") . (\(n, (a, f)) -> show n <> " " <> show a <> " => " <> show f)) (Map.toList as)) <> "}"

    Rec fs -> "{" <> concat (map ((<> ",") . (\(n, f) -> show n <> "=" <> show f)) fs) <> "}"
    Get p n -> show p <> "." <> show n

    BIF n i -> "$" <> show n <> "/" <> show i

    Num d -> show d
    Str s -> show s

    Do ss r -> "do " <> concatMap show ss <> " return " <> show r