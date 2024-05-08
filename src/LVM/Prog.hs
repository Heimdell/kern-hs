
module LVM.Prog where

import Data.Map qualified as Map

import LVM.Name
import LVM.Type
import LVM.Kind

data Prog_ self
  = Var Name

  | Lam Name self
  | App self self

  | Sym String self
  | Match self [Alt self]

  | Rec [(String, self)]
  | Get self String

  | BIF String Type
  | Num Double
  | Str String

  | Ann self Type

  | Do [Stmt self] self
  deriving stock (Functor, Foldable, Traversable)

data Alt prog = Alt
  { pat  :: Pattern
  , body :: prog
  }
  deriving stock (Functor, Foldable, Traversable)

data Pattern
  = PSym   String Pattern
  | PRec [(String, Pattern)]
  | PNum   Double
  | PStr   String
  | PVar   Name

data Stmt prog
  = ValSig   Name Type
  | ValDecl  Name prog
  | TypeDecl Name TypeExpr
  | TypeSig  Name Kind
  deriving stock (Show, Functor, Foldable, Traversable)

instance Show self => Show (Alt self) where
  show (Alt pat b) = show pat <> " => " <> show b

instance Show Pattern where
  show = \case
    PSym c p -> show c <> " " <> show p
    PRec fs  -> concat (map ((<> ",") . (\(n, f) -> show n <> "=" <> show f)) fs)
    PNum n   -> show n
    PStr n   -> show n
    PVar n   -> show n

instance Show self => Show (Prog_ self) where
  show = \case
    Var n -> show n

    Lam n b -> "\\" <> show n <> " => " <> show b
    App f x -> "(" <> show f <> " " <> show x <> ")"

    Sym n a -> "%" <> show n <> " " <> show a
    Match s as -> "match " <> show s <> " with {" <> concat (map ((<> ",") . show) as) <> "}"

    Rec fs -> "{" <> concat (map ((<> ",") . (\(n, f) -> show n <> "=" <> show f)) fs) <> "}"
    Get p n -> show p <> "." <> show n

    BIF n i -> "$" <> show n <> "/" <> show i

    Num d -> show d
    Str s -> show s

    Do ss r -> "do " <> concatMap show ss <> " return " <> show r