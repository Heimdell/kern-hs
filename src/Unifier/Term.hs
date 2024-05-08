
module Unifier.Term where

import Control.Monad

data Term t v
  = Pure v
  | Term (t (Term t v))
  deriving stock (Functor, Foldable, Traversable)

occurs :: (Eq v, Foldable t) => v -> Term t v -> Bool
occurs = any . (==)

instance (Show (t (Term t v)), Show v) => Show (Term t v) where
  show = \case
    Pure v -> show v
    Term t -> show t

instance (Functor t) => Monad (Term t) where
  ma >>= k = case ma of
    Pure a -> k a
    Term t -> Term (fmap (>>= k) t)

instance (Functor t) => Applicative (Term t) where
  pure = Pure
  (<*>) = ap