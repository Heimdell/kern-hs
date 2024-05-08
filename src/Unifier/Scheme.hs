
module Unifier.Scheme where

import GHC.Generics
import Data.Foldable
import Data.Functor.Compose
import Data.Map qualified as Map
import Data.Traversable
import Data.List
import Unifier.Term
import Unifier.Matchable
import Unifier.Rename

data Scheme t v = Scheme
  { vars :: [v]
  , body :: t v
  }
  deriving stock (Functor, Foldable, Traversable, Generic1)
  deriving anyclass (Matchable)

instance (Show (t v), Show v) => Show (Scheme t v) where
  show scheme
    | null scheme.vars = show scheme.body
    | otherwise        = "forall " <> show scheme.vars <> " " <> show scheme.body

generalise :: (Foldable t, Ord v) => t v -> Scheme t v
generalise t = Scheme (nub (toList t)) t

instantiate
  :: (Traversable t, Ord v, MonadRename v m)
  => Scheme t v
  -> m (t v)
instantiate (Scheme vars body) = do
  rename <- for vars \var -> do
    new <- rename var
    return (var, new)
  let mapper = Map.fromList rename
  return (fmap (mapper Map.!) body)

instantiateWith
  :: (Traversable t, Ord v, MonadRename v m, Functor f)
  => [Term t v]
  -> Scheme (Compose f (Term t)) v
  -> m (f (Term t v))
instantiateWith args (Scheme vars (Compose body)) = do
  let mapper = Map.fromList (zip vars args)
  return $ fmap (>>= (mapper Map.!)) body