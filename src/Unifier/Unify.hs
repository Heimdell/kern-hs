
module Unifier.Unify where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map qualified as Map
import Data.Foldable
import Unifier.Matchable
import Unifier.Term

data UError t v
  = Cyclic {name :: v, term :: Term t v}
  | Mismatch {expected, got :: Term t v}

data UState t v = UState
  { bindings :: Map.Map v (Term t v)
  }

type Unify t v m =
  ( MonadState (UState t v) m
  , MonadError (UError t v) m
  , Ord v
  , Matchable t
  )

assign :: Unify t v m => v -> Term t v -> m ()
assign v t = do
  modify \s -> s { bindings = Map.insert v t s.bindings }

findVar :: Unify t v m => v -> m (Maybe (Term t v))
findVar v = gets (Map.lookup v . (.bindings))

(=:) :: Unify t v m => v -> Term t v -> m ()
name =: term = do
  when (occurs name term) do
    throwError Cyclic {name, term}

  assign name term

apply :: Unify t v m => Term t v -> m (Term t v)
apply = \case
  Pure v -> findVar v >>= \case
    Nothing -> return (Pure v)
    Just t -> apply t

  Term t -> Term <$> traverse apply t

semiprune :: Unify t v m => Term t v -> m (Term t v)
semiprune = \case
  Term t -> return (Term t)
  Pure v1 -> do
    findVar v1 >>= \case
      Nothing -> return (Pure v1)
      Just (Pure v2) -> do
        res <- semiprune (Pure v2)
        assign v1 res
        return res
      Just term -> do
        return term

(=:=) :: Unify t v m => Term t v -> Term t v -> m ()
left =:= right = do
  expected <- semiprune left
  got      <- semiprune right

  case (expected, got) of
    (Pure a, Pure b) -> do
      unless (a == b) do
        assign a (Pure b)

    (Pure a, b) -> a =: b
    (a, Pure b) -> b =: a
    (Term s, Term t) -> do
      case match s t of
        Nothing -> throwError Mismatch {expected, got}
        Just u -> do
          for_ u (uncurry (=:=))