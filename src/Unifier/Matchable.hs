
{-# LANGUAGE DefaultSignatures #-}

module Unifier.Matchable where

import GHC.Generics

class (Traversable t) => Matchable t where
  match :: t a -> t a -> Maybe (t (a, a))

  default match
    :: (Matchable (Rep1 t), Generic1 t) => t a -> t a -> Maybe (t (a, a))
  match l r = to1 <$> match (from1 l) (from1 r)

instance Matchable []

instance Matchable V1 where
  match = \case

instance Matchable U1 where
  match U1 U1 = Just U1

instance Matchable Par1 where
  match (Par1 a) (Par1 b) = return $ Par1 (a, b)

instance (Matchable f) => Matchable (Rec1 f) where
  match (Rec1 a) (Rec1 b) = Rec1 <$> match a b

instance (Eq c) => Matchable (K1 i c) where
  match (K1 a) (K1 b)
    | a == b    = return (K1 a)
    | otherwise = Nothing

instance (Matchable f) => Matchable (M1 i c f) where
  match (M1 a) (M1 b) = M1 <$> match a b

instance (Matchable f, Matchable g) => Matchable (f :+: g) where
  match a b = case (a, b) of
    (L1 q, L1 w) -> L1 <$> match q w
    (R1 q, R1 w) -> R1 <$> match q w
    _            -> Nothing

instance (Matchable f, Matchable g) => Matchable (f :*: g) where
  match (a :*: c) (b :*: d) = pure (:*:) <*> match a b <*> match c d

instance (Matchable f, Matchable g) => Matchable (f :.: g) where
  match (Comp1 a) (Comp1 b) = do
    Comp1 <$> (traverse (uncurry match) =<< match a b)