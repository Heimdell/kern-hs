{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Fix (module Data.Fix, module M) where

import Data.Functor.Compose as M

type Tree    f a = Fix (Compose ((,) a) f)
type CutTree f a = Fix (Compose Maybe (Compose ((,) a) f))
data Fix     f   = Fix { unFix :: f (Fix f) }

{-# COMPLETE (:>) #-}
{-# COMPLETE (:>?), None #-}

pattern (:>)  :: a -> f (Tree    f a) -> Tree    f a
pattern (:>?) :: a -> f (CutTree f a) -> CutTree f a
pattern  None ::                         CutTree f a
pattern a :>  f = Fix (Compose                (a, f))
pattern a :>? f = Fix (Compose (Just (Compose (a, f))))
pattern None    = Fix (Compose  Nothing)

deriving stock instance (Show (f (Fix  f))) => Show (Fix f)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (Fix f) = alg (fmap (cata alg) f)