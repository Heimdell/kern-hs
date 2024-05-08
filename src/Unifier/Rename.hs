
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Unifier.Rename where

import Control.Monad.State

newtype RenameT n m a = RenameT
  { unRenameT :: StateT Int m a
  }
  deriving newtype (Functor, Applicative, Monad)

class (Monad m) => MonadRename n m | m -> n where
  rename :: n -> m n

instance {-# OVERLAPPABLE #-}
  (Monad m, MonadRename n m, MonadTrans t)
    =>
  MonadRename n (t m)
    where
  rename = lift . rename
