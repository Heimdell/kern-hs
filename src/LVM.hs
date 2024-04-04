{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module LVM where

import Control.Arrow
import Control.Monad
import Data.Map qualified as Map
import Data.Traversable
import Data.IORef
import Data.Functor.Compose
import Data.Function

import Polysemy
import Polysemy.State
import Polysemy.Reader
import Polysemy.Error
import Polysemy.Fixpoint
