module TreeSitter.CST where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Either (Either)
import Data.Foldable (class Foldable)
import Data.Functor.Compose (Compose)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)
import TreeSitter.Plated (class Plated)

newtype CST l r = CST (Cofree (Compose (Either l) Array) r)
derive instance Newtype (CST l a) _
derive newtype instance (Eq a, Eq l) => Eq (CST l a)
derive newtype instance Functor (CST l)
derive newtype instance Foldable (CST l)
derive newtype instance Traversable (CST l)
derive newtype instance Plated (CST l a)