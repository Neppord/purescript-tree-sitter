module Data.Lens.Plated where

import Prelude

import Control.Comonad.Cofree (Cofree, head, mkCofree, tail)
import Data.Lens (Setter, Traversal', over, toListOf, wander)
import Data.List.Types (List, (:))
import Data.Maybe (Maybe, maybe)
import Data.Traversable (class Traversable, traverse)
import Data.Lens.Fold (foldMapOf)
import Data.Lens.Internal.Forget (Forget)

class Plated a where
    plate :: Traversal' a a

instance (Traversable f) => Plated (Cofree f a) where
    plate = wander (\f tree -> mkCofree (head tree) <$> traverse f (tail tree))

children :: forall a. Plated a => a -> List a
children = toListOf plate

universe :: forall a. Plated a => a -> List a
universe = universeOf plate

universeOf
    :: forall a b c. (Forget (List a) a c -> Forget (List a) a b) -> a -> List a
universeOf p = go
    where
    go a = a : foldMapOf p go a

rewrite :: forall a. Plated a => (a -> Maybe a) -> a -> a
rewrite = rewriteOf plate

rewriteOf :: forall a b. Setter a b a b -> (b -> Maybe a) -> a -> b
rewriteOf plates f = go
    where
    go = transformOf plates (\x -> maybe x go (f x))

transformOf :: forall a b. Setter a b a b -> (b -> b) -> a -> b
transformOf plates f = go
    where
    go s = s
        # over plates (\s' -> go s')
        # f