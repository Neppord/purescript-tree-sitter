module Control.Comonad.Cofree.Zipper where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree as Cofree
import Control.Monad.Writer (execWriter, tell)
import Data.Eq (class Eq1, eq1)
import Data.List (List)


newtype Zipper f a = Zipper
    { extract :: Cofree f a
    , up :: List (Zipper f a)
    , left :: f (Cofree f a)
    , right :: f (Cofree f a)
    }

instance (Eq1 f, Eq a, Eq (Cofree f a)) => Eq (Zipper f a) where
    eq (Zipper a) (Zipper b)
        = a.extract == b.extract
        && a.up == b.up
        && eq1 a.left b.left
        && eq1 a.right b.right

instance (Functor f, Show (f String), Show (f a), Show a) => Show (Zipper f a) where
    show (Zipper zipper) = execWriter do
        tell "(Zipper "
        tell "{ extract: "
        tell $ showCofree zipper.extract
        tell ", up: "
        tell ", left: "
        tell ", right: "
        tell " })"

showCofree :: forall f a. Functor f => Show (f String) => Show a => Cofree f a -> String
showCofree cofree = execWriter do
    tell "("
    tell $ show $ Cofree.head cofree
    tell " :< "
    tell "("
    tell $ show $ map showCofree $ Cofree.tail cofree
    tell ")"
    tell ")"

fromCofree :: forall f a. Monoid (f (Cofree f a)) => Cofree f a -> Zipper f a
fromCofree cofree = Zipper
    { extract : cofree
    , up : mempty
    , left : mempty
    , right : mempty
    }
