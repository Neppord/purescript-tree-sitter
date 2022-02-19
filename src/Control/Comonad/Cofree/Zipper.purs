module Control.Comonad.Cofree.Zipper where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree as Cofree
import Control.Monad.Writer (execWriter, tell)
import Data.Array as Array
import Data.Eq (class Eq1, eq1)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))

class Uncons f where
    uncons :: forall a. f a -> Maybe { head :: a, tail :: f a }

instance Uncons Array where
    uncons = Array.uncons

instance Uncons List where
    uncons = List.uncons

instance Uncons Maybe where
    uncons Nothing = Nothing
    uncons (Just a) = Just {head: a , tail: Nothing}

instance Uncons First where
    uncons (First m) = case uncons m of
        Nothing -> Nothing
        Just {head} -> Just {head, tail: First Nothing}

newtype Trace f a = Trace {left:: f (Cofree f a), focus:: a, right:: f (Cofree f a)}
instance (Eq1 f, Eq a) => Eq (Trace f a) where
    eq (Trace a) (Trace b) = a.focus == b.focus
        && a.left `eq1` b.left
        && a.right `eq1` b.right

instance (Functor f) => Functor (Trace f) where
    map f (Trace trace) = Trace
        { left: (map <<< map) f trace.left
        , focus: f trace.focus
        , right: (map <<< map) f trace.right
        }

instance (Functor f, Show a, Show (Showfree f a), Show (f String)) => Show (Trace f a) where
    show (Trace trace) = execWriter do
        tell "Trace "
        tell "{ left:"
        tell $ show $ map (show <<< Showfree) trace.left
        tell ", focus:"
        tell $ show trace.focus
        tell ", rigth:"
        tell $ show $ map (show <<< Showfree) trace.right
        tell "}"

newtype Zipper f a = Zipper
    { extract :: Cofree f a
    , trace :: List (Trace f a)
    , left :: f (Cofree f a)
    , right :: f (Cofree f a)
    }

instance (Eq1 f, Eq a, Eq (Cofree f a)) => Eq (Zipper f a) where
    eq (Zipper a) (Zipper b)
        = a.extract == b.extract
        && a.trace == b.trace
        && eq1 a.left b.left
        && eq1 a.right b.right

instance (Show (Showfree f a), Show (Trace f a)) => Show (Zipper f a) where
    show (Zipper zipper) = execWriter do
        tell "(Zipper "
        tell "{ extract: "
        tell $ show $ Showfree zipper.extract
        tell ", trace: "
        tell $ show zipper.trace
        tell ", left: "
        tell ", right: "
        tell " })"
instance (Functor f) =>  Functor (Zipper f) where
    map f (Zipper {extract, trace, left, right}) = Zipper
        { extract : map f extract
        , trace : (map <<< map) f trace
        , left : (map <<< map) f left
        , right : (map <<< map) f right
        }

newtype Showfree f a = Showfree (Cofree f a)

instance (Functor f, Show a, Show (f String)) => Show (Showfree f a) where
    show (Showfree cofree) = execWriter do
        tell "("
        tell $ show $ Cofree.head cofree
        tell " :< "
        tell "("
        tell $ show $ map (show <<< Showfree) $ Cofree.tail cofree
        tell ")"
        tell ")"

fromCofree :: forall f a. Monoid (f (Cofree f a)) => Cofree f a -> Zipper f a
fromCofree cofree = Zipper
    { extract : cofree
    , trace : mempty
    , left : mempty
    , right : mempty
    }

goDown :: forall a f. Uncons f => Monoid (f (Cofree f a)) => Zipper f a -> Maybe (Zipper f a)
goDown (Zipper zipper) = do
    let
        focus = Cofree.head zipper.extract
        trace = Trace {left: zipper.left, focus, right: zipper.right}
        left = mempty
    {head, tail} <- uncons $ Cofree.tail zipper.extract
    pure $ Zipper
        { extract: head
        , trace: trace : zipper.trace
        , left
        , right: tail
        }

goUp :: forall a f. Applicative f => Monoid (f (Cofree f a)) => Zipper f a -> Maybe (Zipper f a)
goUp (Zipper zipper) = do
    {head, tail} <- uncons zipper.trace
    let (Trace trace) = head
    pure $ Zipper
        { extract: trace.focus :< (zipper.left <> pure zipper.extract <> zipper.right)
        , trace: tail
        , left: trace.left
        , right: trace.right
        }

goRight :: forall a f. Uncons f => Applicative f => Monoid (f (Cofree f a)) => Zipper f a -> Maybe (Zipper f a)
goRight (Zipper zipper) = do
    {head: extract, tail: right} <- uncons $ zipper.right
    pure $ Zipper
        { extract
        , trace: zipper.trace
        , left:  pure zipper.extract <> zipper.left
        , right
        }

goLeft :: forall a f. Uncons f => Applicative f => Monoid (f (Cofree f a)) => Zipper f a -> Maybe (Zipper f a)
goLeft (Zipper zipper) = do
    {head: extract, tail: left} <- uncons $ zipper.left
    pure $ Zipper
        { extract
        , trace: zipper.trace
        , left
        , right:  pure zipper.extract <> zipper.right
        }
