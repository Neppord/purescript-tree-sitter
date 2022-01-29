module TreeSitter.CST where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail)
import Data.Array (filter, intercalate, (:))
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Functor.Compose (Compose(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Traversable (class Traversable)
import TreeSitter.Plated (class Plated)

type CST' l r = (Cofree (Compose (Either l) Array) r)
newtype CST l r = CST (CST' l r)
derive instance Newtype (CST l a) _
derive newtype instance (Eq a, Eq l) => Eq (CST l a)
derive newtype instance Functor (CST l)
derive newtype instance Foldable (CST l)
derive newtype instance Traversable (CST l)
derive newtype instance Plated (CST l a)

instance (Show l, Show r) => Show (CST l r) where
    show (CST cst') = "CST $ " <> loop cst'
        where
            loop cst = (show $ head cst) <> " :< " <> (showTail $ tail cst)
            showTail (Compose (Left str)) =
                "(Compose (Left " <> show str <> "))"
            showTail (Compose (Right children)) =
                "(Compose (Right [" <> (intercalate ", " $ map loop children) <> "]))"

displayGroups :: forall l r. (Show l) => CST l r -> String
displayGroups = displayGroups_ show (const "")

displayGroups_ :: forall l r. (l -> String) -> (r -> String) -> CST l r -> String
displayGroups_ l r (CST cst) = loop cst where
    prefix node = case r (head node) of
        "" -> ""
        a -> a <> " "
    loop node = case tail node of
        Compose (Left str) -> prefix node <> l str
        Compose (Right children) ->
            prefix node <> "[" <> intercalate ", " (map loop children) <> "]"

sexpression :: forall l r. (r -> String) -> CST l r -> String
sexpression r (CST cst) = loop cst where
    loop :: CST' l r -> String
    loop node = case tail node of
        Compose (Left _) -> case r (head node) of
            "" -> ""
            s -> "(" <> s <> ")"
        Compose (Right children) ->
            let args = filter (not <<< String.null) $ map loop children
                call = r $ head node
            in "(" <>  intercalate " " (call : args) <>")"