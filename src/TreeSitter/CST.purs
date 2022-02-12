module TreeSitter.CST where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Comonad.Cofree as Cofree
import Data.Array (filter, intercalate, (:))
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Functor.Compose (Compose(..))
import Data.Lens.Plated (class Plated)
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Data.Traversable (class Traversable)

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
            loop cst = (show $ Cofree.head cst) <> " :< " <> (showTail $ Cofree.tail cst)
            showTail (Compose (Left str)) =
                "(Compose (Left " <> show str <> "))"
            showTail (Compose (Right children)) =
                "(Compose (Right [" <> (intercalate ", " $ map loop children) <> "]))"

displayGroups :: forall l r. (Show l) => CST l r -> String
displayGroups = displayGroups_ show (const "")

displayGroups_ :: forall l r. (l -> String) -> (r -> String) -> CST l r -> String
displayGroups_ l r (CST cst) = loop cst where
    prefix node = case r (Cofree.head node) of
        "" -> ""
        a -> a <> " "
    loop node = case Cofree.tail node of
        Compose (Left str) -> prefix node <> l str
        Compose (Right children) ->
            prefix node <> "[" <> intercalate ", " (map loop children) <> "]"

sExpression :: forall l r. (r -> String) -> CST l r -> String
sExpression r (CST cst) = loop cst where
    loop :: CST' l r -> String
    loop node = case Cofree.tail node of
        Compose (Left _) -> case r (Cofree.head node) of
            "" -> ""
            s -> "(" <> s <> ")"
        Compose (Right children) ->
            let args = filter (not <<< String.null) $ map loop children
                call = r $ Cofree.head node
            in "(" <>  intercalate " " (call : args) <>")"


infixr 5 mkCST as :<

mkCST :: forall l r. r -> Either l (Array (CST l r)) -> CST l r
mkCST node' children' = CST $ mkCofree node' tail' where
    tail' :: Compose (Either l) Array (CST' l r)
    tail' = Compose $ map unwrap <$> children'

head :: forall l r. CST l r -> r
head (CST cst) = Cofree.head cst

tail :: forall l r. CST l r -> Either l (Array (CST l r))
tail (CST cst) = case Cofree.tail cst of
    Compose x -> map CST <$> x