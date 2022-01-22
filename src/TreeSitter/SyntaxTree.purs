module TreeSitter.SyntaxTree where

import Prelude

import Control.Comonad.Cofree as Cofree
import Data.Foldable (class Foldable)
import Data.Lens.Lens (lens)
import Data.Lens.Types (Lens')
import Data.List (List)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Traversable)
import Data.Tree (mkTree, showTree)
import Data.Tree as Tree
import TreeSitter.Plated (class Plated)

newtype SyntaxTree a = SyntaxTree (Tree.Tree a)
derive instance Newtype (SyntaxTree a) _
derive newtype instance Eq a => Eq (SyntaxTree a)
derive newtype instance Functor SyntaxTree
derive newtype instance Foldable SyntaxTree
derive newtype instance Traversable SyntaxTree
derive newtype instance Plated (SyntaxTree a)

instance  Show a => Show (SyntaxTree a) where
    show = showTree <<< unwrap


mkSyntaxTree :: forall a. a -> List (SyntaxTree a) -> SyntaxTree a
mkSyntaxTree node' children' = SyntaxTree $ mkTree node' $ map unwrap children'

node :: forall a. SyntaxTree a -> a
node (SyntaxTree tree) = Cofree.head tree

children :: forall a. SyntaxTree a -> List (SyntaxTree a)
children = map SyntaxTree <<< Cofree.tail <<< unwrap

_children :: forall a. Lens' (SyntaxTree a) (List (SyntaxTree a))
_children = lens children replaceChildren

replaceChildren :: forall a. SyntaxTree a -> List (SyntaxTree a) -> SyntaxTree a
replaceChildren tree children' = SyntaxTree $ mkTree (node tree) $ map unwrap children'