--| This is the prefered module to work with if you only want to try out
--| tree-sitter or if you are going to make a quck litle script analysing some
--| code. It has the most risk of changin while the library tries to find the
--| best way to represent tree-sitter for a purescript audience

module TreeSitter.Declarative where

import Prelude
import TreeSitter.Plated (class Plated)

import Control.Comonad.Cofree as Cofree
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Lens.Lens (lens)
import Data.Lens.Traversal (traversed)
import Data.Lens.Types (Lens')
import Data.List (List, fromFoldable)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tree (Forest, Tree, mkTree, showTree)
import Data.Tree as Tree
import TreeSitter.Lazy as Lazy
import TreeSitter.Raw as Raw

type LanguageName = String

data Named = Named | Unnamed | Missing
derive instance Generic Named _
derive instance Eq Named
instance Show Named where
    show named = genericShow named

type Node =
    { named :: Named
    , type :: String
    , range :: Raw.Range
    }

newtype SyntaxTree a = SyntaxTree (Tree.Tree a)
derive instance Newtype (SyntaxTree a) _
derive instance Functor SyntaxTree

instance Foldable SyntaxTree where
    foldr f zero tree = foldr f zero $ unwrap tree
    foldl f zero tree = foldl f zero $ unwrap tree
    foldMap f tree = foldMap f $ unwrap tree

instance Traversable SyntaxTree where
    traverse f syntaxTree = SyntaxTree <$> traverse f (unwrap syntaxTree)
    sequence = map SyntaxTree <<< sequence <<< unwrap

instance Plated (SyntaxTree a) where
    plate = _children <<< traversed

instance  Show a => Show (SyntaxTree a) where
    show = showTree <<< unwrap

node :: forall a. SyntaxTree a -> a
node (SyntaxTree tree) = Cofree.head tree

children :: forall a. SyntaxTree a -> List (SyntaxTree a)
children = map SyntaxTree <<< Cofree.tail <<< unwrap

_children :: forall a. Lens' (SyntaxTree a) (List (SyntaxTree a))
_children = lens children replaceChildren

replaceChildren :: forall a. SyntaxTree a -> List (SyntaxTree a) -> SyntaxTree a
replaceChildren tree children' = SyntaxTree $ mkTree (node tree) $ map unwrap children'

parse :: LanguageName -> String -> SyntaxTree Node
parse name input = SyntaxTree $ Lazy.parseString parser input # treeToDeclerative
    where parser = Lazy.mkParser name

treeToDeclerative :: Lazy.Tree -> Tree Node
treeToDeclerative = nodeToDeclerative <<< Lazy.rootNode

nodeToDeclerative :: Lazy.SyntaxNode -> Tree Node
nodeToDeclerative node' = mkTree ({named, type: type', range}) children'
    where
        named | Lazy.isNamed node' = Named
        named | Lazy.isMissing node' = Missing
        named = Unnamed
        type' = Lazy.type' node'
        children' :: Forest Node
        children' = fromFoldable $ map nodeToDeclerative $ Lazy.children node'
        range =
            { startIndex: Lazy.startIndex node'
            , endIndex: Lazy.endIndex node'
            , startPosition: Lazy.startPosition node'
            , endPosition: Lazy.endPosition node'
            }