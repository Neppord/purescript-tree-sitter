--| This is the prefered module to work with if you only want to try out
--| tree-sitter or if you are going to make a quck litle script analysing some
--| code. It has the most risk of changin while the library tries to find the
--| best way to represent tree-sitter for a purescript audience

module TreeSitter.Declarative where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (fromFoldable)
import Data.Show.Generic (genericShow)
import Data.Tree (Forest, Tree, mkTree)
import TreeSitter.Lazy as Lazy
import TreeSitter.Raw as Raw
import TreeSitter.SyntaxTree (SyntaxTree(..))

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


parseAnnotations :: LanguageName -> String -> SyntaxTree Node
parseAnnotations name input =
    Lazy.mkParser name
    # \ parser -> Lazy.parseString parser input
    # treeToDeclerative
    # SyntaxTree

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