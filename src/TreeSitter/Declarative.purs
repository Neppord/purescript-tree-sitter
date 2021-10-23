--| This is the prefered module to work with if you only want to try out
--| tree-sitter or if you are going to make a quck litle script analysing some
--| code. It has the most risk of changin while the library tries to find the
--| best way to represent tree-sitter for a purescript audience

module TreeSitter.Declarative where

import Prelude

import TreeSitter.Lazy as Lazy
import TreeSitter.Raw as Raw
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)


type LanguageName = String

data Named = Named | Unnamed | Missing

newtype Tree = Tree
    { named::Named
    , type :: String
    , range :: Raw.Range
    , children :: Array Tree
    }

derive instance genericNamed :: Generic Named _
instance showNamed :: Show Named where
    show named = genericShow named

derive instance genericTree :: Generic Tree _
instance showTree :: Show Tree where
    show named = genericShow named

parse :: LanguageName -> String -> Tree
parse name input = Lazy.parseString parser input # treeToDeclerative
    where parser = Lazy.mkParser name

treeToDeclerative :: Lazy.Tree -> Tree
treeToDeclerative = nodeToDeclerative <<< Lazy.rootNode

nodeToDeclerative :: Lazy.SyntaxNode -> Tree
nodeToDeclerative node = Tree {named, type: type', range, children}
    where
        named | Lazy.isNamed node = Named
        named | Lazy.isMissing node = Missing
        named = Unnamed
        type' = Lazy.type' node
        children = map nodeToDeclerative $ Lazy.children node
        range =
            { startIndex: Lazy.startIndex node
            , endIndex: Lazy.endIndex node
            , startPosition: Lazy.startPosition node
            , endPosition: Lazy.endPosition node
            }
