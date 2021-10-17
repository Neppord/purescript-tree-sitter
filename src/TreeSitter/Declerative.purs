module TreeSitter.Declerative where

import Prelude

import TreeSitter.Laizy as Laizy
import TreeSitter.Raw as Raw
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)


type LanguageName = String

data Tree
    = Normal { type :: String, children :: Array Tree, range :: Raw.Range }
    | Named  { type :: String, children :: Array Tree, range :: Raw.Range }
    | Missing  { type :: String, children :: Array Tree, range :: Raw.Range }

derive instance genericTree :: Generic Tree _
instance showTree :: Show Tree where
    show tree = genericShow tree

parse :: LanguageName -> String -> Tree
parse name input = Laizy.parseString parser input # treeToDeclerative
    where parser = Laizy.mkParser name

treeToDeclerative :: Laizy.Tree -> Tree
treeToDeclerative = nodeToDeclerative <<< Laizy.rootNode

nodeToDeclerative :: Laizy.SyntaxNode -> Tree
nodeToDeclerative node = const {type: type', children, range}
    where
        const | Laizy.isNamed node = Named
        const | Laizy.isMissing node = Missing
        const = Normal
        type' = Laizy.type' node
        children = map nodeToDeclerative $ Laizy.children node
        range =
            { startIndex: Laizy.startIndex node
            , endIndex: Laizy.endIndex node
            , startPosition: Laizy.startPosition node
            , endPosition: Laizy.endPosition node
            }
