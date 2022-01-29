--| This is the prefered module to work with if you only want to try out
--| tree-sitter or if you are going to make a quck litle script analysing some
--| code. It has the most risk of changin while the library tries to find the
--| best way to represent tree-sitter for a purescript audience

module TreeSitter.Declarative where

import Prelude

import Control.Comonad.Cofree ((:<))
import Data.Array (null)
import Data.Either (Either(..))
import Data.Functor.Compose (Compose(..))
import Data.Generic.Rep (class Generic)
import Data.List (fromFoldable)
import Data.Show.Generic (genericShow)
import Data.Tree (Forest, Tree, mkTree)
import TreeSitter.CST (CST(..))
import TreeSitter.CST as CST
import TreeSitter.Lazy as Lazy
import TreeSitter.Raw as Raw
import TreeSitter.SyntaxTree (SyntaxTree(..))
import Data.Array (fold)

type LanguageName = String

data Named = Named | Unnamed | Missing
derive instance Generic Named _
derive instance Eq Named
instance Show Named where
    show named'' = genericShow named''

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

parseCST :: LanguageName -> String -> DeclarativeCST
parseCST name input =
    Lazy.mkParser name
    # \ parser -> Lazy.parseString parser input
    # lazyTreeToCST

type DeclarativeCST = CST String CSTNode
type CSTNode = {named :: Named , type :: String}

lazyTreeToCST :: Lazy.Tree -> DeclarativeCST
lazyTreeToCST = CST <<< loop <<< Lazy.rootNode where
    loop node  = head node :< tail node
    head node = { named: named' node , type: Lazy.type' node }
    tail node | null $ Lazy.children node = (Compose (Left $ Lazy.text node))
    tail node = (Compose (Right (loop <$> Lazy.children node)))

named :: DeclarativeCST -> Named
named = _.named <<< CST.head

type' :: DeclarativeCST -> String
type' = _.type <<< CST.head

text :: DeclarativeCST -> String
text cst = case CST.tail cst of
    Left str -> str
    Right children -> fold $ map text children

named' :: Lazy.SyntaxNode -> Named
named' node | Lazy.isNamed node = Named
named' node | Lazy.isMissing node = Missing
named' _ = Unnamed

sExpression :: DeclarativeCST -> String
sExpression cst = CST.sExpression toString cst where
    toString {named: Named, type: type''} = type''
    toString _ = ""

treeToDeclerative :: Lazy.Tree -> Tree Node
treeToDeclerative = nodeToDeclerative <<< Lazy.rootNode

nodeToDeclerative :: Lazy.SyntaxNode -> Tree Node
nodeToDeclerative node' = mkTree ({named: named' node', type: type'', range}) children'
    where
        type'' = Lazy.type' node'
        children' :: Forest Node
        children' = fromFoldable $ map nodeToDeclerative $ Lazy.children node'
        range =
            { startIndex: Lazy.startIndex node'
            , endIndex: Lazy.endIndex node'
            , startPosition: Lazy.startPosition node'
            , endPosition: Lazy.endPosition node'
            }