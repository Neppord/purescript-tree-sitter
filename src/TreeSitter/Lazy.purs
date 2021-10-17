module TreeSitter.Lazy where

import Prelude

import TreeSitter.Raw as Raw
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (runEffectFn1)
import Data.Function.Uncurried (runFn3)

type LanguageName = String

type Parser = Raw.Parser
type Point = Raw.Point
newtype Tree = Tree Raw.Tree
newtype SyntaxNode = SyntaxNode Raw.SyntaxNode

-- | Imports the language and creates a parser with that language set
-- | as the language to parse
mkParser :: LanguageName -> Parser
mkParser name =  unsafePerformEffect do
    let parser@(Raw.Parser {setLanguage}) = Raw.mkParser unit
    let language = Raw.mkLanguage name
    _ <- runEffectFn1 setLanguage language
    pure parser

-- | Parses the string and returns a tree
parseString :: Parser -> String -> Tree
parseString (Raw.Parser {parse}) input =
    Tree $ runFn3 parse input Raw.noTree Raw.noOptions

-- | Not sure what this does with the old tree
reParseString :: Parser -> String -> Tree -> Tree
reParseString (Raw.Parser {parse}) input (Tree oldTree) =
    Tree $ runFn3 parse input (Raw.toMaybeTree oldTree) Raw.noOptions

rootNode :: Tree -> SyntaxNode
rootNode (Tree (Raw.Tree {rootNode: rootNode'})) = SyntaxNode $ rootNode'

children :: SyntaxNode -> Array SyntaxNode
children (SyntaxNode (Raw.SyntaxNode {children: children'})) =
 map SyntaxNode children'

namedChildren :: SyntaxNode -> Array SyntaxNode
namedChildren (SyntaxNode (Raw.SyntaxNode {namedChildren: namedChildren'})) =
 map SyntaxNode namedChildren'

isNamed :: SyntaxNode -> Boolean
isNamed (SyntaxNode (Raw.SyntaxNode {isNamed: isNamed'})) =
    isNamed'

startIndex :: SyntaxNode -> Int
startIndex (SyntaxNode (Raw.SyntaxNode {startIndex: startIndex'})) =
    startIndex'

endIndex :: SyntaxNode -> Int
endIndex (SyntaxNode (Raw.SyntaxNode {endIndex: endIndex'})) =
    endIndex'

startPosition :: SyntaxNode -> Point
startPosition (SyntaxNode (Raw.SyntaxNode {startPosition: startPosition'})) =
    startPosition'

endPosition :: SyntaxNode -> Point
endPosition (SyntaxNode (Raw.SyntaxNode {endPosition: endPosition'})) =
    endPosition'

type' :: SyntaxNode -> String
type' (SyntaxNode (Raw.SyntaxNode {type: type''})) =
    type''

isMissing :: SyntaxNode -> Boolean
isMissing (SyntaxNode (Raw.SyntaxNode {isMissing: isMissing'})) =
    isMissing' unit

instance showSyntaxNode :: Show SyntaxNode where
    show (SyntaxNode (Raw.SyntaxNode {toString})) = toString unit

instance showTree :: Show Tree where
    show  = show <<< rootNode