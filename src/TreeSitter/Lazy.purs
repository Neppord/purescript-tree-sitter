--| The lazy module contains only a thin wrapper around the foreign interface,
--| this should be a stable part of the library to depend on. But it only implement
--| the most common operations and just slightly more simple then the raw module

module TreeSitter.Lazy where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree)
import Data.Array (null)
import Data.Function.Uncurried (runFn3)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Uncurried (runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (lookup)
import TreeSitter.Raw as Raw
import Unsafe.Coerce (unsafeCoerce)

type LanguageName = String

type Parser = Raw.Parser
type Point = Raw.Point
newtype Tree = Tree Raw.Tree
newtype SyntaxNode = SyntaxNode Raw.SyntaxNode

parse :: LanguageName -> String -> Cofree Array SyntaxNode
parse languageName =
    parseString (mkParser languageName)
        >>> rootNode
        >>> buildCofree (\s -> Tuple s $ children s)

-- | Imports the language and creates a parser with that language set
-- | as the language to parse
mkParser :: LanguageName -> Parser
mkParser name = unsafePerformEffect do
    let parser@(Raw.Parser { setLanguage }) = Raw.mkParser unit
    let language = Raw.mkLanguage name
    _ <- runEffectFn1 setLanguage language
    pure parser

-- | Parses the string and returns a tree
parseString :: Parser -> String -> Tree
parseString (Raw.Parser { parse }) input =
    Tree $ runFn3 parse input Raw.noTree Raw.noOptions

-- | Not sure what this does with the old tree
reParseString :: Parser -> String -> Tree -> Tree
reParseString (Raw.Parser { parse }) input (Tree oldTree) =
    Tree $ runFn3 parse input (Raw.toMaybeTree oldTree) Raw.noOptions

rootNode :: Tree -> SyntaxNode
rootNode (Tree (Raw.Tree { rootNode: rootNode' })) = SyntaxNode $ rootNode'

hasChildren :: SyntaxNode -> Boolean
hasChildren = children >>> null >>> not

children :: SyntaxNode -> Array SyntaxNode
children (SyntaxNode (Raw.SyntaxNode { children: children' })) =
    map SyntaxNode children'

namedChildren :: SyntaxNode -> Array SyntaxNode
namedChildren (SyntaxNode (Raw.SyntaxNode { namedChildren: namedChildren' })) =
    map SyntaxNode namedChildren'

isNamed :: SyntaxNode -> Boolean
isNamed (SyntaxNode (Raw.SyntaxNode { isNamed: isNamed' })) =
    isNamed'

startIndex :: SyntaxNode -> Int
startIndex (SyntaxNode (Raw.SyntaxNode { startIndex: startIndex' })) =
    startIndex'

endIndex :: SyntaxNode -> Int
endIndex (SyntaxNode (Raw.SyntaxNode { endIndex: endIndex' })) =
    endIndex'

startPosition :: SyntaxNode -> Point
startPosition (SyntaxNode (Raw.SyntaxNode { startPosition: startPosition' })) =
    startPosition'

endPosition :: SyntaxNode -> Point
endPosition (SyntaxNode (Raw.SyntaxNode { endPosition: endPosition' })) =
    endPosition'

type' :: SyntaxNode -> String
type' (SyntaxNode (Raw.SyntaxNode { type: type'' })) =
    type''

text :: SyntaxNode -> String
text (SyntaxNode (Raw.SyntaxNode { text: text' })) = text'

nodeField :: Partial => String -> SyntaxNode -> Maybe SyntaxNode
nodeField name (SyntaxNode (Raw.SyntaxNode node)) = unsafeCoerce node
    # lookup name

arrayField :: Partial => String -> SyntaxNode -> Array SyntaxNode
arrayField name (SyntaxNode (Raw.SyntaxNode node)) = unsafeCoerce node
    # lookup name
    # fromMaybe []

isMissing :: SyntaxNode -> Boolean
isMissing (SyntaxNode (Raw.SyntaxNode { isMissing: isMissing' })) =
    isMissing' unit

instance showSyntaxNode :: Show SyntaxNode where
    show (SyntaxNode (Raw.SyntaxNode { toString })) = toString unit

instance showTree :: Show Tree where
    show = show <<< rootNode