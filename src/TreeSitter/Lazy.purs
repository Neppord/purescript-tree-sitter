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
import TreeSitter.Raw (Parser(..), Point, SyntaxNode(..), Tree(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Nullable (toMaybe)

type LanguageName = String

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
parseString (Raw.Parser { parse: parse' }) input =
    runFn3 parse' input Raw.noTree Raw.noOptions

-- | Not sure what this does with the old tree
reParseString :: Parser -> String -> Tree -> Tree
reParseString (Parser { parse: parse' }) input oldTree =
    runFn3 parse' input (Raw.toMaybeTree oldTree) Raw.noOptions

rootNode :: Tree -> SyntaxNode
rootNode (Tree { rootNode: rootNode' }) = rootNode'

hasChildren :: SyntaxNode -> Boolean
hasChildren = children >>> null >>> not

children :: SyntaxNode -> Array SyntaxNode
children (SyntaxNode { children: children' }) = children'

namedChildren :: SyntaxNode -> Array SyntaxNode
namedChildren (SyntaxNode { namedChildren: namedChildren' }) =
    namedChildren'

isNamed :: SyntaxNode -> Boolean
isNamed (SyntaxNode { isNamed: isNamed' }) =
    isNamed'

startIndex :: SyntaxNode -> Int
startIndex (SyntaxNode { startIndex: startIndex' }) =
    startIndex'

endIndex :: SyntaxNode -> Int
endIndex (SyntaxNode { endIndex: endIndex' }) =
    endIndex'

startPosition :: SyntaxNode -> Point
startPosition (SyntaxNode { startPosition: startPosition' }) =
    startPosition'

endPosition :: SyntaxNode -> Point
endPosition (SyntaxNode { endPosition: endPosition' }) =
    endPosition'

type' :: SyntaxNode -> String
type' (SyntaxNode { type: type'' }) =
    type''

text :: SyntaxNode -> String
text (SyntaxNode { text: text' }) = text'

nodeField :: Partial => String -> SyntaxNode -> Maybe SyntaxNode
nodeField name (SyntaxNode node) = unsafeCoerce node
    # lookup (name <> "Node")
    # (_ >>= toMaybe)

arrayField :: Partial => String -> SyntaxNode -> Array SyntaxNode
arrayField name (SyntaxNode node) = unsafeCoerce node
    # lookup (name <> "Nodes")
    # fromMaybe []

isMissing :: SyntaxNode -> Boolean
isMissing (SyntaxNode { isMissing: isMissing' }) =
    isMissing' unit