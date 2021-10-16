module TreeSitter.Raw where

import Effect.Uncurried (EffectFn1)
import Effect (Effect)
import Data.Unit (Unit)
import Data.Function.Uncurried (Fn3)

newtype Parser = Parser
    { parse :: Fn3 String MaybeTree MaybeOptions Tree
    , getLanguage :: Effect Language
    , setLanguage :: EffectFn1 Language Unit
    , getLogger :: Effect Logger
    , setLogger :: EffectFn1 Logger Unit
    , printDotGraphs :: EffectFn1 Boolean Unit
    }

type Options =
    { bufferSize :: Number
    , includedRanges :: Array Range
    }

type Point =
    { row :: Number
    , column :: Number
    }

type Range =
    { startIndex :: Number
    , endIndex :: Number
    , startPosition :: Point
    , endPosition :: Point
    }

type Edit =
    { startIndex :: Number
    , endIndex :: Number
    , newEndIndex :: Number
    , startPosition :: Point
    , endPosition :: Point
    , newEndPosition :: Point
    }

newtype SyntaxNode = SyntaxNode
    { type :: String
    , tree :: SyntaxNode
    , typeId :: String
    , isNamed :: Boolean
    , test :: String
    , startIndex :: Number
    , endIndex :: Number
    , startPosition :: Point
    , endPosition :: Point
    , parent :: MaybeSyntaxNode
    , children :: Array SyntaxNode
    , namedChildren :: Array SyntaxNode
    , childCount :: Number
    , namedChildCount :: Number
    , firstChild :: MaybeSyntaxNode
    , firstNamedChild :: MaybeSyntaxNode
    , lastChild :: MaybeSyntaxNode
    , nextSibling :: MaybeSyntaxNode
    , nextNamedSibling :: MaybeSyntaxNode
    , previousSibling :: MaybeSyntaxNode
    , previousNamedSibling :: MaybeSyntaxNode
    , hasChanges :: Unit -> Boolean
    , hasErrors :: Unit -> Boolean
    , isMissing :: Unit -> Boolean
    , toString :: Unit -> String
    }

newtype Tree = Tree
    { rootNode :: SyntaxNode
    , edit :: Edit -> Tree
    , getChangeRanges :: Tree -> Array Range
    , getEditedRange :: Tree -> Range
    , printDotGraph :: Effect Unit
    }

foreign import data Language :: Type
foreign import data MaybeOptions :: Type
foreign import data MaybeSyntaxNode :: Type
foreign import data MaybeTree :: Type
foreign import data PackedTree :: Type
foreign import data PackedSyntaxNode :: Type
foreign import data Query :: Type
foreign import data Logger :: Type

foreign import mkParser :: Unit -> Parser
foreign import mkLanguage :: String -> Language
foreign import noTree :: MaybeTree
foreign import noOptions :: MaybeOptions
foreign import toMaybeTree :: Tree -> MaybeTree
foreign import toMaybeOptions :: Options -> MaybeOptions

