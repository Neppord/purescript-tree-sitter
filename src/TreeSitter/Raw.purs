--| The raw module contains only imports of the foreign interface, this should
--| be the most stable part of the library to depend on, but is on a verry
--| tedius level to work with.

module TreeSitter.Raw where

import Data.Function.Uncurried (Fn3)
import Data.Show (class Show, show)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)

newtype Parser = Parser
    { parse :: Fn3 String MaybeTree MaybeOptions Tree
    , getLanguage :: Effect Language
    , setLanguage :: EffectFn1 Language Unit
    , getLogger :: Effect Logger
    , setLogger :: EffectFn1 Logger Unit
    , printDotGraphs :: EffectFn1 Boolean Unit
    }

type Options =
    { bufferSize :: Int
    , includedRanges :: Array Range
    }

type Point =
    { row :: Int
    , column :: Int
    }

type Range =
    { startIndex :: Int
    , endIndex :: Int
    , startPosition :: Point
    , endPosition :: Point
    }

type Edit =
    { startIndex :: Int
    , endIndex :: Int
    , newEndIndex :: Int
    , startPosition :: Point
    , endPosition :: Point
    , newEndPosition :: Point
    }

newtype SyntaxNode = SyntaxNode
    { type :: String
    , tree :: SyntaxNode
    , typeId :: String
    , isNamed :: Boolean
    , text :: String
    , startIndex :: Int
    , endIndex :: Int
    , startPosition :: Point
    , endPosition :: Point
    , parent :: MaybeSyntaxNode
    , children :: Array SyntaxNode
    , namedChildren :: Array SyntaxNode
    , childCount :: Int
    , namedChildCount :: Int
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
    , fields :: Array String
    }

newtype Tree = Tree
    { rootNode :: SyntaxNode
    , edit :: Edit -> Tree
    , getChangeRanges :: Tree -> Array Range
    , getEditedRange :: Tree -> Range
    , printDotGraph :: Effect Unit
    }

instance showSyntaxNode :: Show SyntaxNode where
    show (SyntaxNode { toString }) = toString unit

instance showTree :: Show Tree where
    show (Tree { rootNode }) = show rootNode

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

