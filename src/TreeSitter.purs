module TreeSitter where

import Prelude

import TreeSitter.Raw as Raw
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (runEffectFn1)
import Data.Function.Uncurried (runFn3)

type LanguageName = String

type Parser = Raw.Parser
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

instance showSyntaxNode :: Show SyntaxNode where
    show (SyntaxNode (Raw.SyntaxNode {toString})) = toString unit

instance showTree :: Show Tree where
    show  = show <<< rootNode