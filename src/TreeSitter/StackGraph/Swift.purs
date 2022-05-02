module TreeSitter.StackGraph.Swift where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail)
import Data.Array (concat, filter, find)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (for)
import TreeSitter.Lazy (endIndex, startIndex, text, type')
import TreeSitter.Raw (SyntaxNode)
import TreeSitter.StackGraph (CreateGraph, declare, namedScope', newId, scope, scopeWithId, usage_)
import TreeSitteer.Codegen.Swift (SourceFile)

type Tree = Cofree Array SyntaxNode

childrenOfType :: String -> Tree -> Array (Tree)
childrenOfType t node = filter (head >>> type' >>> (_ == t))
    (tail node)

childOfType :: String -> Tree -> Maybe (Tree)
childOfType t node = find (head >>> type' >>> (_ == t))
    (tail node)

assert :: Maybe (CreateGraph (Array Int)) -> CreateGraph (Array Int)
assert = fromMaybe (pure [])

position :: SyntaxNode -> { start :: Int, end :: Int }
position node = { start: startIndex node, end: endIndex node }

functionDeclaration :: Int -> Tree -> CreateGraph (Array Int)
functionDeclaration globalScope t = fromMaybe (pure []) ado
    identifier <- head <$> childOfType "simple_identifier" t
    functionBody <- childOfType "function_body" t
    in
        do
            usage_ "score1" { start: 474, end: 474 + 6 } globalScope
            id <- declare (text identifier)
                { start: startIndex identifier, end: endIndex identifier }
            pure [ id ]

statement :: Int -> CreateGraph (Array Int)
statement prevScope = do
    pure []

classDeclaration :: Int -> Tree -> CreateGraph (Array Int)
classDeclaration globalScope t = fromMaybe (pure []) ado
    identifier <- head <$> childOfType "type_identifier" t
    classBody <- childOfType "class_body" t
    in
        do
            classScope <- scope [ globalScope ]
            id <- namedScope' (text identifier) do
                ids <- for
                    (childrenOfType "function_declaration" classBody)
                    (functionDeclaration classScope)
                pure $ concat ids
            pure [ id ]

callExpression :: Int -> Tree -> CreateGraph (Array Int)
callExpression scope t = assert ado
    identifier <- head <$> childOfType "simple_identifier" t
    in
        do
            usage_ (text identifier) (position identifier) scope
            pure []

expression :: Int -> Tree -> CreateGraph (Array Int)
expression globalScope subtree = case type' $ head subtree of
    "function_declaration" -> functionDeclaration globalScope subtree
    "class_declaration" -> classDeclaration globalScope subtree
    "call_expression" -> callExpression globalScope subtree
    _ -> pure []

sourceFile :: Tree -> CreateGraph Unit
sourceFile sourceTree = do
    globalScope <- newId
    ids <- for (tail sourceTree) (expression globalScope)
    void $ scopeWithId globalScope $ concat ids

typedSourceFile :: SourceFile SyntaxNode -> CreateGraph Unit
typedSourceFile _ = do
    pure unit

