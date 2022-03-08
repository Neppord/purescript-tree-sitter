module TreeSitter.StackGraph.Swift where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail)
import Data.Array (concat, filter, find)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (for)
import TreeSitter.Lazy (SyntaxNode, endIndex, startIndex, text, type')
import TreeSitter.StackGraph (CreateGraph, declare, namedScope', newId, scope, scopeWithId, usage_)

childrenOfType
    :: String
    -> Cofree Array SyntaxNode
    -> Array (Cofree Array SyntaxNode)
childrenOfType t node = filter (head >>> type' >>> (_ == t))
    (tail node)

childOfType
    :: String
    -> Cofree Array SyntaxNode
    -> Maybe (Cofree Array SyntaxNode)
childOfType t node = find (head >>> type' >>> (_ == t))
    (tail node)

functionDeclaration
    :: Int -> Cofree Array SyntaxNode -> CreateGraph (Array Int)
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

classDeclaration
    :: Int -> Cofree Array SyntaxNode -> CreateGraph (Array Int)
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

callExpression :: Int -> CreateGraph (Array Int)
callExpression scope = do
    usage_ "hello" { start: 104, end: 109 } scope
    pure []

sourceFile :: Cofree Array SyntaxNode -> CreateGraph Unit
sourceFile sourceTree = do
    globalScope <- newId
    ids <- for (tail sourceTree)
        ( \subtree -> case type' $ head subtree of
              "function_declaration" -> functionDeclaration globalScope subtree
              "class_declaration" -> classDeclaration globalScope subtree
              "call_expression" -> callExpression globalScope
              _ -> pure []
        )
    void $ scopeWithId globalScope $ concat ids
