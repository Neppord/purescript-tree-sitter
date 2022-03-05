module Test.TreeSitter.StackGraph.Swift where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail)
import Data.Array (filter, find, mapMaybe, reverse)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import TreeSitter.Lazy (SyntaxNode, endIndex, startIndex, text, type')
import TreeSitter.StackGraph (CreateGraph, declare, namedScope, scope, supply)

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
    :: Cofree Array SyntaxNode -> Maybe (CreateGraph Int)
functionDeclaration t = do
    identifier <- head <$> childOfType "simple_identifier" t
    pure $ declare (text identifier)
        { start: startIndex identifier, end: endIndex identifier }

classDeclaration
    :: Cofree Array SyntaxNode -> Maybe (CreateGraph Int)
classDeclaration t = do
    identifier <- head <$> childOfType "type_identifier" t
    classBody <- childOfType "class_body" t
    let
        methods = childrenOfType "function_declaration" classBody
            # mapMaybe functionDeclaration
    pure $ namedScope (text identifier) methods

sourceFile :: Cofree Array SyntaxNode -> CreateGraph Unit
sourceFile sourceTree = do
    ids <- (tail sourceTree)
        # sequence <<< mapMaybe
              ( \(subtree :: Cofree Array SyntaxNode) ->
                    case type' $ head subtree of
                        "function_declaration" ->
                            functionDeclaration subtree
                        "class_declaration" ->
                            classDeclaration subtree
                        _ -> Nothing
              )
    file <- scope (reverse ids)
    void $ supply "hello" file