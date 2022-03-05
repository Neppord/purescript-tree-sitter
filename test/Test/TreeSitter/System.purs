module Test.TreeSitter.System where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, buildCofree, head, tail)
import Control.Comonad.Cofree.Zipper (Zipper, fromCofree, goUp)
import Control.Extend (duplicate)
import Data.Array (concat, elem, filter, find, foldMap, foldr, fromFoldable, mapMaybe, reverse)
import Data.Lens.Plated (universe)
import Data.Map.Internal (toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)
import TreeSitter.Lazy (SyntaxNode, children, endIndex, mkParser, parseString, rootNode, startIndex, text, type')
import TreeSitter.StackGraph (CreateGraph, Node(..), createGraph_, declare, demand, scope, supply)

program :: String
program =
    """
function_name () {
    cat "my_file"
}
other_function_name () {
    function_name
}
"""

swiftProgram :: String
swiftProgram =
    """
func hello() {
    print("hello")
}
class SomeClass {
    func hello() {
        print("hello")
    }
}
hello()
"""

swiftTree :: Cofree Array SyntaxNode
swiftTree = swiftProgram
    # parseString (mkParser "swift")
    # rootNode
    # buildCofree (\s -> Tuple s $ children s)

tree :: Cofree Array SyntaxNode
tree = program
    # parseString (mkParser "bash")
    # rootNode
    # buildCofree (\s -> Tuple s $ children s)

headType :: forall a. Cofree a SyntaxNode -> String
headType = type' <<< head

headText :: forall a. Cofree a SyntaxNode -> String
headText = text <<< head

index :: SyntaxNode -> Tuple Int Int
index n = Tuple (startIndex n) (endIndex n)

is :: forall a. Eq a => a -> a -> Boolean
is a = (_ == a)

headIs :: forall a. String -> Cofree a SyntaxNode -> Boolean
headIs a = is a <<< headType

type SimpleTree = Cofree Array
    { type :: String
    , info :: { start :: Int, end :: Int }
    , text :: Maybe String
    }

spec :: Spec Unit
spec = describe "System" do
    it "extracts identifiers form a file" do
        let
            words = tree
                # fromFoldable
                # filter (is "word" <<< type')
                # map text
        words `shouldEqual`
            [ "function_name"
            , "cat"
            , "other_function_name"
            , "function_name"
            ]

    it "extracts function names from file" do
        let
            function_names = tree
                # universe
                # fromFoldable
                # filter (headIs "function_definition")
                # foldMap tail
                # filter (headIs "word")
                # map headText
        function_names `shouldEqual`
            [ "function_name"
            , "other_function_name"
            ]
    it "rename function names in file" do
        let
            ranges = tree
                # fromFoldable
                # filter (type' >>> is "word")
                # filter (text >>> is "function_name")
                # map index
            replace string (Tuple start end) p =
                String.take start p <> string <> String.drop end p
            program' = foldr (replace "new_function_name") program ranges
        program' `shouldEqual`
            """
new_function_name () {
    cat "my_file"
}
other_function_name () {
    new_function_name
}
"""
    itOnly "can create stack graphs" do
        let
            transform n =
                let
                    t = type' n
                in
                    { type: t
                    , info: { start: startIndex n, end: endIndex n }
                    , text:
                          if elem t [ "simple_identifier", "type_identifier" ] then
                              Just (text n)
                          else Nothing
                    }
            structure = transform <$> swiftTree

            childrenOfType :: String -> SimpleTree -> Array SimpleTree
            childrenOfType t node = filter (head >>> _.type >>> is t)
                (tail node)
            childOfType :: String -> SimpleTree -> Maybe SimpleTree
            childOfType t node = find (head >>> _.type >>> is t)
                (tail node)

            functionDeclaration :: SimpleTree -> Maybe (CreateGraph  Int)
            functionDeclaration t = do
                identifier <- head <$> childOfType "simple_identifier" t
                pure $ declare (fromMaybe "" identifier.text) identifier.info

            classDeclaration :: SimpleTree -> Maybe (CreateGraph Int)
            classDeclaration t = do
                identifier <- head <$> childOfType "type_identifier" t
                classBody <- childOfType "class_body" t
                className <- identifier.text
                let methods = childrenOfType "function_declaration" classBody
                        # mapMaybe functionDeclaration
                pure $ namedScope className methods

            namedScope :: String -> Array (CreateGraph Int) -> CreateGraph Int
            namedScope name nodes = demand name =<< scope =<< sequence nodes
            newSourceFile :: SimpleTree -> CreateGraph Unit
            newSourceFile sourceTree = do
                ids <- (tail sourceTree)
                    # sequence <<< mapMaybe (\ subtree ->
                        case head subtree of
                            {type: "function_declaration" } ->
                                functionDeclaration subtree
                            { type: "class_declaration" } ->
                                classDeclaration subtree
                            _ -> Nothing
                    )
                file <- scope (reverse ids)
                void $ supply "hello" file
            graph = createGraph_ $ newSourceFile structure
        toUnfoldable graph `shouldEqual`
            [ (Tuple 0 (Info { end: 11, start: 6 }))
            , (Tuple 1 (Pop "hello" 0))
            , (Tuple 2 (Info { end: 69, start: 64 }))
            , (Tuple 3 (Pop "hello" 2))
            , (Tuple 4 (Branch [ 3 ]))
            , (Tuple 5 (Pop "SomeClass" 4))
            , (Tuple 6 (Branch [ 5, 1 ]))
            , (Tuple 7 (Push "hello" 6))
            ]
    it "can find class that defines method" do
        let
            zipper = fromCofree swiftTree

            nodeType :: Zipper Array SyntaxNode -> String
            nodeType = extract >>> type'
            nodeIsFunction = nodeType >>> is "function_declaration"
            nodeIsClass = nodeType >>> is "class_declaration"

            methods :: Array (Zipper Array SyntaxNode)
            methods = duplicate zipper
                # fromFoldable
                # filter nodeIsFunction
                # filter
                      ((goUp >=> goUp) >>> map nodeIsClass >>> is (Just true))
        map (extract >>> type') methods `shouldEqual` [ "function_declaration" ]
