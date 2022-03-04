module Test.TreeSitter.System where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, buildCofree, head, tail, (:<))
import Control.Comonad.Cofree.Zipper (Zipper, fromCofree, goUp)
import Control.Extend (duplicate)
import Data.Array (elem, filter, foldMap, foldr, fromFoldable, (!!))
import Data.Lens.Plated (universe)
import Data.Map.Internal (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TreeSitter.Lazy (SyntaxNode, children, endIndex, mkParser, parseString, rootNode, startIndex, text, type')
import TreeSitter.StackGraph (CreateGraph, createGraph_, declare, demand, scope)
import Data.Array (find)
import Data.Maybe (fromMaybe)
import Data.Traversable (sequence)
import Data.Array (concat)
import TreeSitter.StackGraph (supply)
import TreeSitter.StackGraph (Node(..))

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
    it "can create stack graphs" do
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

            functionDeclaration :: SimpleTree -> CreateGraph (Array Int)
            functionDeclaration tree = sequence do
                identifier <- head <$> childrenOfType "simple_identifier" tree
                pure $ declare (fromMaybe "" identifier.text) identifier.info

            classDeclaration :: SimpleTree -> CreateGraph (Array Int)
            classDeclaration tree = sequence do
                identifier <- head <$> childrenOfType "type_identifier" tree
                classBody <- childrenOfType "class_body" tree
                pure $ do
                    (ids :: Array Int) <-
                        childrenOfType "function_declaration" classBody
                            # map functionDeclaration
                            # sequence
                            # map concat
                    id <- scope ids
                    demand (fromMaybe "" identifier.text) id

            callExpression :: SimpleTree -> CreateGraph (Array Int)
            callExpression _ = pure []

            sourceFile :: SimpleTree -> CreateGraph (Array Int)
            sourceFile t = case head t of
                { type: "function_declaration" } -> functionDeclaration t
                { type: "class_declaration" } -> classDeclaration t
                { type: "call_expression" } -> callExpression t
                _ -> pure []

            namedScope :: String -> Array (CreateGraph Int) -> CreateGraph Int
            namedScope name nodes = demand name =<< scope =<< sequence nodes

            newSourceFile :: SimpleTree -> CreateGraph (Array Int)
            newSourceFile sourceTree = do
                hello <- declare "hello" { end: 11, start: 6 }
                someClass <- namedScope "SomeClass" do
                    pure $ declare "hello" { end: 69, start: 64 }
                file <- scope [ someClass, hello ]
                call <- supply "hello" file
                pure [ call ]

            go :: CreateGraph Int -> SimpleTree -> CreateGraph Int
            go s tree = scope []
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
