module Test.TreeSitter.System where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree, head, tail)
import Data.Array (filter, foldMap, foldr, fromFoldable)
import Data.Lens.Plated (universe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TreeSitter.Lazy (SyntaxNode, children, endIndex, mkParser, parseString, rootNode, startIndex, text, type')

program :: String
program = """
function_name () {
    cat "my_file"
}
other_function_name () {
    function_name
}
"""
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

spec :: Spec Unit
spec = describe "System" do
    it "extracts identifiers form a file" do
        let words = tree
                # fromFoldable
                # filter (is "word" <<< type')
                # map text
        words `shouldEqual`
            ["function_name"
            , "cat"
            , "other_function_name"
            , "function_name"
            ]

    it "extracts function names from file" do
        let function_names = tree
                # universe
                # fromFoldable
                # filter (headIs "function_definition")
                # foldMap tail
                # filter (headIs "word")
                # map headText
        function_names `shouldEqual`
            ["function_name"
            , "other_function_name"
            ]
    it "rename function names in file" do
        let ranges = tree
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