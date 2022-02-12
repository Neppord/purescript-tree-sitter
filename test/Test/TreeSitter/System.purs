module Test.TreeSitter.System where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree)
import Data.Array (filter, fromFoldable)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TreeSitter.Lazy (SyntaxNode, children, mkParser, parseString, rootNode, text, type')
import Data.Lens.Plated (rewrite)
import Data.Maybe (Maybe(..))
import Control.Comonad.Cofree (head)
import Data.Lens.Plated (universe)
import Data.Array (foldMap)
import Control.Comonad.Cofree (tail)

program :: String
program = """
function_name () {
    cat "my_file"
}
other_function_name () {
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
