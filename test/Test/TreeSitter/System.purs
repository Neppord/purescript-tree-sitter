module Test.TreeSitter.System where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, buildCofree, head, tail)
import Control.Comonad.Cofree.Zipper (Zipper, fromCofree, goUp)
import Control.Extend (duplicate)
import Data.Array (filter, foldMap, foldr, fromFoldable)
import Data.Lens.Plated (universe)
import Data.Map (keys, lookup)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual)
import TreeSitter.Lazy (SyntaxNode, children, endIndex, mkParser, parseString, rootNode, startIndex, text, type')
import TreeSitter.StackGraph (createGraph_, findDefinition)
import TreeSitter.StackGraph.Swift (sourceFile)

swiftProgram :: String
swiftProgram =
    """\
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

is :: forall a. Eq a => a -> a -> Boolean
is a = (_ == a)

spec :: Spec Unit
spec = describe "System" do
    it "can create stack graphs" do
        let
            (Tuple index graph) = createGraph_ $ sourceFile swiftTree
            entryPoints = keys index
            entryPoint = { start: 106, end: 111 }
            id = lookup entryPoint index
        entryPoints `shouldContain` entryPoint
        (findDefinition graph <$> id) `shouldEqual` Just
            [ { end: 12, start: 7 } ]
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
