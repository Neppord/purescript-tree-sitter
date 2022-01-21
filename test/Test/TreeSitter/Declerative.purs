module Test.TreeSitter.Declerative where

import Prelude

import Data.Lens (over, traversed)
import Data.List (List(..), (:))
import Data.Traversable (traverse)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TreeSitter.Declarative (children, mkSyntaxTree, node)
import TreeSitter.Plated (rewrite)
import Data.Maybe (Maybe(..))

spec :: Spec Unit
spec = describe "SyntaxTree" do
    it "can be constructed and deconstructed" do
        let tree = mkSyntaxTree 1 Nil
        node tree `shouldEqual` 1
        children tree `shouldEqual` Nil
    it "is traversable" do
        let
            tree = mkSyntaxTree 1 Nil
            trees = traverse (_ : Nil) tree
        trees `shouldEqual` (mkSyntaxTree 1 Nil : Nil)
    it "can alter nodes with the help of lenses" do
        let tree = mkSyntaxTree 1 Nil
        over traversed (_ + 1) tree `shouldEqual` mkSyntaxTree 2 Nil
    it "can be rewriten with plating" do
        let
            tree = mkSyntaxTree 1 Nil
            go tree' | node tree' == 1 = Just $ mkSyntaxTree 2 $ children tree'
            go _ = Nothing
        rewrite go tree `shouldEqual` mkSyntaxTree 2 Nil


