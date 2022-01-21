module Test.TreeSitter.Declerative where

import Prelude

import Data.List (List(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TreeSitter.Declarative (children, mkSyntaxTree, node)

spec :: Spec Unit
spec = describe "SyntaxTree" do
    it "can be constructed and deconstructed" do
        let tree = mkSyntaxTree 1 Nil
        node tree `shouldEqual` 1
        children tree `shouldEqual` Nil