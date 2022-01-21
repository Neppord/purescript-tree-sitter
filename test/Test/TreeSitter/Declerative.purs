module Test.TreeSitter.Declerative where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "SyntaxTree" do
    it "true" do
        true `shouldEqual` true