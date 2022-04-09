module Test.TreeSitter.Codegen where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TreeSitter.Codegen (toProper)

spec :: Spec Unit
spec = describe "System" do
    describe "toProper" do
        it "Capitalize the name" do
            toProper "hello" `shouldEqual` "Hello"
        it "Capitalize each _ seperated word" do
            toProper "hello_world" `shouldEqual` "HelloWorld"