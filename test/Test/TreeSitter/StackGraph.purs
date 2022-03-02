module Test.TreeSitter.StackGraph where

import Prelude

import Data.Map (empty, insert)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TreeSitter.StackGraph (Graph, Node(..), findDefinition)

-- | this example is for the code below
-- | ```swift
-- | let h = "hello"
-- | let w = "world"
-- | print(h + " " + w)
-- | ```
example :: Graph Int
example = empty
    # insert 0 (Info { start: 4, end: 5 })
    # insert 1 (Pop "h" 0)
    # insert 2 (Branch [ 1 ])
    # insert 3 (Info { start: 20, end: 21 })
    # insert 4 (Pop "w" 3)
    # insert 5 (Branch [ 4, 2 ])
    # insert 6 (Push "h" 5)
    # insert 8 (Push "w" 5)

spec :: Spec Unit
spec = describe "StackGraph" do
    it "finds definition" do
        findDefinition example 8 `shouldEqual` [{ start: 20, end: 21 }]