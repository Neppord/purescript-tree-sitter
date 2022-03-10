module Test.TreeSitter.StackGraph where

import Prelude

import Data.Map (empty, insert, lookup)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TreeSitter.StackGraph (CreateGraph, Graph, Node(..), createGraph_, declare, findDefinition, scope, usage)
import Data.Maybe (Maybe(..))
import TreeSitter.StackGraph (usage_)


example :: Graph Int
example = empty
    # insert 0 (Info { start: 4, end: 5 })
    # insert 1 (Pop "h" 0)
    # insert 2 (Branch [ 1 ])
    # insert 3 (Info { start: 20, end: 21 })
    # insert 4 (Pop "w" 3)
    # insert 5 (Branch [ 4, 2 ])
    # insert 6 (Push "h" 5)
    # insert 7 (Push "w" 5)

-- | this example is for the code below
-- | ```swift
-- | let h = "hello"
-- | let w = "world"
-- | print(h + " " + w)
-- | ```
monadExample :: CreateGraph Unit
monadExample = do
    id1 <- declare "h" { start: 4, end: 5 }
    id2 <- scope [ id1 ]
    id4 <- declare "w" { start: 20, end: 21 }
    id5 <- scope [ id4, id2 ]
    usage_ "h" { start: 38, end: 39 } id5
    usage_ "w" { start: 48, end: 49 } id5

spec :: Spec Unit
spec = describe "StackGraph" do
    it "finds definition" do
        findDefinition example 7 `shouldEqual` [ { start: 20, end: 21 } ]
    it "has a monad that creates correct graphs" do
        let (Tuple index graph) = createGraph_ monadExample
        graph `shouldEqual` example
        (findDefinition graph <$> (lookup { start: 48, end: 49 } index))
            `shouldEqual` Just [ { start: 20, end: 21 } ]