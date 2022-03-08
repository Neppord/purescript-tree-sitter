module Test.TreeSitter.StackGraph.Swift where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Map (Map, keys)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain)
import Test.TreeSitter.StackGraph.Swift.Source (FileSystem, getFile, projectTennis)
import TreeSitter.Lazy (SyntaxNode, parse)
import TreeSitter.StackGraph (Graph, Info, createGraph_)
import TreeSitter.StackGraph.Swift (sourceFile)

trees :: FileSystem (Cofree Array SyntaxNode)
trees = parse "swift" <$> projectTennis

graphs :: FileSystem (Tuple (Map Info Int) (Graph Int))
graphs = (createGraph_ <<< sourceFile) <$> trees

spec :: Spec Unit
spec = describe "TreeSitter.StackGraph.Swift" do
    describe "finds all variables in tennis 1" do
        let
            path = [ "Swift", "Tennis", "TennisGame1.swift" ]
            file = getFile path graphs
            (Tuple index graph) = unsafePartial $ fromJust file
        it "finds score1 usage" do
            keys index `shouldContain` { start: 474, end: 474 + 6 }
