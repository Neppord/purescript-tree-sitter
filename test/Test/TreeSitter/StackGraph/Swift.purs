module Test.TreeSitter.StackGraph.Swift where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Map.Internal (Map)
import Data.Tuple (Tuple)
import Test.Spec (Spec, describe)
import Test.TreeSitter.StackGraph.Swift.Source (FileSystem, projectTennis)
import TreeSitter.Lazy (SyntaxNode, parse)
import TreeSitter.StackGraph (Graph, Info, createGraph_)
import TreeSitter.StackGraph.Swift (sourceFile)

trees :: FileSystem (Cofree Array SyntaxNode)
trees = parse "swift" <$> projectTennis

graphs :: FileSystem (Tuple (Map Info Int) (Graph Int))
graphs = (createGraph_ <<< sourceFile) <$> trees

spec :: Spec Unit
spec = describe "TreeSitter.StackGraph.Swift" do
    pure unit
