module Test.TreeSitter.StackGraph.Swift where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree)
import Data.Map.Internal (Map)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe)
import Test.TreeSitter.StackGraph.Swift.Source (FileSystem, project)
import TreeSitter.Lazy (SyntaxNode, children, mkParser, parseString, rootNode)
import TreeSitter.StackGraph (Graph, Info, createGraph_)
import TreeSitter.StackGraph.Swift (sourceFile)

parse :: String -> Cofree Array SyntaxNode
parse text = text
    # parseString (mkParser "swift")
    # rootNode
    # buildCofree (\s -> Tuple s $ children s)

trees :: FileSystem (Cofree Array SyntaxNode)
trees = parse <$> project

graphs :: FileSystem (Tuple (Map Info Int) (Graph Int))
graphs = (createGraph_ <<< sourceFile) <$> trees

spec :: Spec Unit
spec = describe "TreeSitter.StackGraph.Swift" do
    pure unit
