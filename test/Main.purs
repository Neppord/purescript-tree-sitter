module Test.Main where

import Prelude
import TreeSitter

import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = do
  let bashParser = mkParser "bash"
  let tree = parseString bashParser "cat file | grep 'foo'"
  logShow tree
  tree
    # rootNode
    # children
    # map namedChildren
    # logShow
