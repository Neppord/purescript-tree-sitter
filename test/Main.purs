module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import TreeSitter (mkParser, parseString)

main :: Effect Unit
main = do
  let bashParser = mkParser "bash"
  let tree = parseString bashParser "cat file | grep 'foo'"
  logShow tree
