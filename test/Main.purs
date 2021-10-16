module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import TreeSitter (children, mkParser, namedChildren, parseString, rootNode)

bashSource :: String
bashSource = """
do_stuff () {
    cat file | grep "foo"
}

do_stuff
"""
main :: Effect Unit
main = do
  let bashParser = mkParser "bash"
  let tree = parseString bashParser bashSource
  logShow tree
  tree
    # rootNode
    # children
    # map namedChildren
    # logShow
