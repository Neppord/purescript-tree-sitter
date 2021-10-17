module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import TreeSitter.Laizy (children, mkParser, namedChildren, parseString, rootNode)
import TreeSitter.Declerative (parse)

bashSource :: String
bashSource = """
do_stuff () {
    cat file | grep "foo"
}

do_stuff
"""
main :: Effect Unit
main = do
  let tree = parse "bash" bashSource
  logShow tree
