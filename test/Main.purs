module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import TreeSitter.Lazy (children, mkParser, namedChildren, parseString, rootNode)
import TreeSitter.Declarative (parse)

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
