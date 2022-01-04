module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import TreeSitter.Declarative (parse)
import Effect.Console (log)
import TreeSitter.Tree (showTree)

bashSource :: String
bashSource = """
do_stuff () {
    cat file | grep "foo"
}

do_stuff
"""
swiftSource :: String
swiftSource = """
print("Hello, world!")
// Prints "Hello, world!"
"""
main :: Effect Unit
main = do
  log "Bash"
  log bashSource
  log $ showTree (parse "bash" bashSource)
  log "====="
  log "Swift"
  log swiftSource
  log $ showTree (parse "swift" swiftSource)