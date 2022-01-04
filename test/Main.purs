module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import TreeSitter.Declarative (parse)
import Effect.Console (log)

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
  logShow (parse "bash" bashSource)
  log "====="
  log "Swift"
  log swiftSource
  logShow (parse "swift" swiftSource)