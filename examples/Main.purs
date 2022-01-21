module Examples.Main where

import Prelude

import Effect (Effect)
import TreeSitter.Declarative (parse)
import Effect.Console (log, logShow)

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
  parse "swift" swiftSource
    # (map \ n -> {type: n.type, start: n.range.startIndex, end: n.range.endIndex})
    # logShow