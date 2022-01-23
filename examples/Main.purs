module Examples.Main where

import Prelude

import Effect (Effect)
import TreeSitter.Declarative (parseAnnotations)
import Effect.Console (log, logShow)
import TreeSitter.Declarative (parseCST)

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
  logShow (parseAnnotations "bash" bashSource)
  log "====="
  log "Swift"
  log swiftSource
  parseAnnotations "swift" swiftSource
    # (map \ n -> {type: n.type, start: n.range.startIndex, end: n.range.endIndex})
    # logShow
  parseCST "swift" swiftSource
    # logShow