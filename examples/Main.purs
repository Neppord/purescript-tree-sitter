module Examples.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import TreeSitter.CST (displayGroups, displayGroups_, sexpression)
import TreeSitter.Declarative (Named(..), parseCST)

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
  log "show"
  parseCST "swift" swiftSource
    # logShow
  log "displayGroups"
  parseCST "swift" swiftSource
    # displayGroups
    # log
  log "displayGroups_"
  parseCST "swift" swiftSource
    # displayGroups_ show (\ n -> if n.named == Named then n.type else "")
    # log
  log "sexpression"
  parseCST "swift" swiftSource
    # sexpression (\ n -> if n.named == Named then n.type else "")
    # log