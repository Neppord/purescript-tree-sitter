module Examples.Main where

import Prelude

import Control.Alternative (guard)
import Data.Array ((!!))
import Data.Either (Either(..), hush)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import TreeSitter.CST (displayGroups, displayGroups_, tail, (:<))
import TreeSitter.Declarative (DeclarativeCST, Named(..), parseCST, sExpression, text, type')
import Data.Lens.Plated (rewrite)

bashSource :: String
bashSource = """
do_stuff () {
    cat file | grep "foo"
}

do_stuff
"""
swiftSource :: String
swiftSource = """
let a = 1 + 2 + 3
print("Hello, world!")
// Prints "Hello, world!"
"""

collapseAdditions :: DeclarativeCST -> Maybe DeclarativeCST
collapseAdditions cst = do
    guard $ type' cst == "additive_expression"
    children <- hush $ tail cst
    x <- children !! 0
    y <- children !! 2
    guard $ type' x == "integer_literal"
    guard $ type' y == "integer_literal"
    xInt <- fromString $ text x
    yInt <- fromString $ text y
    Just $ {type: "integer_literal", named: Named} :< Left (show (xInt + yInt))

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
  log "s-expression"
  parseCST "swift" swiftSource
    # sExpression
    # log
  log "collapseAdditions"
  parseCST "swift" swiftSource
    # rewrite collapseAdditions
    # text
    # log