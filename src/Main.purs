module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Tidy.Codegen (printModule)
import TreeSitter.Codegen (renderVariantModule)
import TreeSitter.Codegen.NodeTypes (parse)

main :: Effect Unit
main = do
    json <- readTextFile UTF8
        "node_modules/tree-sitter-swift/src/node-types.json"
    case runExcept (parse json) of
        Right a -> log $ printModule $ renderVariantModule "TreeSitteer.Codegen.Swift"
            a
        Left err -> logShow err
