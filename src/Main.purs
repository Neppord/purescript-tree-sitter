module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Tidy.Codegen (printModule)
import TreeSitter.Codegen (renderVariantModule)
import TreeSitter.Codegen.NodeTypes (parse)

main :: Effect Unit
main = do
    json <- readTextFile UTF8
        "node_modules/tree-sitter-swift/src/node-types.json"
    case runExcept (parse json) of
        Right a -> do
            let
                cst = renderVariantModule "TreeSitteer.Codegen.Swift" a
                source = printModule cst
            writeTextFile UTF8 "src/TreeSitter/Codegen/Swift.purs" source
        Left err -> logShow err
