module Main where

import Prelude

import Data.Array (concat, elem, filter, fromFoldable, (!!))
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (readTextFile, readdir, stat)
import Node.Process (argv)
import TreeSitter.Lazy (parse, text, type')

isDir :: String -> Effect Boolean
isDir filePath = isDirectory <$> stat filePath

allFiles :: String -> Effect (Array String)
allFiles filePath = do
    dir <- isDir filePath
    if dir then do
        files <- readdir filePath
        let
            filePaths = files
                # map (\p -> filePath <> "/" <> p)
        arrayOfPaths <- for filePaths allFiles
        pure $ concat arrayOfPaths
    else pure [ filePath ]

main :: Effect Unit
main = void do
    args <- argv
    let
        baseDir = args !! 1
            # fromMaybe "."
    files <- allFiles baseDir
    let
        pythonFiles = files
            # filter (contains (Pattern ".py"))
        pythonParser = parse "Python"
    for_ pythonFiles $ \path -> do
        source <- readTextFile UTF8 path
        let
            tree = pythonParser source
            nodes = fromFoldable tree
            importTypes =
                [ "import_statement"
                , "future_import_statement"
                , "import_from_statement"
                ]
            imports = nodes
                # filter (\n -> type' n `elem` importTypes)
        for_ imports (text >>> log)