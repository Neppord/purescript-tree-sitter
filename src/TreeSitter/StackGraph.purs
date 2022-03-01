module TreeSitter.StackGraph where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List(..), uncons, (:))
import Data.Map (Map, empty, lookup)
import Data.Map.Internal (insert)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

type Info = { start :: Int, end :: Int }

data Node next
    = Push String next
    | Pop String next
    | Info Info
    | Branch (Array next)

derive instance Generic (Node a) _

instance (Show a) => Show (Node a) where
    show = genericShow

type Graph a = Map a (Node a)

-- | this example is for the code below
-- | ```swift
-- | let h = "hello"
-- | let w = "world"
-- | print(h + " " + w)
-- | ```
example :: Graph Int
example = empty
    # insert 0 (Info { start: 4, end: 5 })
    # insert 1 (Pop "h" 0)
    # insert 2 (Branch [ 1 ])
    # insert 3 (Info { start: 20, end: 21 })
    # insert 4 (Pop "w" 3)
    # insert 5 (Branch [ 4, 2 ])
    # insert 6 (Push "h" 5)
    # insert 8 (Push "w" 5)

findDefinition :: Graph Int -> Int -> Array Info
findDefinition = go Nil
    where
    go stack graph start = case lookup start graph of
        Nothing -> []
        Just (Info info) -> [ info ]
        Just (Push p next) -> go (p : stack) graph next
        Just (Pop p next) -> case uncons stack of
            Just { head, tail } | p == head -> go tail graph next
            _ -> []
        Just (Branch branches) -> do
            branch <- branches
            go stack graph branch
