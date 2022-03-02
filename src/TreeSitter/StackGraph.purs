module TreeSitter.StackGraph where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List(..), uncons, (:))
import Data.Map (Map, lookup)
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
