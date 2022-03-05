module TreeSitter.StackGraph where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Writer (WriterT, tell)
import Control.Monad.Writer.Trans (execWriterT)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), uncons, (:))
import Data.Map (Map, SemigroupMap(..), lookup)
import Data.Map.Internal (empty, singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup.Last (Last(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Bifunctor (bimap)

type Info = { start :: Int, end :: Int }

data Node next
    = Push String next
    | Pop String next
    | Info Info
    | Branch (Array next)

derive instance Generic (Node a) _
derive instance (Eq a) => Eq (Node a)

instance (Show a) => Show (Node a) where
    show = genericShow

type Graph a = Map a (Node a)

data CreateGraphF a
    = NewNode (Int -> a)
    | Connect Int (Node Int) a
    | EntryPoint Info Int a

derive instance Functor CreateGraphF

type CreateGraph = Free CreateGraphF

type Interp = WriterT
    ( Tuple
          (SemigroupMap Info (Last Int))
          (SemigroupMap Int (Last (Node Int)))
    )
    (State Int)

createGraph_ :: forall a. CreateGraph a -> Tuple (Map Info Int) (Graph Int)
createGraph_ cg = evalState (execWriterT (foldFree runCreateGraph cg)) 0
    # bimap unwrap unwrap
    # bimap (map unwrap) (map unwrap)

runCreateGraph :: forall a. CreateGraphF a -> Interp a
runCreateGraph (NewNode k) = do
    n <- get
    put $ n + 1
    pure $ k n
runCreateGraph (Connect id node a) = do
    tell $ Tuple
        (SemigroupMap $ empty)
        (SemigroupMap $ singleton id (Last node))
    pure a
runCreateGraph (EntryPoint info index a) = do
    tell $ Tuple
        (SemigroupMap $ singleton info (Last index))
        (SemigroupMap $ empty)
    pure a

newId :: Free CreateGraphF Int
newId = liftF $ NewNode identity

connect :: Node Int -> Int -> Free CreateGraphF Int
connect node id = liftF $ Connect id node id

entryPoint :: Info -> Int -> Free CreateGraphF Int
entryPoint info id = liftF $ EntryPoint info id id

info :: { end :: Int, start :: Int } -> Free CreateGraphF Int
info i = newId >>= connect (Info i)

demand :: String -> Int -> Free CreateGraphF Int
demand i next = newId >>= connect (Pop i next)

scope :: Array Int -> Free CreateGraphF Int
scope nexts = newId >>= connect (Branch nexts)

supply :: String -> Int -> Free CreateGraphF Int
supply i next = newId >>= connect (Push i next)

usage :: String -> Info -> Int -> Free CreateGraphF Int
usage name info id = supply name id >>= entryPoint info

declare :: String -> { end :: Int, start :: Int } -> Free CreateGraphF Int
declare var pos = info pos >>= demand var

namedScope :: String -> Array (CreateGraph Int) -> CreateGraph Int
namedScope name nodes = demand name =<< scope =<< sequence nodes

findDefinition :: Graph Int -> Int -> Array Info
findDefinition = go Nil
    where
    go stack graph start = case lookup start graph of
        Nothing -> []
        Just (Info i) -> [ i ]
        Just (Push p next) -> go (p : stack) graph next
        Just (Pop p next) -> case uncons stack of
            Just { head, tail } | p == head -> go tail graph next
            _ -> []
        Just (Branch branches) -> do
            branch <- branches
            go stack graph branch
