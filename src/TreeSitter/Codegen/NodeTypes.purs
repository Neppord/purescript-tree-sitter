module TreeSitter.Codegen.NodeTypes where

import Prelude

import Data.Maybe (Maybe)
import Foreign (F)
import Foreign.Generic (decodeJSON)
import Foreign.Object (Object)

type ChildType =
    { types ::
          Array
              { type :: String
              , named :: Boolean
              }
    , multiple :: Boolean
    , required :: Maybe Boolean
    }

type NodeType =
    { type :: String
    , named :: Boolean
    , fields :: Maybe (Object ChildType)
    , children :: Maybe ChildType
    }

parse :: String -> F (Array NodeType)
parse = decodeJSON