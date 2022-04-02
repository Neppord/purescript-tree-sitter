module TreeSitter.Codegen where

import Prelude

import Control.Monad.Writer (tell)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Foreign (F)
import Foreign.Generic (decodeJSON)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (DataCtor, Declaration, Module)
import PureScript.CST.Types as CST
import Tidy.Codegen (dataCtor, declData, typeApp, typeCtor, typeRecord, typeVar)
import Tidy.Codegen.Monad (codegenModule)
import Tidy.Codegen (declNewtype)
import Data.Unfoldable (fromMaybe)

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

toProper :: String -> String
toProper = ("N" <> _)

renderFields :: Partial => Object ChildType -> Array (CST.Type Void)
renderFields object = [ typeRecord (Object.foldMap go object) Nothing ]
  where
  go field childType =
    [ (Tuple field $ typeCtor (childType.types # Array.filter _.named # map _.type # Array.intercalate "__" # toProper)) ]

renderData :: Partial => NodeType -> Declaration Void
renderData { type: type', fields } = declData properName [] [ dataCtor properName $ maybe [] renderFields fields ]
  where
  properName = toProper type'

renderCtor :: Partial => NodeType -> DataCtor Void
renderCtor {type: type' , fields} = dataCtor (toProper type')
    [ typeRecord
        [ Tuple "fields" $ typeRecord fields' Nothing
        , Tuple "children" $ typeApp (typeCtor "Array") [typeCtor "Node"]
        ]
        Nothing
    ] where
    fields' = maybe
        ([] :: Array (Tuple String (CST.Type Void)))
        (Object.foldMap \ key _ -> [Tuple key (typeCtor "Node") ])
        fields

render :: String -> Array NodeType -> Module Void
render name nodeTypes = unsafePartial $ codegenModule name do
  let named = nodeTypes # Array.filter _.named
  tell $ [declData "Node" [] $ map renderCtor named]


renderVariantFields :: Object ChildType -> Tuple String (CST.Type Void)
renderVariantFields fields = Tuple "fileds" value where
    value = (typeRecord ([] :: Array (Tuple String (CST.Type Void))) Nothing)

renderVariantChildren :: Partial => ChildType -> Tuple String (CST.Type Void)
renderVariantChildren children = Tuple "children" value where
    value = (typeApp (typeCtor "Array") [typeCtor "Node"])

renderVariantNewType :: Partial => NodeType -> Declaration Void
renderVariantNewType {type: type', fields, children} = declNewtype name [] name record where
    name = toProper type'
    record = typeRecord ( children' <> fields' ) Nothing
    fields' = renderVariantFields <$> fromMaybe fields
    children' = renderVariantChildren <$> fromMaybe children

renderVariant :: String -> Array NodeType -> Module Void
renderVariant name nodeTypes = unsafePartial $ codegenModule name do
  nodeTypes
    # Array.filter _.named
    # map renderVariantNewType
    # tell
