module TreeSitter.Codegen where

import Prelude

import Control.Monad.Writer (tell)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (DataCtor, Declaration, Module)
import PureScript.CST.Types as CST
import Tidy.Codegen (dataCtor, declData, declNewtype, typeApp, typeCtor, typeRecord, typeRow)
import Tidy.Codegen.Monad (codegenModule)
import TreeSitter.Codegen.NodeTypes (ChildType, NodeType)

toProper :: String -> String
toProper = ("N" <> _)

renderFields :: Partial => Object ChildType -> Array (CST.Type Void)
renderFields object = [ typeRecord (Object.foldMap go object) Nothing ]
    where
    go field childType =
        [ ( Tuple field $ typeCtor
                ( childType.types # Array.filter _.named # map _.type
                      # Array.intercalate "__"
                      # toProper
                )
          )
        ]

renderData :: Partial => NodeType -> Declaration Void
renderData { type: type', fields } = declData properName []
    [ dataCtor properName $ maybe [] renderFields fields ]
    where
    properName = toProper type'

renderCtor :: Partial => NodeType -> DataCtor Void
renderCtor { type: type', fields } = dataCtor (toProper type')
    [ typeRecord
          [ Tuple "fields" $ typeRecord fields' Nothing
          , Tuple "children" $ typeApp (typeCtor "Array") [ typeCtor "Node" ]
          ]
          Nothing
    ]
    where
    fields' = maybe
        ([] :: Array (Tuple String (CST.Type Void)))
        (Object.foldMap \key _ -> [ Tuple key (typeCtor "Node") ])
        fields

render :: String -> Array NodeType -> Module Void
render name nodeTypes = unsafePartial $ codegenModule name do
    let named = nodeTypes # Array.filter _.named
    tell $ [ declData "Node" [] $ map renderCtor named ]

renderVariantFields
    :: Partial => Object ChildType -> Tuple String (CST.Type Void)
renderVariantFields fields = Tuple "fields" value
    where
    value = typeRecord rows Nothing

    rows :: Array (Tuple String (CST.Type Void))
    rows = Object.foldMap
        (\name childType -> [ Tuple name (renderVariantChildType childType) ])
        fields

renderVariantChildType :: Partial => ChildType -> CST.Type Void
renderVariantChildType { types, multiple, required } = case multiple of
    false -> case required of
        Just true -> variant
        _ -> typeApp (typeCtor "Maybe") [ variant ]
    true -> typeApp (typeCtor "Array") [ variant ]
    where
    variant = renderVariantType types

renderVariantChildren :: Partial => ChildType -> Tuple String (CST.Type Void)
renderVariantChildren childType = Tuple name $ renderVariantChildType childType
    where
    name = case childType.multiple of
        false -> "child"
        true -> "children"

renderVariantType
    :: Partial => Array { type :: String, named :: Boolean } -> CST.Type Void
renderVariantType types = node
    where
    node = typeApp (typeCtor "Variant") [ typeRow rows Nothing ]
    rows = types
        # Array.filter _.named
        # map row
    row { type: type' } = Tuple type' $ typeCtor $ toProper type'

renderVariantNewType :: Partial => NodeType -> Declaration Void
renderVariantNewType { type: type', fields, children } = declNewtype name []
    name
    record
    where
    name = toProper type'
    record = typeRecord (children' <> fields') Nothing
    fields' = renderVariantFields <$> fromMaybe fields
    children' = renderVariantChildren <$> fromMaybe children

renderVariantModule :: String -> Array NodeType -> Module Void
renderVariantModule name nodeTypes = unsafePartial $ codegenModule name do
    nodeTypes
        # Array.filter _.named
        # map renderVariantNewType
        # tell
