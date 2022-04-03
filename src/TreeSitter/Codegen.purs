module TreeSitter.Codegen where

import Prelude

import Control.Monad.Writer (tell)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Declaration, Module)
import PureScript.CST.Types as CST
import Tidy.Codegen (declNewtype, typeApp, typeCtor, typeRecord, typeRow)
import Tidy.Codegen.Monad (codegenModule, importOpen)
import TreeSitter.Codegen.NodeTypes (ChildType, NodeType)

toProper :: String -> String
toProper = ("N" <> _)

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
renderVariantNewType { type: type', fields, children } =
    declNewtype name [] name record
    where
    name = toProper type'
    record = typeRecord (children' <> fields') Nothing
    fields' = renderVariantFields <$> fromMaybe fields
    children' = renderVariantChildren <$> fromMaybe children

renderVariantModule :: String -> Array NodeType -> Module Void
renderVariantModule name nodeTypes = unsafePartial $ codegenModule name do
    importOpen "Data.Variant"
    importOpen "Data.Maybe"
    importOpen "Data.Array"
    nodeTypes
        # Array.filter _.named
        # map renderVariantNewType
        # tell
