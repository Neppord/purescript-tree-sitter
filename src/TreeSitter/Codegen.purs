module TreeSitter.Codegen where

import Prelude

import Control.Monad.Writer (tell)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Declaration, Module)
import PureScript.CST.Types as CST
import Tidy.Codegen (declDerive, declNewtype, typeApp, typeCtor, typeRecord, typeRow, typeVar)
import Tidy.Codegen.Monad (codegenModule, importOpen)
import TreeSitter.Codegen.NodeTypes (ChildType, NodeType)

capitalize :: String -> String
capitalize word =
    String.toUpper (String.take 1 word) <> String.drop 1 word

toProper :: String -> String
toProper name = String.split (String.Pattern "_") name
    # map capitalize
    # String.joinWith ""

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
    node = typeApp (typeCtor "VariantF") [ typeRow rows Nothing, typeVar "a" ]
    rows = types
        # Array.filter _.named
        # map row
    row { type: type' } = Tuple type' $ typeCtor $ toProper type'

renderVariantNewType :: Partial => NodeType -> Declaration Void
renderVariantNewType { type: type', fields, children } =
    declNewtype name [ typeVar "a" ] name record
    where
    name = toProper type'
    record = typeRecord (value <> children' <> fields') Nothing
    value = [ Tuple "value" (typeVar "a") ]
    fields' = renderVariantFields <$> fromMaybe fields
    children' = renderVariantChildren <$> fromMaybe children

renderFunctorDerivation :: Partial => NodeType -> Declaration Void
renderFunctorDerivation { type: type' } =
    declDerive Nothing [] "Functor" [ typeCtor (toProper type') ]

renderVariantModule :: String -> Array NodeType -> Module Void
renderVariantModule name nodeTypes = unsafePartial $ codegenModule name do
    importOpen "Prelude"
    importOpen "Data.Functor.Variant"
    importOpen "Data.Maybe"
    importOpen "Data.Array"
    for_ nodeTypes \node -> do
        if node.named then
            tell
                [ renderVariantNewType node
                , renderFunctorDerivation node
                ]
        else
            pure unit
