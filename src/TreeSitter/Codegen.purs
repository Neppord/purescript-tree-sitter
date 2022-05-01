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
import Tidy.Codegen (binaryOp, binderString, binderVar, caseBranch, declDerive, declNewtype, declSignature, declValue, exprApp, exprCase, exprCtor, exprIdent, exprLambda, exprOp, exprRecord, exprString, exprTyped, typeApp, typeArrow, typeConstrained, typeCtor, typeRecord, typeRow, typeString, typeVar)
import Tidy.Codegen.Monad (codegenModule, importClass, importFrom, importOp, importOpen, importType, importValue)
import TreeSitter.Codegen.NodeTypes (ChildType, NodeType)
import Tidy.Codegen.Monad (importTypeAll)

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
        ( \name childType ->
              if childType.types # Array.filter _.named # Array.null then []
              else [ Tuple name (renderVariantChildType childType) ]
        )
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


renderParseFields :: Partial => Object ChildType -> Tuple String (CST.Expr Void)
renderParseFields fields = Tuple "fields" (exprRecord rows)
    where
    lambda childType = exprLambda [ binderVar "field" ]
        (renderSelectParserFromChildType childType (exprIdent "field"))
    renderParseField name childType =
        if childType.types # Array.filter _.named # Array.null then []
        else
            [ Tuple name $ case childType.multiple of
                  false -> case childType.required of
                      Just true -> exprApp (lambda childType)
                          [ exprApp (exprIdent "fromJust")
                                [ exprApp (exprIdent "nodeField")
                                      [ exprString name
                                      , exprIdent "syntaxNode"
                                      ]
                                ]
                          ]
                      _ -> exprOp (lambda childType)
                          [ binaryOp "<$>" $ exprApp
                                (exprIdent "nodeField")
                                [ exprString name
                                , exprIdent "syntaxNode"
                                ]
                          ]
                  true -> exprOp (lambda childType)
                      [ binaryOp "<$>" $ exprApp (exprIdent "arrayField")
                            [ exprString name, exprIdent "syntaxNode" ]
                      ]
            ]
    rows = Object.foldMap renderParseField fields

renderSelectParserFromChildType
    :: Partial => ChildType -> CST.Expr Void -> CST.Expr Void
renderSelectParserFromChildType childType ident =
    exprCase [ exprApp (exprIdent "type'") [ ident ] ]
        (matchTypeAndParse <$> (childType.types # Array.filter _.named))
    where
    matchTypeAndParse { type: type' } = caseBranch [ binderString type' ]
        ( exprApp (exprIdent "inj")
              [ exprTyped (exprCtor "Proxy")
                    (typeApp (typeCtor "Proxy") [ typeString type' ])
              , (exprApp (exprIdent ("parse" <> toProper type')) [ ident ])
              ]
        )

renderParseChildren :: Partial => ChildType -> Tuple String (CST.Expr Void)
renderParseChildren childType = Tuple name expr
    where
    expr = case childType.multiple of
        false -> case childType.required of
            Just true -> exprApp lambda
                ([ exprApp (exprIdent "Partial.head") [ children ] ])
            _ -> exprOp lambda
                [ binaryOp "<$>" (exprApp (exprIdent "head") [ children ])
                ]
        true -> exprOp lambda [ binaryOp "<$>" children ]
    lambda = exprLambda [ binderVar "child" ]
        (renderSelectParserFromChildType childType (exprIdent "child"))
    children = exprApp (exprIdent "namedChildren") [ exprIdent "syntaxNode" ]
    name = case childType.multiple of
        false -> "child"
        true -> "children"

renderParser :: Partial => NodeType -> Declaration Void
renderParser { type: type', fields, children } =
    declValue ("parse" <> toProper type')
        [ binderVar "syntaxNode" ]
        ( exprApp (exprCtor (toProper type'))
              [ exprRecord
                    ( [ Tuple "value" (exprIdent "syntaxNode") ]
                          <> (renderParseFields <$> fromMaybe fields)
                          <> (renderParseChildren <$> fromMaybe children)
                    )
              ]

        )

renderVariantModule :: String -> Array NodeType -> Module Void
renderVariantModule name nodeTypes = unsafePartial $ codegenModule name do
    void $ importFrom "Data.Functor.Variant" $ importType "VariantF"
    void $ importFrom "Data.Functor.Variant" $ importValue "inj"

    void $ importFrom "TreeSitter.Lazy"
        { syntaxNode: importType "SyntaxNode"
        , arrayField: importValue "arrayField"
        , children: importValue "children"
        , namedChildren: importValue "namedChildren"
        , nodeField: importValue "nodeField"
        , type': importValue "type'"
        }

    void $ importFrom "Data.Array" $ importValue "head"

    void $ importFrom "Data.Maybe" $ importType "Maybe"
    void $ importFrom "Data.Maybe" $ importValue "fromJust"

    void $ importFrom "Type.Proxy" $ importTypeAll "Proxy"
    void $ importFrom "Prelude" $ importClass "Functor"
    void $ importFrom "Prelude" $ importOp "<$>"

    void $ importFrom "Data.Array.Partial" (importValue "Partial.head")

    for_ nodeTypes \node -> do
        if node.named then
            tell
                [ renderVariantNewType node
                , renderFunctorDerivation node
                , declSignature ("parse" <> toProper node.type)
                      $ typeConstrained [ typeCtor "Partial" ]
                      $ typeArrow [ typeCtor "SyntaxNode" ]
                      $
                          typeApp
                              (typeCtor $ toProper node.type)
                              [ typeCtor "SyntaxNode" ]
                , renderParser node
                ]
        else
            pure unit
