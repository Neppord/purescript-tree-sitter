module TreeSitteer.Codegen.Swift where

import Data.Array
import Data.Functor.Variant
import Data.Maybe
import Prelude
import TreeSitter.Lazy
import Type.Proxy

import Data.Array.Partial as Partial

newtype AdditiveExpression a = AdditiveExpression
    { value :: a
    , fields ::
          { lhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , rhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor AdditiveExpression

parseAdditiveExpression
    :: Partial => SyntaxNode -> AdditiveExpression SyntaxNode
parseAdditiveExpression syntaxNode = AdditiveExpression
    { value: syntaxNode
    , fields:
          { lhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "lhs" syntaxNode
          , rhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "rhs" syntaxNode
          }
    }

newtype ArrayLiteral a = ArrayLiteral
    { value :: a
    , fields ::
          { element ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor ArrayLiteral

parseArrayLiteral :: Partial => SyntaxNode -> ArrayLiteral SyntaxNode
parseArrayLiteral syntaxNode = ArrayLiteral
    { value: syntaxNode
    , fields:
          { element:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "element" syntaxNode
          }
    }

newtype ArrayType a = ArrayType
    { value :: a
    , fields ::
          { element ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          , name ::
                VariantF
                    ( array_type :: ArrayType
                    , dictionary_type :: DictionaryType
                    , function_type :: FunctionType
                    , metatype :: Metatype
                    , opaque_type :: OpaqueType
                    , optional_type :: OptionalType
                    , protocol_composition_type :: ProtocolCompositionType
                    , tuple_type :: TupleType
                    , user_type :: UserType
                    )
                    a
          }
    }

derive instance Functor ArrayType

parseArrayType :: Partial => SyntaxNode -> ArrayType SyntaxNode
parseArrayType syntaxNode = ArrayType
    { value: syntaxNode
    , fields:
          { element:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "element" syntaxNode
          , name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) (fromJust (nodeField "name" syntaxNode))
          }
    }

newtype AsExpression a = AsExpression
    { value :: a
    , child :: VariantF (as_operator :: AsOperator) a
    , fields ::
          { expr ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , name ::
                VariantF
                    ( array_type :: ArrayType
                    , dictionary_type :: DictionaryType
                    , function_type :: FunctionType
                    , metatype :: Metatype
                    , opaque_type :: OpaqueType
                    , optional_type :: OptionalType
                    , protocol_composition_type :: ProtocolCompositionType
                    , tuple_type :: TupleType
                    , user_type :: UserType
                    )
                    a
          , type ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor AsExpression

parseAsExpression :: Partial => SyntaxNode -> AsExpression SyntaxNode
parseAsExpression syntaxNode = AsExpression
    { value: syntaxNode
    , fields:
          { expr:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "expr" syntaxNode
          , name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) (fromJust (nodeField "name" syntaxNode))
          , type:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "type" syntaxNode
          }
    , child:
          ( \child -> case type' child of
                "as_operator" -> inj (Proxy :: Proxy "as_operator")
                    (parseAsOperator child)
          ) (Partial.head (children syntaxNode))
    }

newtype AsOperator a = AsOperator { value :: a, fields :: {} }

derive instance Functor AsOperator

parseAsOperator :: Partial => SyntaxNode -> AsOperator SyntaxNode
parseAsOperator syntaxNode = AsOperator { value: syntaxNode, fields: {} }

newtype Assignment a = Assignment
    { value :: a
    , fields ::
          { result ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , target ::
                VariantF
                    ( directly_assignable_expression ::
                          DirectlyAssignableExpression
                    )
                    a
          }
    }

derive instance Functor Assignment

parseAssignment :: Partial => SyntaxNode -> Assignment SyntaxNode
parseAssignment syntaxNode = Assignment
    { value: syntaxNode
    , fields:
          { result:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "result" syntaxNode
          , target:
                ( \field -> case type' field of
                      "directly_assignable_expression" -> inj
                          (Proxy :: Proxy "directly_assignable_expression")
                          (parseDirectlyAssignableExpression field)
                ) (fromJust (nodeField "target" syntaxNode))
          }
    }

newtype AssociatedtypeDeclaration a = AssociatedtypeDeclaration
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( modifiers :: Modifiers
                    , type_constraints :: TypeConstraints
                    )
                    a
              )
    , fields ::
          { default_value ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          , must_inherit ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          , name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_identifier :: TypeIdentifier
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor AssociatedtypeDeclaration

parseAssociatedtypeDeclaration
    :: Partial => SyntaxNode -> AssociatedtypeDeclaration SyntaxNode
parseAssociatedtypeDeclaration syntaxNode = AssociatedtypeDeclaration
    { value: syntaxNode
    , fields:
          { default_value:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "default_value" syntaxNode
          , must_inherit:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "must_inherit" syntaxNode
          , name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_identifier" -> inj
                          (Proxy :: Proxy "type_identifier")
                          (parseTypeIdentifier field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
                "type_constraints" -> inj (Proxy :: Proxy "type_constraints")
                    (parseTypeConstraints child)
          ) <$> children syntaxNode
    }

newtype Async a = Async { value :: a, fields :: {} }

derive instance Functor Async

parseAsync :: Partial => SyntaxNode -> Async SyntaxNode
parseAsync syntaxNode = Async { value: syntaxNode, fields: {} }

newtype Attribute a = Attribute
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , check_expression :: CheckExpression
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , custom_operator :: CustomOperator
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , equality_expression :: EqualityExpression
                    , fully_open_range :: FullyOpenRange
                    , hex_literal :: HexLiteral
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , postfix_expression :: PostfixExpression
                    , prefix_expression :: PrefixExpression
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    , super_expression :: SuperExpression
                    , ternary_expression :: TernaryExpression
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    , user_type :: UserType
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor Attribute

parseAttribute :: Partial => SyntaxNode -> Attribute SyntaxNode
parseAttribute syntaxNode = Attribute
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
                "user_type" -> inj (Proxy :: Proxy "user_type")
                    (parseUserType child)
          ) <$> children syntaxNode
    }

newtype AvailabilityCondition a = AvailabilityCondition
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( identifier :: Identifier
                    , integer_literal :: IntegerLiteral
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor AvailabilityCondition

parseAvailabilityCondition
    :: Partial => SyntaxNode -> AvailabilityCondition SyntaxNode
parseAvailabilityCondition syntaxNode = AvailabilityCondition
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "identifier" -> inj (Proxy :: Proxy "identifier")
                    (parseIdentifier child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
          ) <$> children syntaxNode
    }

newtype AwaitExpression a = AwaitExpression
    { value :: a
    , fields ::
          { expr ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor AwaitExpression

parseAwaitExpression :: Partial => SyntaxNode -> AwaitExpression SyntaxNode
parseAwaitExpression syntaxNode = AwaitExpression
    { value: syntaxNode
    , fields:
          { expr:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "expr" syntaxNode
          }
    }

newtype BindingPattern a = BindingPattern
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , binding_pattern :: BindingPattern
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , check_expression :: CheckExpression
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , custom_operator :: CustomOperator
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , equality_expression :: EqualityExpression
                    , fully_open_range :: FullyOpenRange
                    , hex_literal :: HexLiteral
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , non_binding_pattern :: NonBindingPattern
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , postfix_expression :: PostfixExpression
                    , prefix_expression :: PrefixExpression
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    , super_expression :: SuperExpression
                    , ternary_expression :: TernaryExpression
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    , type_modifiers :: TypeModifiers
                    , user_type :: UserType
                    , wildcard_pattern :: WildcardPattern
                    )
                    a
              )
    , fields ::
          { name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor BindingPattern

parseBindingPattern :: Partial => SyntaxNode -> BindingPattern SyntaxNode
parseBindingPattern syntaxNode = BindingPattern
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "binding_pattern" -> inj (Proxy :: Proxy "binding_pattern")
                    (parseBindingPattern child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "non_binding_pattern" -> inj
                    (Proxy :: Proxy "non_binding_pattern")
                    (parseNonBindingPattern child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
                "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                    (parseTypeModifiers child)
                "user_type" -> inj (Proxy :: Proxy "user_type")
                    (parseUserType child)
                "wildcard_pattern" -> inj (Proxy :: Proxy "wildcard_pattern")
                    (parseWildcardPattern child)
          ) <$> children syntaxNode
    }

newtype BitwiseOperation a = BitwiseOperation
    { value :: a
    , fields ::
          { lhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , rhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor BitwiseOperation

parseBitwiseOperation :: Partial => SyntaxNode -> BitwiseOperation SyntaxNode
parseBitwiseOperation syntaxNode = BitwiseOperation
    { value: syntaxNode
    , fields:
          { lhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "lhs" syntaxNode
          , rhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "rhs" syntaxNode
          }
    }

newtype BooleanLiteral a = BooleanLiteral { value :: a, fields :: {} }

derive instance Functor BooleanLiteral

parseBooleanLiteral :: Partial => SyntaxNode -> BooleanLiteral SyntaxNode
parseBooleanLiteral syntaxNode = BooleanLiteral
    { value: syntaxNode, fields: {} }

newtype CallExpression a = CallExpression
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , call_suffix :: CallSuffix
                    , check_expression :: CheckExpression
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , custom_operator :: CustomOperator
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , equality_expression :: EqualityExpression
                    , fully_open_range :: FullyOpenRange
                    , hex_literal :: HexLiteral
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , postfix_expression :: PostfixExpression
                    , prefix_expression :: PrefixExpression
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    , super_expression :: SuperExpression
                    , ternary_expression :: TernaryExpression
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor CallExpression

parseCallExpression :: Partial => SyntaxNode -> CallExpression SyntaxNode
parseCallExpression syntaxNode = CallExpression
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "call_suffix" -> inj (Proxy :: Proxy "call_suffix")
                    (parseCallSuffix child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
          ) <$> children syntaxNode
    }

newtype CallSuffix a = CallSuffix
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( lambda_literal :: LambdaLiteral
                    , value_arguments :: ValueArguments
                    )
                    a
              )
    , fields ::
          { name :: Array (VariantF (simple_identifier :: SimpleIdentifier) a) }
    }

derive instance Functor CallSuffix

parseCallSuffix :: Partial => SyntaxNode -> CallSuffix SyntaxNode
parseCallSuffix syntaxNode = CallSuffix
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                ) <$> arrayField "name" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "value_arguments" -> inj (Proxy :: Proxy "value_arguments")
                    (parseValueArguments child)
          ) <$> children syntaxNode
    }

newtype CaptureList a = CaptureList
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( attribute :: Attribute
                    , capture_list_item :: CaptureListItem
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor CaptureList

parseCaptureList :: Partial => SyntaxNode -> CaptureList SyntaxNode
parseCaptureList syntaxNode = CaptureList
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "capture_list_item" -> inj (Proxy :: Proxy "capture_list_item")
                    (parseCaptureListItem child)
          ) <$> children syntaxNode
    }

newtype CaptureListItem a = CaptureListItem
    { value :: a
    , child :: Maybe (VariantF (ownership_modifier :: OwnershipModifier) a)
    , fields ::
          { name ::
                VariantF
                    ( self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    )
                    a
          , value ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor CaptureListItem

parseCaptureListItem :: Partial => SyntaxNode -> CaptureListItem SyntaxNode
parseCaptureListItem syntaxNode = CaptureListItem
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                ) (fromJust (nodeField "name" syntaxNode))
          , value:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "value" syntaxNode
          }
    , child:
          ( \child -> case type' child of
                "ownership_modifier" -> inj
                    (Proxy :: Proxy "ownership_modifier")
                    (parseOwnershipModifier child)
          ) <$> head (children syntaxNode)
    }

newtype CatchBlock a = CatchBlock
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( catch_keyword :: CatchKeyword
                    , statements :: Statements
                    , where_clause :: WhereClause
                    )
                    a
              )
    , fields ::
          { error :: Maybe (VariantF (binding_pattern :: BindingPattern) a) }
    }

derive instance Functor CatchBlock

parseCatchBlock :: Partial => SyntaxNode -> CatchBlock SyntaxNode
parseCatchBlock syntaxNode = CatchBlock
    { value: syntaxNode
    , fields:
          { error:
                ( \field -> case type' field of
                      "binding_pattern" -> inj
                          (Proxy :: Proxy "binding_pattern")
                          (parseBindingPattern field)
                ) <$> nodeField "error" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "catch_keyword" -> inj (Proxy :: Proxy "catch_keyword")
                    (parseCatchKeyword child)
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
                "where_clause" -> inj (Proxy :: Proxy "where_clause")
                    (parseWhereClause child)
          ) <$> children syntaxNode
    }

newtype CheckExpression a = CheckExpression
    { value :: a
    , fields ::
          { name ::
                VariantF
                    ( array_type :: ArrayType
                    , dictionary_type :: DictionaryType
                    , function_type :: FunctionType
                    , metatype :: Metatype
                    , opaque_type :: OpaqueType
                    , optional_type :: OptionalType
                    , protocol_composition_type :: ProtocolCompositionType
                    , tuple_type :: TupleType
                    , user_type :: UserType
                    )
                    a
          , target ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , type ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor CheckExpression

parseCheckExpression :: Partial => SyntaxNode -> CheckExpression SyntaxNode
parseCheckExpression syntaxNode = CheckExpression
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) (fromJust (nodeField "name" syntaxNode))
          , target:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "target" syntaxNode
          , type:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "type" syntaxNode
          }
    }

newtype ClassBody a = ClassBody
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( associatedtype_declaration :: AssociatedtypeDeclaration
                    , class_declaration :: ClassDeclaration
                    , deinit_declaration :: DeinitDeclaration
                    , function_declaration :: FunctionDeclaration
                    , import_declaration :: ImportDeclaration
                    , multiline_comment :: MultilineComment
                    , operator_declaration :: OperatorDeclaration
                    , precedence_group_declaration :: PrecedenceGroupDeclaration
                    , property_declaration :: PropertyDeclaration
                    , protocol_declaration :: ProtocolDeclaration
                    , subscript_declaration :: SubscriptDeclaration
                    , typealias_declaration :: TypealiasDeclaration
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor ClassBody

parseClassBody :: Partial => SyntaxNode -> ClassBody SyntaxNode
parseClassBody syntaxNode = ClassBody
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "associatedtype_declaration" -> inj
                    (Proxy :: Proxy "associatedtype_declaration")
                    (parseAssociatedtypeDeclaration child)
                "class_declaration" -> inj (Proxy :: Proxy "class_declaration")
                    (parseClassDeclaration child)
                "deinit_declaration" -> inj
                    (Proxy :: Proxy "deinit_declaration")
                    (parseDeinitDeclaration child)
                "function_declaration" -> inj
                    (Proxy :: Proxy "function_declaration")
                    (parseFunctionDeclaration child)
                "import_declaration" -> inj
                    (Proxy :: Proxy "import_declaration")
                    (parseImportDeclaration child)
                "multiline_comment" -> inj (Proxy :: Proxy "multiline_comment")
                    (parseMultilineComment child)
                "operator_declaration" -> inj
                    (Proxy :: Proxy "operator_declaration")
                    (parseOperatorDeclaration child)
                "precedence_group_declaration" -> inj
                    (Proxy :: Proxy "precedence_group_declaration")
                    (parsePrecedenceGroupDeclaration child)
                "property_declaration" -> inj
                    (Proxy :: Proxy "property_declaration")
                    (parsePropertyDeclaration child)
                "protocol_declaration" -> inj
                    (Proxy :: Proxy "protocol_declaration")
                    (parseProtocolDeclaration child)
                "subscript_declaration" -> inj
                    (Proxy :: Proxy "subscript_declaration")
                    (parseSubscriptDeclaration child)
                "typealias_declaration" -> inj
                    (Proxy :: Proxy "typealias_declaration")
                    (parseTypealiasDeclaration child)
          ) <$> children syntaxNode
    }

newtype ClassDeclaration a = ClassDeclaration
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( attribute :: Attribute
                    , inheritance_modifier :: InheritanceModifier
                    , inheritance_specifier :: InheritanceSpecifier
                    , modifiers :: Modifiers
                    , ownership_modifier :: OwnershipModifier
                    , property_behavior_modifier :: PropertyBehaviorModifier
                    , type_constraints :: TypeConstraints
                    , type_parameters :: TypeParameters
                    )
                    a
              )
    , fields ::
          { body ::
                VariantF
                    (class_body :: ClassBody, enum_class_body :: EnumClassBody)
                    a
          , name ::
                VariantF
                    (type_identifier :: TypeIdentifier, user_type :: UserType)
                    a
          }
    }

derive instance Functor ClassDeclaration

parseClassDeclaration :: Partial => SyntaxNode -> ClassDeclaration SyntaxNode
parseClassDeclaration syntaxNode = ClassDeclaration
    { value: syntaxNode
    , fields:
          { body:
                ( \field -> case type' field of
                      "class_body" -> inj (Proxy :: Proxy "class_body")
                          (parseClassBody field)
                      "enum_class_body" -> inj
                          (Proxy :: Proxy "enum_class_body")
                          (parseEnumClassBody field)
                ) (fromJust (nodeField "body" syntaxNode))
          , name:
                ( \field -> case type' field of
                      "type_identifier" -> inj
                          (Proxy :: Proxy "type_identifier")
                          (parseTypeIdentifier field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) (fromJust (nodeField "name" syntaxNode))
          }
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "inheritance_modifier" -> inj
                    (Proxy :: Proxy "inheritance_modifier")
                    (parseInheritanceModifier child)
                "inheritance_specifier" -> inj
                    (Proxy :: Proxy "inheritance_specifier")
                    (parseInheritanceSpecifier child)
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
                "ownership_modifier" -> inj
                    (Proxy :: Proxy "ownership_modifier")
                    (parseOwnershipModifier child)
                "property_behavior_modifier" -> inj
                    (Proxy :: Proxy "property_behavior_modifier")
                    (parsePropertyBehaviorModifier child)
                "type_constraints" -> inj (Proxy :: Proxy "type_constraints")
                    (parseTypeConstraints child)
                "type_parameters" -> inj (Proxy :: Proxy "type_parameters")
                    (parseTypeParameters child)
          ) <$> children syntaxNode
    }

newtype ComparisonExpression a = ComparisonExpression
    { value :: a
    , fields ::
          { lhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , rhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor ComparisonExpression

parseComparisonExpression
    :: Partial => SyntaxNode -> ComparisonExpression SyntaxNode
parseComparisonExpression syntaxNode = ComparisonExpression
    { value: syntaxNode
    , fields:
          { lhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "lhs" syntaxNode
          , rhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "rhs" syntaxNode
          }
    }

newtype ComputedGetter a = ComputedGetter
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( attribute :: Attribute
                    , getter_specifier :: GetterSpecifier
                    , statements :: Statements
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor ComputedGetter

parseComputedGetter :: Partial => SyntaxNode -> ComputedGetter SyntaxNode
parseComputedGetter syntaxNode = ComputedGetter
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "getter_specifier" -> inj (Proxy :: Proxy "getter_specifier")
                    (parseGetterSpecifier child)
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
          ) <$> children syntaxNode
    }

newtype ComputedModify a = ComputedModify
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( attribute :: Attribute
                    , modify_specifier :: ModifySpecifier
                    , statements :: Statements
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor ComputedModify

parseComputedModify :: Partial => SyntaxNode -> ComputedModify SyntaxNode
parseComputedModify syntaxNode = ComputedModify
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "modify_specifier" -> inj (Proxy :: Proxy "modify_specifier")
                    (parseModifySpecifier child)
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
          ) <$> children syntaxNode
    }

newtype ComputedProperty a = ComputedProperty
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( computed_getter :: ComputedGetter
                    , computed_modify :: ComputedModify
                    , computed_setter :: ComputedSetter
                    , statements :: Statements
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor ComputedProperty

parseComputedProperty :: Partial => SyntaxNode -> ComputedProperty SyntaxNode
parseComputedProperty syntaxNode = ComputedProperty
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "computed_getter" -> inj (Proxy :: Proxy "computed_getter")
                    (parseComputedGetter child)
                "computed_modify" -> inj (Proxy :: Proxy "computed_modify")
                    (parseComputedModify child)
                "computed_setter" -> inj (Proxy :: Proxy "computed_setter")
                    (parseComputedSetter child)
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
          ) <$> children syntaxNode
    }

newtype ComputedSetter a = ComputedSetter
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( attribute :: Attribute
                    , setter_specifier :: SetterSpecifier
                    , simple_identifier :: SimpleIdentifier
                    , statements :: Statements
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor ComputedSetter

parseComputedSetter :: Partial => SyntaxNode -> ComputedSetter SyntaxNode
parseComputedSetter syntaxNode = ComputedSetter
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "setter_specifier" -> inj (Proxy :: Proxy "setter_specifier")
                    (parseSetterSpecifier child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
          ) <$> children syntaxNode
    }

newtype ConjunctionExpression a = ConjunctionExpression
    { value :: a
    , fields ::
          { lhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , rhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor ConjunctionExpression

parseConjunctionExpression
    :: Partial => SyntaxNode -> ConjunctionExpression SyntaxNode
parseConjunctionExpression syntaxNode = ConjunctionExpression
    { value: syntaxNode
    , fields:
          { lhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "lhs" syntaxNode
          , rhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "rhs" syntaxNode
          }
    }

newtype ConstructorExpression a = ConstructorExpression
    { value :: a
    , child :: VariantF (constructor_suffix :: ConstructorSuffix) a
    , fields ::
          { constructed_type ::
                VariantF
                    ( array_type :: ArrayType
                    , dictionary_type :: DictionaryType
                    , user_type :: UserType
                    )
                    a
          }
    }

derive instance Functor ConstructorExpression

parseConstructorExpression
    :: Partial => SyntaxNode -> ConstructorExpression SyntaxNode
parseConstructorExpression syntaxNode = ConstructorExpression
    { value: syntaxNode
    , fields:
          { constructed_type:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) (fromJust (nodeField "constructed_type" syntaxNode))
          }
    , child:
          ( \child -> case type' child of
                "constructor_suffix" -> inj
                    (Proxy :: Proxy "constructor_suffix")
                    (parseConstructorSuffix child)
          ) (Partial.head (children syntaxNode))
    }

newtype ConstructorSuffix a = ConstructorSuffix
    { value :: a
    , child ::
          VariantF
              ( lambda_literal :: LambdaLiteral
              , value_arguments :: ValueArguments
              )
              a
    , fields :: {}
    }

derive instance Functor ConstructorSuffix

parseConstructorSuffix :: Partial => SyntaxNode -> ConstructorSuffix SyntaxNode
parseConstructorSuffix syntaxNode = ConstructorSuffix
    { value: syntaxNode
    , fields: {}
    , child:
          ( \child -> case type' child of
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "value_arguments" -> inj (Proxy :: Proxy "value_arguments")
                    (parseValueArguments child)
          ) (Partial.head (children syntaxNode))
    }

newtype ControlTransferStatement a = ControlTransferStatement
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , check_expression :: CheckExpression
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , custom_operator :: CustomOperator
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , equality_expression :: EqualityExpression
                    , fully_open_range :: FullyOpenRange
                    , hex_literal :: HexLiteral
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , postfix_expression :: PostfixExpression
                    , prefix_expression :: PrefixExpression
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    , super_expression :: SuperExpression
                    , ternary_expression :: TernaryExpression
                    , throw_keyword :: ThrowKeyword
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    )
                    a
              )
    , fields ::
          { result ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor ControlTransferStatement

parseControlTransferStatement
    :: Partial => SyntaxNode -> ControlTransferStatement SyntaxNode
parseControlTransferStatement syntaxNode = ControlTransferStatement
    { value: syntaxNode
    , fields:
          { result:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "result" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "throw_keyword" -> inj (Proxy :: Proxy "throw_keyword")
                    (parseThrowKeyword child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
          ) <$> children syntaxNode
    }

newtype CustomOperator a = CustomOperator { value :: a, fields :: {} }

derive instance Functor CustomOperator

parseCustomOperator :: Partial => SyntaxNode -> CustomOperator SyntaxNode
parseCustomOperator syntaxNode = CustomOperator
    { value: syntaxNode, fields: {} }

newtype DeinitDeclaration a = DeinitDeclaration
    { value :: a
    , child :: Maybe (VariantF (modifiers :: Modifiers) a)
    , fields :: { body :: VariantF (function_body :: FunctionBody) a }
    }

derive instance Functor DeinitDeclaration

parseDeinitDeclaration :: Partial => SyntaxNode -> DeinitDeclaration SyntaxNode
parseDeinitDeclaration syntaxNode = DeinitDeclaration
    { value: syntaxNode
    , fields:
          { body:
                ( \field -> case type' field of
                      "function_body" -> inj (Proxy :: Proxy "function_body")
                          (parseFunctionBody field)
                ) (fromJust (nodeField "body" syntaxNode))
          }
    , child:
          ( \child -> case type' child of
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
          ) <$> head (children syntaxNode)
    }

newtype DictionaryLiteral a = DictionaryLiteral
    { value :: a
    , fields ::
          { key ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , value ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor DictionaryLiteral

parseDictionaryLiteral :: Partial => SyntaxNode -> DictionaryLiteral SyntaxNode
parseDictionaryLiteral syntaxNode = DictionaryLiteral
    { value: syntaxNode
    , fields:
          { key:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "key" syntaxNode
          , value:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "value" syntaxNode
          }
    }

newtype DictionaryType a = DictionaryType
    { value :: a
    , fields ::
          { key ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          , name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          , value ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor DictionaryType

parseDictionaryType :: Partial => SyntaxNode -> DictionaryType SyntaxNode
parseDictionaryType syntaxNode = DictionaryType
    { value: syntaxNode
    , fields:
          { key:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "key" syntaxNode
          , name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          , value:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "value" syntaxNode
          }
    }

newtype DirectlyAssignableExpression a = DirectlyAssignableExpression
    { value :: a
    , child ::
          VariantF
              ( call_expression :: CallExpression
              , navigation_expression :: NavigationExpression
              , self_expression :: SelfExpression
              , simple_identifier :: SimpleIdentifier
              , tuple_expression :: TupleExpression
              )
              a
    , fields :: {}
    }

derive instance Functor DirectlyAssignableExpression

parseDirectlyAssignableExpression
    :: Partial => SyntaxNode -> DirectlyAssignableExpression SyntaxNode
parseDirectlyAssignableExpression syntaxNode = DirectlyAssignableExpression
    { value: syntaxNode
    , fields: {}
    , child:
          ( \child -> case type' child of
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
          ) (Partial.head (children syntaxNode))
    }

newtype DisjunctionExpression a = DisjunctionExpression
    { value :: a
    , fields ::
          { lhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , rhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor DisjunctionExpression

parseDisjunctionExpression
    :: Partial => SyntaxNode -> DisjunctionExpression SyntaxNode
parseDisjunctionExpression syntaxNode = DisjunctionExpression
    { value: syntaxNode
    , fields:
          { lhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "lhs" syntaxNode
          , rhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "rhs" syntaxNode
          }
    }

newtype DoStatement a = DoStatement
    { value :: a
    , children ::
          Array
              (VariantF (catch_block :: CatchBlock, statements :: Statements) a)
    , fields :: {}
    }

derive instance Functor DoStatement

parseDoStatement :: Partial => SyntaxNode -> DoStatement SyntaxNode
parseDoStatement syntaxNode = DoStatement
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "catch_block" -> inj (Proxy :: Proxy "catch_block")
                    (parseCatchBlock child)
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
          ) <$> children syntaxNode
    }

newtype EnumClassBody a = EnumClassBody
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( associatedtype_declaration :: AssociatedtypeDeclaration
                    , class_declaration :: ClassDeclaration
                    , deinit_declaration :: DeinitDeclaration
                    , enum_entry :: EnumEntry
                    , function_declaration :: FunctionDeclaration
                    , import_declaration :: ImportDeclaration
                    , operator_declaration :: OperatorDeclaration
                    , precedence_group_declaration :: PrecedenceGroupDeclaration
                    , property_declaration :: PropertyDeclaration
                    , protocol_declaration :: ProtocolDeclaration
                    , subscript_declaration :: SubscriptDeclaration
                    , typealias_declaration :: TypealiasDeclaration
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor EnumClassBody

parseEnumClassBody :: Partial => SyntaxNode -> EnumClassBody SyntaxNode
parseEnumClassBody syntaxNode = EnumClassBody
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "associatedtype_declaration" -> inj
                    (Proxy :: Proxy "associatedtype_declaration")
                    (parseAssociatedtypeDeclaration child)
                "class_declaration" -> inj (Proxy :: Proxy "class_declaration")
                    (parseClassDeclaration child)
                "deinit_declaration" -> inj
                    (Proxy :: Proxy "deinit_declaration")
                    (parseDeinitDeclaration child)
                "enum_entry" -> inj (Proxy :: Proxy "enum_entry")
                    (parseEnumEntry child)
                "function_declaration" -> inj
                    (Proxy :: Proxy "function_declaration")
                    (parseFunctionDeclaration child)
                "import_declaration" -> inj
                    (Proxy :: Proxy "import_declaration")
                    (parseImportDeclaration child)
                "operator_declaration" -> inj
                    (Proxy :: Proxy "operator_declaration")
                    (parseOperatorDeclaration child)
                "precedence_group_declaration" -> inj
                    (Proxy :: Proxy "precedence_group_declaration")
                    (parsePrecedenceGroupDeclaration child)
                "property_declaration" -> inj
                    (Proxy :: Proxy "property_declaration")
                    (parsePropertyDeclaration child)
                "protocol_declaration" -> inj
                    (Proxy :: Proxy "protocol_declaration")
                    (parseProtocolDeclaration child)
                "subscript_declaration" -> inj
                    (Proxy :: Proxy "subscript_declaration")
                    (parseSubscriptDeclaration child)
                "typealias_declaration" -> inj
                    (Proxy :: Proxy "typealias_declaration")
                    (parseTypealiasDeclaration child)
          ) <$> children syntaxNode
    }

newtype EnumEntry a = EnumEntry
    { value :: a
    , child :: Maybe (VariantF (modifiers :: Modifiers) a)
    , fields ::
          { data_contents ::
                Array (VariantF (enum_type_parameters :: EnumTypeParameters) a)
          , name :: Array (VariantF (simple_identifier :: SimpleIdentifier) a)
          , raw_value ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor EnumEntry

parseEnumEntry :: Partial => SyntaxNode -> EnumEntry SyntaxNode
parseEnumEntry syntaxNode = EnumEntry
    { value: syntaxNode
    , fields:
          { data_contents:
                ( \field -> case type' field of
                      "enum_type_parameters" -> inj
                          (Proxy :: Proxy "enum_type_parameters")
                          (parseEnumTypeParameters field)
                ) <$> arrayField "data_contents" syntaxNode
          , name:
                ( \field -> case type' field of
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                ) <$> arrayField "name" syntaxNode
          , raw_value:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "raw_value" syntaxNode
          }
    , child:
          ( \child -> case type' child of
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
          ) <$> head (children syntaxNode)
    }

newtype EnumTypeParameters a = EnumTypeParameters
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , check_expression :: CheckExpression
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , custom_operator :: CustomOperator
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , equality_expression :: EqualityExpression
                    , fully_open_range :: FullyOpenRange
                    , hex_literal :: HexLiteral
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , postfix_expression :: PostfixExpression
                    , prefix_expression :: PrefixExpression
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    , super_expression :: SuperExpression
                    , ternary_expression :: TernaryExpression
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    , type_modifiers :: TypeModifiers
                    , wildcard_pattern :: WildcardPattern
                    )
                    a
              )
    , fields ::
          { name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor EnumTypeParameters

parseEnumTypeParameters
    :: Partial => SyntaxNode -> EnumTypeParameters SyntaxNode
parseEnumTypeParameters syntaxNode = EnumTypeParameters
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
                "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                    (parseTypeModifiers child)
                "wildcard_pattern" -> inj (Proxy :: Proxy "wildcard_pattern")
                    (parseWildcardPattern child)
          ) <$> children syntaxNode
    }

newtype EqualityConstraint a = EqualityConstraint
    { value :: a
    , children :: Array (VariantF (attribute :: Attribute) a)
    , fields ::
          { constrained_type :: VariantF (identifier :: Identifier) a
          , must_equal ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          , name ::
                VariantF
                    ( array_type :: ArrayType
                    , dictionary_type :: DictionaryType
                    , function_type :: FunctionType
                    , metatype :: Metatype
                    , opaque_type :: OpaqueType
                    , optional_type :: OptionalType
                    , protocol_composition_type :: ProtocolCompositionType
                    , tuple_type :: TupleType
                    , user_type :: UserType
                    )
                    a
          }
    }

derive instance Functor EqualityConstraint

parseEqualityConstraint
    :: Partial => SyntaxNode -> EqualityConstraint SyntaxNode
parseEqualityConstraint syntaxNode = EqualityConstraint
    { value: syntaxNode
    , fields:
          { constrained_type:
                ( \field -> case type' field of
                      "identifier" -> inj (Proxy :: Proxy "identifier")
                          (parseIdentifier field)
                ) (fromJust (nodeField "constrained_type" syntaxNode))
          , must_equal:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "must_equal" syntaxNode
          , name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) (fromJust (nodeField "name" syntaxNode))
          }
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
          ) <$> children syntaxNode
    }

newtype EqualityExpression a = EqualityExpression
    { value :: a
    , fields ::
          { lhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , rhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor EqualityExpression

parseEqualityExpression
    :: Partial => SyntaxNode -> EqualityExpression SyntaxNode
parseEqualityExpression syntaxNode = EqualityExpression
    { value: syntaxNode
    , fields:
          { lhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "lhs" syntaxNode
          , rhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "rhs" syntaxNode
          }
    }

newtype ForStatement a = ForStatement
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( statements :: Statements
                    , type_annotation :: TypeAnnotation
                    , where_clause :: WhereClause
                    )
                    a
              )
    , fields ::
          { collection ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , item ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , array_type :: ArrayType
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , binding_pattern :: BindingPattern
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , dictionary_type :: DictionaryType
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , function_type :: FunctionType
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , metatype :: Metatype
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , non_binding_pattern :: NonBindingPattern
                          , oct_literal :: OctLiteral
                          , opaque_type :: OpaqueType
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , optional_type :: OptionalType
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , protocol_composition_type :: ProtocolCompositionType
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          , wildcard_pattern :: WildcardPattern
                          )
                          a
                    )
          , name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor ForStatement

parseForStatement :: Partial => SyntaxNode -> ForStatement SyntaxNode
parseForStatement syntaxNode = ForStatement
    { value: syntaxNode
    , fields:
          { collection:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "collection" syntaxNode
          , item:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "binding_pattern" -> inj
                          (Proxy :: Proxy "binding_pattern")
                          (parseBindingPattern field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "non_binding_pattern" -> inj
                          (Proxy :: Proxy "non_binding_pattern")
                          (parseNonBindingPattern field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                      "wildcard_pattern" -> inj
                          (Proxy :: Proxy "wildcard_pattern")
                          (parseWildcardPattern field)
                ) <$> arrayField "item" syntaxNode
          , name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
                "type_annotation" -> inj (Proxy :: Proxy "type_annotation")
                    (parseTypeAnnotation child)
                "where_clause" -> inj (Proxy :: Proxy "where_clause")
                    (parseWhereClause child)
          ) <$> children syntaxNode
    }

newtype FullyOpenRange a = FullyOpenRange { value :: a, fields :: {} }

derive instance Functor FullyOpenRange

parseFullyOpenRange :: Partial => SyntaxNode -> FullyOpenRange SyntaxNode
parseFullyOpenRange syntaxNode = FullyOpenRange
    { value: syntaxNode, fields: {} }

newtype FunctionBody a = FunctionBody
    { value :: a
    , child :: Maybe (VariantF (statements :: Statements) a)
    , fields :: {}
    }

derive instance Functor FunctionBody

parseFunctionBody :: Partial => SyntaxNode -> FunctionBody SyntaxNode
parseFunctionBody syntaxNode = FunctionBody
    { value: syntaxNode
    , fields: {}
    , child:
          ( \child -> case type' child of
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
          ) <$> head (children syntaxNode)
    }

newtype FunctionDeclaration a = FunctionDeclaration
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( async :: Async
                    , attribute :: Attribute
                    , bang :: Bang
                    , inheritance_modifier :: InheritanceModifier
                    , modifiers :: Modifiers
                    , ownership_modifier :: OwnershipModifier
                    , parameter :: Parameter
                    , property_behavior_modifier :: PropertyBehaviorModifier
                    , throws :: Throws
                    , type_constraints :: TypeConstraints
                    , type_parameters :: TypeParameters
                    )
                    a
              )
    , fields ::
          { body :: VariantF (function_body :: FunctionBody) a
          , default_value ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , bang :: Bang
                          , custom_operator :: CustomOperator
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , simple_identifier :: SimpleIdentifier
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          , return_type ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor FunctionDeclaration

parseFunctionDeclaration
    :: Partial => SyntaxNode -> FunctionDeclaration SyntaxNode
parseFunctionDeclaration syntaxNode = FunctionDeclaration
    { value: syntaxNode
    , fields:
          { body:
                ( \field -> case type' field of
                      "function_body" -> inj (Proxy :: Proxy "function_body")
                          (parseFunctionBody field)
                ) (fromJust (nodeField "body" syntaxNode))
          , default_value:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "default_value" syntaxNode
          , name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          , return_type:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "return_type" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "async" -> inj (Proxy :: Proxy "async") (parseAsync child)
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "inheritance_modifier" -> inj
                    (Proxy :: Proxy "inheritance_modifier")
                    (parseInheritanceModifier child)
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
                "ownership_modifier" -> inj
                    (Proxy :: Proxy "ownership_modifier")
                    (parseOwnershipModifier child)
                "parameter" -> inj (Proxy :: Proxy "parameter")
                    (parseParameter child)
                "property_behavior_modifier" -> inj
                    (Proxy :: Proxy "property_behavior_modifier")
                    (parsePropertyBehaviorModifier child)
                "throws" -> inj (Proxy :: Proxy "throws") (parseThrows child)
                "type_constraints" -> inj (Proxy :: Proxy "type_constraints")
                    (parseTypeConstraints child)
                "type_parameters" -> inj (Proxy :: Proxy "type_parameters")
                    (parseTypeParameters child)
          ) <$> children syntaxNode
    }

newtype FunctionModifier a = FunctionModifier { value :: a, fields :: {} }

derive instance Functor FunctionModifier

parseFunctionModifier :: Partial => SyntaxNode -> FunctionModifier SyntaxNode
parseFunctionModifier syntaxNode = FunctionModifier
    { value: syntaxNode, fields: {} }

newtype FunctionType a = FunctionType
    { value :: a
    , children :: Array (VariantF (async :: Async, throws :: Throws) a)
    , fields ::
          { name ::
                VariantF
                    ( array_type :: ArrayType
                    , dictionary_type :: DictionaryType
                    , function_type :: FunctionType
                    , metatype :: Metatype
                    , opaque_type :: OpaqueType
                    , optional_type :: OptionalType
                    , protocol_composition_type :: ProtocolCompositionType
                    , tuple_type :: TupleType
                    , user_type :: UserType
                    )
                    a
          , params :: VariantF (tuple_type :: TupleType) a
          , return_type ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor FunctionType

parseFunctionType :: Partial => SyntaxNode -> FunctionType SyntaxNode
parseFunctionType syntaxNode = FunctionType
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) (fromJust (nodeField "name" syntaxNode))
          , params:
                ( \field -> case type' field of
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                ) (fromJust (nodeField "params" syntaxNode))
          , return_type:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "return_type" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "async" -> inj (Proxy :: Proxy "async") (parseAsync child)
                "throws" -> inj (Proxy :: Proxy "throws") (parseThrows child)
          ) <$> children syntaxNode
    }

newtype GetterSpecifier a = GetterSpecifier
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( async :: Async
                    , mutation_modifier :: MutationModifier
                    , throws :: Throws
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor GetterSpecifier

parseGetterSpecifier :: Partial => SyntaxNode -> GetterSpecifier SyntaxNode
parseGetterSpecifier syntaxNode = GetterSpecifier
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "async" -> inj (Proxy :: Proxy "async") (parseAsync child)
                "mutation_modifier" -> inj (Proxy :: Proxy "mutation_modifier")
                    (parseMutationModifier child)
                "throws" -> inj (Proxy :: Proxy "throws") (parseThrows child)
          ) <$> children syntaxNode
    }

newtype GuardStatement a = GuardStatement
    { value :: a
    , children :: Array (VariantF (else :: Else, statements :: Statements) a)
    , fields ::
          { condition ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , availability_condition :: AvailabilityCondition
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , binding_pattern :: BindingPattern
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          , type_annotation :: TypeAnnotation
                          , value_binding_pattern :: ValueBindingPattern
                          )
                          a
                    )
          }
    }

derive instance Functor GuardStatement

parseGuardStatement :: Partial => SyntaxNode -> GuardStatement SyntaxNode
parseGuardStatement syntaxNode = GuardStatement
    { value: syntaxNode
    , fields:
          { condition:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "availability_condition" -> inj
                          (Proxy :: Proxy "availability_condition")
                          (parseAvailabilityCondition field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "binding_pattern" -> inj
                          (Proxy :: Proxy "binding_pattern")
                          (parseBindingPattern field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                      "type_annotation" -> inj
                          (Proxy :: Proxy "type_annotation")
                          (parseTypeAnnotation field)
                      "value_binding_pattern" -> inj
                          (Proxy :: Proxy "value_binding_pattern")
                          (parseValueBindingPattern field)
                ) <$> arrayField "condition" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "else" -> inj (Proxy :: Proxy "else") (parseElse child)
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
          ) <$> children syntaxNode
    }

newtype Identifier a = Identifier
    { value :: a
    , children :: Array (VariantF (simple_identifier :: SimpleIdentifier) a)
    , fields :: {}
    }

derive instance Functor Identifier

parseIdentifier :: Partial => SyntaxNode -> Identifier SyntaxNode
parseIdentifier syntaxNode = Identifier
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
          ) <$> children syntaxNode
    }

newtype IfStatement a = IfStatement
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( else :: Else
                    , if_statement :: IfStatement
                    , statements :: Statements
                    )
                    a
              )
    , fields ::
          { condition ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , availability_condition :: AvailabilityCondition
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , binding_pattern :: BindingPattern
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          , type_annotation :: TypeAnnotation
                          , value_binding_pattern :: ValueBindingPattern
                          )
                          a
                    )
          }
    }

derive instance Functor IfStatement

parseIfStatement :: Partial => SyntaxNode -> IfStatement SyntaxNode
parseIfStatement syntaxNode = IfStatement
    { value: syntaxNode
    , fields:
          { condition:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "availability_condition" -> inj
                          (Proxy :: Proxy "availability_condition")
                          (parseAvailabilityCondition field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "binding_pattern" -> inj
                          (Proxy :: Proxy "binding_pattern")
                          (parseBindingPattern field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                      "type_annotation" -> inj
                          (Proxy :: Proxy "type_annotation")
                          (parseTypeAnnotation field)
                      "value_binding_pattern" -> inj
                          (Proxy :: Proxy "value_binding_pattern")
                          (parseValueBindingPattern field)
                ) <$> arrayField "condition" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "else" -> inj (Proxy :: Proxy "else") (parseElse child)
                "if_statement" -> inj (Proxy :: Proxy "if_statement")
                    (parseIfStatement child)
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
          ) <$> children syntaxNode
    }

newtype ImportDeclaration a = ImportDeclaration
    { value :: a
    , children ::
          Array (VariantF (identifier :: Identifier, modifiers :: Modifiers) a)
    , fields :: {}
    }

derive instance Functor ImportDeclaration

parseImportDeclaration :: Partial => SyntaxNode -> ImportDeclaration SyntaxNode
parseImportDeclaration syntaxNode = ImportDeclaration
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "identifier" -> inj (Proxy :: Proxy "identifier")
                    (parseIdentifier child)
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
          ) <$> children syntaxNode
    }

newtype InfixExpression a = InfixExpression
    { value :: a
    , fields ::
          { lhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , op :: VariantF (custom_operator :: CustomOperator) a
          , rhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor InfixExpression

parseInfixExpression :: Partial => SyntaxNode -> InfixExpression SyntaxNode
parseInfixExpression syntaxNode = InfixExpression
    { value: syntaxNode
    , fields:
          { lhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "lhs" syntaxNode
          , op:
                ( \field -> case type' field of
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                ) (fromJust (nodeField "op" syntaxNode))
          , rhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "rhs" syntaxNode
          }
    }

newtype InheritanceConstraint a = InheritanceConstraint
    { value :: a
    , children :: Array (VariantF (attribute :: Attribute) a)
    , fields ::
          { constrained_type :: VariantF (identifier :: Identifier) a
          , inherits_from ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          , name ::
                VariantF
                    ( array_type :: ArrayType
                    , dictionary_type :: DictionaryType
                    , function_type :: FunctionType
                    , metatype :: Metatype
                    , opaque_type :: OpaqueType
                    , optional_type :: OptionalType
                    , protocol_composition_type :: ProtocolCompositionType
                    , tuple_type :: TupleType
                    , user_type :: UserType
                    )
                    a
          }
    }

derive instance Functor InheritanceConstraint

parseInheritanceConstraint
    :: Partial => SyntaxNode -> InheritanceConstraint SyntaxNode
parseInheritanceConstraint syntaxNode = InheritanceConstraint
    { value: syntaxNode
    , fields:
          { constrained_type:
                ( \field -> case type' field of
                      "identifier" -> inj (Proxy :: Proxy "identifier")
                          (parseIdentifier field)
                ) (fromJust (nodeField "constrained_type" syntaxNode))
          , inherits_from:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "inherits_from" syntaxNode
          , name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) (fromJust (nodeField "name" syntaxNode))
          }
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
          ) <$> children syntaxNode
    }

newtype InheritanceModifier a = InheritanceModifier { value :: a, fields :: {} }

derive instance Functor InheritanceModifier

parseInheritanceModifier
    :: Partial => SyntaxNode -> InheritanceModifier SyntaxNode
parseInheritanceModifier syntaxNode = InheritanceModifier
    { value: syntaxNode, fields: {} }

newtype InheritanceSpecifier a = InheritanceSpecifier
    { value :: a
    , fields ::
          { inherits_from ::
                VariantF (function_type :: FunctionType, user_type :: UserType)
                    a
          }
    }

derive instance Functor InheritanceSpecifier

parseInheritanceSpecifier
    :: Partial => SyntaxNode -> InheritanceSpecifier SyntaxNode
parseInheritanceSpecifier syntaxNode = InheritanceSpecifier
    { value: syntaxNode
    , fields:
          { inherits_from:
                ( \field -> case type' field of
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) (fromJust (nodeField "inherits_from" syntaxNode))
          }
    }

newtype InterpolatedExpression a = InterpolatedExpression
    { value :: a
    , child :: Maybe (VariantF (type_modifiers :: TypeModifiers) a)
    , fields ::
          { name :: Maybe (VariantF (simple_identifier :: SimpleIdentifier) a)
          , reference_specifier ::
                Array (VariantF (simple_identifier :: SimpleIdentifier) a)
          , value ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor InterpolatedExpression

parseInterpolatedExpression
    :: Partial => SyntaxNode -> InterpolatedExpression SyntaxNode
parseInterpolatedExpression syntaxNode = InterpolatedExpression
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                ) <$> nodeField "name" syntaxNode
          , reference_specifier:
                ( \field -> case type' field of
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                ) <$> arrayField "reference_specifier" syntaxNode
          , value:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "value" syntaxNode
          }
    , child:
          ( \child -> case type' child of
                "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                    (parseTypeModifiers child)
          ) <$> head (children syntaxNode)
    }

newtype KeyPathExpression a = KeyPathExpression
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( array_type :: ArrayType
                    , bang :: Bang
                    , dictionary_type :: DictionaryType
                    , simple_identifier :: SimpleIdentifier
                    , type_arguments :: TypeArguments
                    , type_identifier :: TypeIdentifier
                    , value_argument :: ValueArgument
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor KeyPathExpression

parseKeyPathExpression :: Partial => SyntaxNode -> KeyPathExpression SyntaxNode
parseKeyPathExpression syntaxNode = KeyPathExpression
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "array_type" -> inj (Proxy :: Proxy "array_type")
                    (parseArrayType child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "dictionary_type" -> inj (Proxy :: Proxy "dictionary_type")
                    (parseDictionaryType child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "type_arguments" -> inj (Proxy :: Proxy "type_arguments")
                    (parseTypeArguments child)
                "type_identifier" -> inj (Proxy :: Proxy "type_identifier")
                    (parseTypeIdentifier child)
                "value_argument" -> inj (Proxy :: Proxy "value_argument")
                    (parseValueArgument child)
          ) <$> children syntaxNode
    }

newtype KeyPathStringExpression a = KeyPathStringExpression
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , check_expression :: CheckExpression
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , custom_operator :: CustomOperator
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , equality_expression :: EqualityExpression
                    , fully_open_range :: FullyOpenRange
                    , hex_literal :: HexLiteral
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , postfix_expression :: PostfixExpression
                    , prefix_expression :: PrefixExpression
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    , super_expression :: SuperExpression
                    , ternary_expression :: TernaryExpression
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor KeyPathStringExpression

parseKeyPathStringExpression
    :: Partial => SyntaxNode -> KeyPathStringExpression SyntaxNode
parseKeyPathStringExpression syntaxNode = KeyPathStringExpression
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
          ) <$> children syntaxNode
    }

newtype LambdaFunctionType a = LambdaFunctionType
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( async :: Async
                    , lambda_function_type_parameters ::
                          LambdaFunctionTypeParameters
                    , throws :: Throws
                    )
                    a
              )
    , fields ::
          { name ::
                Maybe
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          , return_type ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor LambdaFunctionType

parseLambdaFunctionType
    :: Partial => SyntaxNode -> LambdaFunctionType SyntaxNode
parseLambdaFunctionType syntaxNode = LambdaFunctionType
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> nodeField "name" syntaxNode
          , return_type:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "return_type" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "async" -> inj (Proxy :: Proxy "async") (parseAsync child)
                "lambda_function_type_parameters" -> inj
                    (Proxy :: Proxy "lambda_function_type_parameters")
                    (parseLambdaFunctionTypeParameters child)
                "throws" -> inj (Proxy :: Proxy "throws") (parseThrows child)
          ) <$> children syntaxNode
    }

newtype LambdaFunctionTypeParameters a = LambdaFunctionTypeParameters
    { value :: a
    , children :: Array (VariantF (lambda_parameter :: LambdaParameter) a)
    , fields :: {}
    }

derive instance Functor LambdaFunctionTypeParameters

parseLambdaFunctionTypeParameters
    :: Partial => SyntaxNode -> LambdaFunctionTypeParameters SyntaxNode
parseLambdaFunctionTypeParameters syntaxNode = LambdaFunctionTypeParameters
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "lambda_parameter" -> inj (Proxy :: Proxy "lambda_parameter")
                    (parseLambdaParameter child)
          ) <$> children syntaxNode
    }

newtype LambdaLiteral a = LambdaLiteral
    { value :: a
    , child :: Maybe (VariantF (statements :: Statements) a)
    , fields ::
          { captures :: Maybe (VariantF (capture_list :: CaptureList) a)
          , type ::
                Maybe (VariantF (lambda_function_type :: LambdaFunctionType) a)
          }
    }

derive instance Functor LambdaLiteral

parseLambdaLiteral :: Partial => SyntaxNode -> LambdaLiteral SyntaxNode
parseLambdaLiteral syntaxNode = LambdaLiteral
    { value: syntaxNode
    , fields:
          { captures:
                ( \field -> case type' field of
                      "capture_list" -> inj (Proxy :: Proxy "capture_list")
                          (parseCaptureList field)
                ) <$> nodeField "captures" syntaxNode
          , type:
                ( \field -> case type' field of
                      "lambda_function_type" -> inj
                          (Proxy :: Proxy "lambda_function_type")
                          (parseLambdaFunctionType field)
                ) <$> nodeField "type" syntaxNode
          }
    , child:
          ( \child -> case type' child of
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
          ) <$> head (children syntaxNode)
    }

newtype LambdaParameter a = LambdaParameter
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( attribute :: Attribute
                    , parameter_modifiers :: ParameterModifiers
                    , self_expression :: SelfExpression
                    )
                    a
              )
    , fields ::
          { external_name ::
                Maybe (VariantF (simple_identifier :: SimpleIdentifier) a)
          , name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , simple_identifier :: SimpleIdentifier
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          , type ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor LambdaParameter

parseLambdaParameter :: Partial => SyntaxNode -> LambdaParameter SyntaxNode
parseLambdaParameter syntaxNode = LambdaParameter
    { value: syntaxNode
    , fields:
          { external_name:
                ( \field -> case type' field of
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                ) <$> nodeField "external_name" syntaxNode
          , name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          , type:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "type" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "parameter_modifiers" -> inj
                    (Proxy :: Proxy "parameter_modifiers")
                    (parseParameterModifiers child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
          ) <$> children syntaxNode
    }

newtype LineStrText a = LineStrText { value :: a, fields :: {} }

derive instance Functor LineStrText

parseLineStrText :: Partial => SyntaxNode -> LineStrText SyntaxNode
parseLineStrText syntaxNode = LineStrText { value: syntaxNode, fields: {} }

newtype LineStringLiteral a = LineStringLiteral
    { value :: a
    , fields ::
          { interpolation ::
                Array
                    ( VariantF
                          (interpolated_expression :: InterpolatedExpression)
                          a
                    )
          , text ::
                Array
                    ( VariantF
                          ( line_str_text :: LineStrText
                          , str_escaped_char :: StrEscapedChar
                          )
                          a
                    )
          }
    }

derive instance Functor LineStringLiteral

parseLineStringLiteral :: Partial => SyntaxNode -> LineStringLiteral SyntaxNode
parseLineStringLiteral syntaxNode = LineStringLiteral
    { value: syntaxNode
    , fields:
          { interpolation:
                ( \field -> case type' field of
                      "interpolated_expression" -> inj
                          (Proxy :: Proxy "interpolated_expression")
                          (parseInterpolatedExpression field)
                ) <$> arrayField "interpolation" syntaxNode
          , text:
                ( \field -> case type' field of
                      "line_str_text" -> inj (Proxy :: Proxy "line_str_text")
                          (parseLineStrText field)
                      "str_escaped_char" -> inj
                          (Proxy :: Proxy "str_escaped_char")
                          (parseStrEscapedChar field)
                ) <$> arrayField "text" syntaxNode
          }
    }

newtype MemberModifier a = MemberModifier { value :: a, fields :: {} }

derive instance Functor MemberModifier

parseMemberModifier :: Partial => SyntaxNode -> MemberModifier SyntaxNode
parseMemberModifier syntaxNode = MemberModifier
    { value: syntaxNode, fields: {} }

newtype Metatype a = Metatype
    { value :: a
    , child ::
          VariantF
              ( array_type :: ArrayType
              , dictionary_type :: DictionaryType
              , function_type :: FunctionType
              , metatype :: Metatype
              , opaque_type :: OpaqueType
              , optional_type :: OptionalType
              , protocol_composition_type :: ProtocolCompositionType
              , tuple_type :: TupleType
              , user_type :: UserType
              )
              a
    , fields :: {}
    }

derive instance Functor Metatype

parseMetatype :: Partial => SyntaxNode -> Metatype SyntaxNode
parseMetatype syntaxNode = Metatype
    { value: syntaxNode
    , fields: {}
    , child:
          ( \child -> case type' child of
                "array_type" -> inj (Proxy :: Proxy "array_type")
                    (parseArrayType child)
                "dictionary_type" -> inj (Proxy :: Proxy "dictionary_type")
                    (parseDictionaryType child)
                "function_type" -> inj (Proxy :: Proxy "function_type")
                    (parseFunctionType child)
                "metatype" -> inj (Proxy :: Proxy "metatype")
                    (parseMetatype child)
                "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                    (parseOpaqueType child)
                "optional_type" -> inj (Proxy :: Proxy "optional_type")
                    (parseOptionalType child)
                "protocol_composition_type" -> inj
                    (Proxy :: Proxy "protocol_composition_type")
                    (parseProtocolCompositionType child)
                "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                    (parseTupleType child)
                "user_type" -> inj (Proxy :: Proxy "user_type")
                    (parseUserType child)
          ) (Partial.head (children syntaxNode))
    }

newtype Modifiers a = Modifiers
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( attribute :: Attribute
                    , function_modifier :: FunctionModifier
                    , inheritance_modifier :: InheritanceModifier
                    , member_modifier :: MemberModifier
                    , mutation_modifier :: MutationModifier
                    , ownership_modifier :: OwnershipModifier
                    , parameter_modifier :: ParameterModifier
                    , property_behavior_modifier :: PropertyBehaviorModifier
                    , property_modifier :: PropertyModifier
                    , visibility_modifier :: VisibilityModifier
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor Modifiers

parseModifiers :: Partial => SyntaxNode -> Modifiers SyntaxNode
parseModifiers syntaxNode = Modifiers
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "function_modifier" -> inj (Proxy :: Proxy "function_modifier")
                    (parseFunctionModifier child)
                "inheritance_modifier" -> inj
                    (Proxy :: Proxy "inheritance_modifier")
                    (parseInheritanceModifier child)
                "member_modifier" -> inj (Proxy :: Proxy "member_modifier")
                    (parseMemberModifier child)
                "mutation_modifier" -> inj (Proxy :: Proxy "mutation_modifier")
                    (parseMutationModifier child)
                "ownership_modifier" -> inj
                    (Proxy :: Proxy "ownership_modifier")
                    (parseOwnershipModifier child)
                "parameter_modifier" -> inj
                    (Proxy :: Proxy "parameter_modifier")
                    (parseParameterModifier child)
                "property_behavior_modifier" -> inj
                    (Proxy :: Proxy "property_behavior_modifier")
                    (parsePropertyBehaviorModifier child)
                "property_modifier" -> inj (Proxy :: Proxy "property_modifier")
                    (parsePropertyModifier child)
                "visibility_modifier" -> inj
                    (Proxy :: Proxy "visibility_modifier")
                    (parseVisibilityModifier child)
          ) <$> children syntaxNode
    }

newtype ModifySpecifier a = ModifySpecifier
    { value :: a
    , child :: Maybe (VariantF (mutation_modifier :: MutationModifier) a)
    , fields :: {}
    }

derive instance Functor ModifySpecifier

parseModifySpecifier :: Partial => SyntaxNode -> ModifySpecifier SyntaxNode
parseModifySpecifier syntaxNode = ModifySpecifier
    { value: syntaxNode
    , fields: {}
    , child:
          ( \child -> case type' child of
                "mutation_modifier" -> inj (Proxy :: Proxy "mutation_modifier")
                    (parseMutationModifier child)
          ) <$> head (children syntaxNode)
    }

newtype MultiLineStrText a = MultiLineStrText { value :: a, fields :: {} }

derive instance Functor MultiLineStrText

parseMultiLineStrText :: Partial => SyntaxNode -> MultiLineStrText SyntaxNode
parseMultiLineStrText syntaxNode = MultiLineStrText
    { value: syntaxNode, fields: {} }

newtype MultiLineStringLiteral a = MultiLineStringLiteral
    { value :: a
    , fields ::
          { interpolation ::
                Array
                    ( VariantF
                          (interpolated_expression :: InterpolatedExpression)
                          a
                    )
          , text ::
                Array
                    ( VariantF
                          ( multi_line_str_text :: MultiLineStrText
                          , str_escaped_char :: StrEscapedChar
                          )
                          a
                    )
          }
    }

derive instance Functor MultiLineStringLiteral

parseMultiLineStringLiteral
    :: Partial => SyntaxNode -> MultiLineStringLiteral SyntaxNode
parseMultiLineStringLiteral syntaxNode = MultiLineStringLiteral
    { value: syntaxNode
    , fields:
          { interpolation:
                ( \field -> case type' field of
                      "interpolated_expression" -> inj
                          (Proxy :: Proxy "interpolated_expression")
                          (parseInterpolatedExpression field)
                ) <$> arrayField "interpolation" syntaxNode
          , text:
                ( \field -> case type' field of
                      "multi_line_str_text" -> inj
                          (Proxy :: Proxy "multi_line_str_text")
                          (parseMultiLineStrText field)
                      "str_escaped_char" -> inj
                          (Proxy :: Proxy "str_escaped_char")
                          (parseStrEscapedChar field)
                ) <$> arrayField "text" syntaxNode
          }
    }

newtype MultiplicativeExpression a = MultiplicativeExpression
    { value :: a
    , fields ::
          { lhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , rhs ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor MultiplicativeExpression

parseMultiplicativeExpression
    :: Partial => SyntaxNode -> MultiplicativeExpression SyntaxNode
parseMultiplicativeExpression syntaxNode = MultiplicativeExpression
    { value: syntaxNode
    , fields:
          { lhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "lhs" syntaxNode
          , rhs:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "rhs" syntaxNode
          }
    }

newtype MutationModifier a = MutationModifier { value :: a, fields :: {} }

derive instance Functor MutationModifier

parseMutationModifier :: Partial => SyntaxNode -> MutationModifier SyntaxNode
parseMutationModifier syntaxNode = MutationModifier
    { value: syntaxNode, fields: {} }

newtype NavigationExpression a = NavigationExpression
    { value :: a
    , fields ::
          { suffix :: VariantF (navigation_suffix :: NavigationSuffix) a
          , target ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , array_type :: ArrayType
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , dictionary_type :: DictionaryType
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor NavigationExpression

parseNavigationExpression
    :: Partial => SyntaxNode -> NavigationExpression SyntaxNode
parseNavigationExpression syntaxNode = NavigationExpression
    { value: syntaxNode
    , fields:
          { suffix:
                ( \field -> case type' field of
                      "navigation_suffix" -> inj
                          (Proxy :: Proxy "navigation_suffix")
                          (parseNavigationSuffix field)
                ) (fromJust (nodeField "suffix" syntaxNode))
          , target:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "target" syntaxNode
          }
    }

newtype NavigationSuffix a = NavigationSuffix
    { value :: a
    , fields ::
          { suffix ::
                VariantF
                    ( integer_literal :: IntegerLiteral
                    , simple_identifier :: SimpleIdentifier
                    )
                    a
          }
    }

derive instance Functor NavigationSuffix

parseNavigationSuffix :: Partial => SyntaxNode -> NavigationSuffix SyntaxNode
parseNavigationSuffix syntaxNode = NavigationSuffix
    { value: syntaxNode
    , fields:
          { suffix:
                ( \field -> case type' field of
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                ) (fromJust (nodeField "suffix" syntaxNode))
          }
    }

newtype NilCoalescingExpression a = NilCoalescingExpression
    { value :: a
    , fields ::
          { if_nil ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , value ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor NilCoalescingExpression

parseNilCoalescingExpression
    :: Partial => SyntaxNode -> NilCoalescingExpression SyntaxNode
parseNilCoalescingExpression syntaxNode = NilCoalescingExpression
    { value: syntaxNode
    , fields:
          { if_nil:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "if_nil" syntaxNode
          , value:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "value" syntaxNode
          }
    }

newtype NonBindingPattern a = NonBindingPattern
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , check_expression :: CheckExpression
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , custom_operator :: CustomOperator
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , equality_expression :: EqualityExpression
                    , fully_open_range :: FullyOpenRange
                    , hex_literal :: HexLiteral
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , non_binding_pattern :: NonBindingPattern
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , postfix_expression :: PostfixExpression
                    , prefix_expression :: PrefixExpression
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    , super_expression :: SuperExpression
                    , ternary_expression :: TernaryExpression
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    , type_modifiers :: TypeModifiers
                    , user_type :: UserType
                    , wildcard_pattern :: WildcardPattern
                    )
                    a
              )
    , fields ::
          { name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor NonBindingPattern

parseNonBindingPattern :: Partial => SyntaxNode -> NonBindingPattern SyntaxNode
parseNonBindingPattern syntaxNode = NonBindingPattern
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "non_binding_pattern" -> inj
                    (Proxy :: Proxy "non_binding_pattern")
                    (parseNonBindingPattern child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
                "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                    (parseTypeModifiers child)
                "user_type" -> inj (Proxy :: Proxy "user_type")
                    (parseUserType child)
                "wildcard_pattern" -> inj (Proxy :: Proxy "wildcard_pattern")
                    (parseWildcardPattern child)
          ) <$> children syntaxNode
    }

newtype OpaqueType a = OpaqueType
    { value :: a, child :: VariantF (user_type :: UserType) a, fields :: {} }

derive instance Functor OpaqueType

parseOpaqueType :: Partial => SyntaxNode -> OpaqueType SyntaxNode
parseOpaqueType syntaxNode = OpaqueType
    { value: syntaxNode
    , fields: {}
    , child:
          ( \child -> case type' child of
                "user_type" -> inj (Proxy :: Proxy "user_type")
                    (parseUserType child)
          ) (Partial.head (children syntaxNode))
    }

newtype OpenEndRangeExpression a = OpenEndRangeExpression
    { value :: a
    , fields ::
          { start ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor OpenEndRangeExpression

parseOpenEndRangeExpression
    :: Partial => SyntaxNode -> OpenEndRangeExpression SyntaxNode
parseOpenEndRangeExpression syntaxNode = OpenEndRangeExpression
    { value: syntaxNode
    , fields:
          { start:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "start" syntaxNode
          }
    }

newtype OpenStartRangeExpression a = OpenStartRangeExpression
    { value :: a
    , fields ::
          { end ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor OpenStartRangeExpression

parseOpenStartRangeExpression
    :: Partial => SyntaxNode -> OpenStartRangeExpression SyntaxNode
parseOpenStartRangeExpression syntaxNode = OpenStartRangeExpression
    { value: syntaxNode
    , fields:
          { end:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "end" syntaxNode
          }
    }

newtype OperatorDeclaration a = OperatorDeclaration
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( custom_operator :: CustomOperator
                    , simple_identifier :: SimpleIdentifier
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor OperatorDeclaration

parseOperatorDeclaration
    :: Partial => SyntaxNode -> OperatorDeclaration SyntaxNode
parseOperatorDeclaration syntaxNode = OperatorDeclaration
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
          ) <$> children syntaxNode
    }

newtype OptionalType a = OptionalType
    { value :: a
    , fields ::
          { wrapped ::
                VariantF
                    ( array_type :: ArrayType
                    , dictionary_type :: DictionaryType
                    , tuple_type :: TupleType
                    , user_type :: UserType
                    )
                    a
          }
    }

derive instance Functor OptionalType

parseOptionalType :: Partial => SyntaxNode -> OptionalType SyntaxNode
parseOptionalType syntaxNode = OptionalType
    { value: syntaxNode
    , fields:
          { wrapped:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) (fromJust (nodeField "wrapped" syntaxNode))
          }
    }

newtype OwnershipModifier a = OwnershipModifier { value :: a, fields :: {} }

derive instance Functor OwnershipModifier

parseOwnershipModifier :: Partial => SyntaxNode -> OwnershipModifier SyntaxNode
parseOwnershipModifier syntaxNode = OwnershipModifier
    { value: syntaxNode, fields: {} }

newtype Parameter a = Parameter
    { value :: a
    , child :: Maybe (VariantF (parameter_modifiers :: ParameterModifiers) a)
    , fields ::
          { external_name ::
                Maybe (VariantF (simple_identifier :: SimpleIdentifier) a)
          , name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , simple_identifier :: SimpleIdentifier
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          , type ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor Parameter

parseParameter :: Partial => SyntaxNode -> Parameter SyntaxNode
parseParameter syntaxNode = Parameter
    { value: syntaxNode
    , fields:
          { external_name:
                ( \field -> case type' field of
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                ) <$> nodeField "external_name" syntaxNode
          , name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          , type:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "type" syntaxNode
          }
    , child:
          ( \child -> case type' child of
                "parameter_modifiers" -> inj
                    (Proxy :: Proxy "parameter_modifiers")
                    (parseParameterModifiers child)
          ) <$> head (children syntaxNode)
    }

newtype ParameterModifier a = ParameterModifier { value :: a, fields :: {} }

derive instance Functor ParameterModifier

parseParameterModifier :: Partial => SyntaxNode -> ParameterModifier SyntaxNode
parseParameterModifier syntaxNode = ParameterModifier
    { value: syntaxNode, fields: {} }

newtype ParameterModifiers a = ParameterModifiers
    { value :: a
    , children :: Array (VariantF (parameter_modifier :: ParameterModifier) a)
    , fields :: {}
    }

derive instance Functor ParameterModifiers

parseParameterModifiers
    :: Partial => SyntaxNode -> ParameterModifiers SyntaxNode
parseParameterModifiers syntaxNode = ParameterModifiers
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "parameter_modifier" -> inj
                    (Proxy :: Proxy "parameter_modifier")
                    (parseParameterModifier child)
          ) <$> children syntaxNode
    }

newtype PostfixExpression a = PostfixExpression
    { value :: a
    , fields ::
          { operation :: VariantF (bang :: Bang) a
          , target ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor PostfixExpression

parsePostfixExpression :: Partial => SyntaxNode -> PostfixExpression SyntaxNode
parsePostfixExpression syntaxNode = PostfixExpression
    { value: syntaxNode
    , fields:
          { operation:
                ( \field -> case type' field of
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                ) (fromJust (nodeField "operation" syntaxNode))
          , target:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "target" syntaxNode
          }
    }

newtype PrecedenceGroupAttribute a = PrecedenceGroupAttribute
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( boolean_literal :: BooleanLiteral
                    , simple_identifier :: SimpleIdentifier
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor PrecedenceGroupAttribute

parsePrecedenceGroupAttribute
    :: Partial => SyntaxNode -> PrecedenceGroupAttribute SyntaxNode
parsePrecedenceGroupAttribute syntaxNode = PrecedenceGroupAttribute
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
          ) <$> children syntaxNode
    }

newtype PrecedenceGroupAttributes a = PrecedenceGroupAttributes
    { value :: a
    , children ::
          Array
              ( VariantF
                    (precedence_group_attribute :: PrecedenceGroupAttribute)
                    a
              )
    , fields :: {}
    }

derive instance Functor PrecedenceGroupAttributes

parsePrecedenceGroupAttributes
    :: Partial => SyntaxNode -> PrecedenceGroupAttributes SyntaxNode
parsePrecedenceGroupAttributes syntaxNode = PrecedenceGroupAttributes
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "precedence_group_attribute" -> inj
                    (Proxy :: Proxy "precedence_group_attribute")
                    (parsePrecedenceGroupAttribute child)
          ) <$> children syntaxNode
    }

newtype PrecedenceGroupDeclaration a = PrecedenceGroupDeclaration
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( precedence_group_attributes :: PrecedenceGroupAttributes
                    , simple_identifier :: SimpleIdentifier
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor PrecedenceGroupDeclaration

parsePrecedenceGroupDeclaration
    :: Partial => SyntaxNode -> PrecedenceGroupDeclaration SyntaxNode
parsePrecedenceGroupDeclaration syntaxNode = PrecedenceGroupDeclaration
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "precedence_group_attributes" -> inj
                    (Proxy :: Proxy "precedence_group_attributes")
                    (parsePrecedenceGroupAttributes child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
          ) <$> children syntaxNode
    }

newtype PrefixExpression a = PrefixExpression
    { value :: a
    , fields ::
          { operation ::
                VariantF (bang :: Bang, custom_operator :: CustomOperator) a
          , target ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor PrefixExpression

parsePrefixExpression :: Partial => SyntaxNode -> PrefixExpression SyntaxNode
parsePrefixExpression syntaxNode = PrefixExpression
    { value: syntaxNode
    , fields:
          { operation:
                ( \field -> case type' field of
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                ) (fromJust (nodeField "operation" syntaxNode))
          , target:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "target" syntaxNode
          }
    }

newtype PropertyDeclaration a = PropertyDeclaration
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( attribute :: Attribute
                    , inheritance_modifier :: InheritanceModifier
                    , modifiers :: Modifiers
                    , ownership_modifier :: OwnershipModifier
                    , property_behavior_modifier :: PropertyBehaviorModifier
                    , type_annotation :: TypeAnnotation
                    , type_constraints :: TypeConstraints
                    )
                    a
              )
    , fields ::
          { computed_value ::
                Array (VariantF (computed_property :: ComputedProperty) a)
          , name ::
                Array
                    (VariantF (value_binding_pattern :: ValueBindingPattern) a)
          , value ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor PropertyDeclaration

parsePropertyDeclaration
    :: Partial => SyntaxNode -> PropertyDeclaration SyntaxNode
parsePropertyDeclaration syntaxNode = PropertyDeclaration
    { value: syntaxNode
    , fields:
          { computed_value:
                ( \field -> case type' field of
                      "computed_property" -> inj
                          (Proxy :: Proxy "computed_property")
                          (parseComputedProperty field)
                ) <$> arrayField "computed_value" syntaxNode
          , name:
                ( \field -> case type' field of
                      "value_binding_pattern" -> inj
                          (Proxy :: Proxy "value_binding_pattern")
                          (parseValueBindingPattern field)
                ) <$> arrayField "name" syntaxNode
          , value:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "value" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "inheritance_modifier" -> inj
                    (Proxy :: Proxy "inheritance_modifier")
                    (parseInheritanceModifier child)
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
                "ownership_modifier" -> inj
                    (Proxy :: Proxy "ownership_modifier")
                    (parseOwnershipModifier child)
                "property_behavior_modifier" -> inj
                    (Proxy :: Proxy "property_behavior_modifier")
                    (parsePropertyBehaviorModifier child)
                "type_annotation" -> inj (Proxy :: Proxy "type_annotation")
                    (parseTypeAnnotation child)
                "type_constraints" -> inj (Proxy :: Proxy "type_constraints")
                    (parseTypeConstraints child)
          ) <$> children syntaxNode
    }

newtype PropertyModifier a = PropertyModifier { value :: a, fields :: {} }

derive instance Functor PropertyModifier

parsePropertyModifier :: Partial => SyntaxNode -> PropertyModifier SyntaxNode
parsePropertyModifier syntaxNode = PropertyModifier
    { value: syntaxNode, fields: {} }

newtype ProtocolBody a = ProtocolBody
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( associatedtype_declaration :: AssociatedtypeDeclaration
                    , deinit_declaration :: DeinitDeclaration
                    , protocol_function_declaration ::
                          ProtocolFunctionDeclaration
                    , protocol_property_declaration ::
                          ProtocolPropertyDeclaration
                    , subscript_declaration :: SubscriptDeclaration
                    , typealias_declaration :: TypealiasDeclaration
                    )
                    a
              )
    , fields ::
          { body ::
                Array
                    ( VariantF
                          ( protocol_function_declaration ::
                                ProtocolFunctionDeclaration
                          )
                          a
                    )
          }
    }

derive instance Functor ProtocolBody

parseProtocolBody :: Partial => SyntaxNode -> ProtocolBody SyntaxNode
parseProtocolBody syntaxNode = ProtocolBody
    { value: syntaxNode
    , fields:
          { body:
                ( \field -> case type' field of
                      "protocol_function_declaration" -> inj
                          (Proxy :: Proxy "protocol_function_declaration")
                          (parseProtocolFunctionDeclaration field)
                ) <$> arrayField "body" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "associatedtype_declaration" -> inj
                    (Proxy :: Proxy "associatedtype_declaration")
                    (parseAssociatedtypeDeclaration child)
                "deinit_declaration" -> inj
                    (Proxy :: Proxy "deinit_declaration")
                    (parseDeinitDeclaration child)
                "protocol_function_declaration" -> inj
                    (Proxy :: Proxy "protocol_function_declaration")
                    (parseProtocolFunctionDeclaration child)
                "protocol_property_declaration" -> inj
                    (Proxy :: Proxy "protocol_property_declaration")
                    (parseProtocolPropertyDeclaration child)
                "subscript_declaration" -> inj
                    (Proxy :: Proxy "subscript_declaration")
                    (parseSubscriptDeclaration child)
                "typealias_declaration" -> inj
                    (Proxy :: Proxy "typealias_declaration")
                    (parseTypealiasDeclaration child)
          ) <$> children syntaxNode
    }

newtype ProtocolCompositionType a = ProtocolCompositionType
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( array_type :: ArrayType
                    , dictionary_type :: DictionaryType
                    , function_type :: FunctionType
                    , metatype :: Metatype
                    , opaque_type :: OpaqueType
                    , optional_type :: OptionalType
                    , protocol_composition_type :: ProtocolCompositionType
                    , tuple_type :: TupleType
                    , user_type :: UserType
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor ProtocolCompositionType

parseProtocolCompositionType
    :: Partial => SyntaxNode -> ProtocolCompositionType SyntaxNode
parseProtocolCompositionType syntaxNode = ProtocolCompositionType
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "array_type" -> inj (Proxy :: Proxy "array_type")
                    (parseArrayType child)
                "dictionary_type" -> inj (Proxy :: Proxy "dictionary_type")
                    (parseDictionaryType child)
                "function_type" -> inj (Proxy :: Proxy "function_type")
                    (parseFunctionType child)
                "metatype" -> inj (Proxy :: Proxy "metatype")
                    (parseMetatype child)
                "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                    (parseOpaqueType child)
                "optional_type" -> inj (Proxy :: Proxy "optional_type")
                    (parseOptionalType child)
                "protocol_composition_type" -> inj
                    (Proxy :: Proxy "protocol_composition_type")
                    (parseProtocolCompositionType child)
                "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                    (parseTupleType child)
                "user_type" -> inj (Proxy :: Proxy "user_type")
                    (parseUserType child)
          ) <$> children syntaxNode
    }

newtype ProtocolDeclaration a = ProtocolDeclaration
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( attribute :: Attribute
                    , inheritance_specifier :: InheritanceSpecifier
                    , modifiers :: Modifiers
                    , type_constraints :: TypeConstraints
                    , type_parameters :: TypeParameters
                    )
                    a
              )
    , fields ::
          { body :: VariantF (protocol_body :: ProtocolBody) a
          , name :: VariantF (type_identifier :: TypeIdentifier) a
          }
    }

derive instance Functor ProtocolDeclaration

parseProtocolDeclaration
    :: Partial => SyntaxNode -> ProtocolDeclaration SyntaxNode
parseProtocolDeclaration syntaxNode = ProtocolDeclaration
    { value: syntaxNode
    , fields:
          { body:
                ( \field -> case type' field of
                      "protocol_body" -> inj (Proxy :: Proxy "protocol_body")
                          (parseProtocolBody field)
                ) (fromJust (nodeField "body" syntaxNode))
          , name:
                ( \field -> case type' field of
                      "type_identifier" -> inj
                          (Proxy :: Proxy "type_identifier")
                          (parseTypeIdentifier field)
                ) (fromJust (nodeField "name" syntaxNode))
          }
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "inheritance_specifier" -> inj
                    (Proxy :: Proxy "inheritance_specifier")
                    (parseInheritanceSpecifier child)
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
                "type_constraints" -> inj (Proxy :: Proxy "type_constraints")
                    (parseTypeConstraints child)
                "type_parameters" -> inj (Proxy :: Proxy "type_parameters")
                    (parseTypeParameters child)
          ) <$> children syntaxNode
    }

newtype ProtocolFunctionDeclaration a = ProtocolFunctionDeclaration
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( async :: Async
                    , attribute :: Attribute
                    , bang :: Bang
                    , modifiers :: Modifiers
                    , parameter :: Parameter
                    , statements :: Statements
                    , throws :: Throws
                    , type_constraints :: TypeConstraints
                    , type_parameters :: TypeParameters
                    )
                    a
              )
    , fields ::
          { default_value ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , bang :: Bang
                          , custom_operator :: CustomOperator
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , simple_identifier :: SimpleIdentifier
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          , return_type ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor ProtocolFunctionDeclaration

parseProtocolFunctionDeclaration
    :: Partial => SyntaxNode -> ProtocolFunctionDeclaration SyntaxNode
parseProtocolFunctionDeclaration syntaxNode = ProtocolFunctionDeclaration
    { value: syntaxNode
    , fields:
          { default_value:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "default_value" syntaxNode
          , name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          , return_type:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "return_type" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "async" -> inj (Proxy :: Proxy "async") (parseAsync child)
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
                "parameter" -> inj (Proxy :: Proxy "parameter")
                    (parseParameter child)
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
                "throws" -> inj (Proxy :: Proxy "throws") (parseThrows child)
                "type_constraints" -> inj (Proxy :: Proxy "type_constraints")
                    (parseTypeConstraints child)
                "type_parameters" -> inj (Proxy :: Proxy "type_parameters")
                    (parseTypeParameters child)
          ) <$> children syntaxNode
    }

newtype ProtocolPropertyDeclaration a = ProtocolPropertyDeclaration
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( modifiers :: Modifiers
                    , protocol_property_requirements ::
                          ProtocolPropertyRequirements
                    , type_annotation :: TypeAnnotation
                    , type_constraints :: TypeConstraints
                    )
                    a
              )
    , fields ::
          { name :: VariantF (value_binding_pattern :: ValueBindingPattern) a }
    }

derive instance Functor ProtocolPropertyDeclaration

parseProtocolPropertyDeclaration
    :: Partial => SyntaxNode -> ProtocolPropertyDeclaration SyntaxNode
parseProtocolPropertyDeclaration syntaxNode = ProtocolPropertyDeclaration
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "value_binding_pattern" -> inj
                          (Proxy :: Proxy "value_binding_pattern")
                          (parseValueBindingPattern field)
                ) (fromJust (nodeField "name" syntaxNode))
          }
    , children:
          ( \child -> case type' child of
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
                "protocol_property_requirements" -> inj
                    (Proxy :: Proxy "protocol_property_requirements")
                    (parseProtocolPropertyRequirements child)
                "type_annotation" -> inj (Proxy :: Proxy "type_annotation")
                    (parseTypeAnnotation child)
                "type_constraints" -> inj (Proxy :: Proxy "type_constraints")
                    (parseTypeConstraints child)
          ) <$> children syntaxNode
    }

newtype ProtocolPropertyRequirements a = ProtocolPropertyRequirements
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( getter_specifier :: GetterSpecifier
                    , setter_specifier :: SetterSpecifier
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor ProtocolPropertyRequirements

parseProtocolPropertyRequirements
    :: Partial => SyntaxNode -> ProtocolPropertyRequirements SyntaxNode
parseProtocolPropertyRequirements syntaxNode = ProtocolPropertyRequirements
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "getter_specifier" -> inj (Proxy :: Proxy "getter_specifier")
                    (parseGetterSpecifier child)
                "setter_specifier" -> inj (Proxy :: Proxy "setter_specifier")
                    (parseSetterSpecifier child)
          ) <$> children syntaxNode
    }

newtype RangeExpression a = RangeExpression
    { value :: a
    , fields ::
          { end ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , start ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor RangeExpression

parseRangeExpression :: Partial => SyntaxNode -> RangeExpression SyntaxNode
parseRangeExpression syntaxNode = RangeExpression
    { value: syntaxNode
    , fields:
          { end:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "end" syntaxNode
          , start:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "start" syntaxNode
          }
    }

newtype RawStrInterpolation a = RawStrInterpolation
    { value :: a
    , child ::
          VariantF (raw_str_interpolation_start :: RawStrInterpolationStart) a
    , fields ::
          { interpolation ::
                Array
                    ( VariantF
                          (interpolated_expression :: InterpolatedExpression)
                          a
                    )
          }
    }

derive instance Functor RawStrInterpolation

parseRawStrInterpolation
    :: Partial => SyntaxNode -> RawStrInterpolation SyntaxNode
parseRawStrInterpolation syntaxNode = RawStrInterpolation
    { value: syntaxNode
    , fields:
          { interpolation:
                ( \field -> case type' field of
                      "interpolated_expression" -> inj
                          (Proxy :: Proxy "interpolated_expression")
                          (parseInterpolatedExpression field)
                ) <$> arrayField "interpolation" syntaxNode
          }
    , child:
          ( \child -> case type' child of
                "raw_str_interpolation_start" -> inj
                    (Proxy :: Proxy "raw_str_interpolation_start")
                    (parseRawStrInterpolationStart child)
          ) (Partial.head (children syntaxNode))
    }

newtype RawStringLiteral a = RawStringLiteral
    { value :: a
    , children ::
          Array
              ( VariantF
                    (raw_str_continuing_indicator :: RawStrContinuingIndicator)
                    a
              )
    , fields ::
          { interpolation ::
                Array
                    (VariantF (raw_str_interpolation :: RawStrInterpolation) a)
          , text ::
                Array
                    ( VariantF
                          ( raw_str_end_part :: RawStrEndPart
                          , raw_str_part :: RawStrPart
                          )
                          a
                    )
          }
    }

derive instance Functor RawStringLiteral

parseRawStringLiteral :: Partial => SyntaxNode -> RawStringLiteral SyntaxNode
parseRawStringLiteral syntaxNode = RawStringLiteral
    { value: syntaxNode
    , fields:
          { interpolation:
                ( \field -> case type' field of
                      "raw_str_interpolation" -> inj
                          (Proxy :: Proxy "raw_str_interpolation")
                          (parseRawStrInterpolation field)
                ) <$> arrayField "interpolation" syntaxNode
          , text:
                ( \field -> case type' field of
                      "raw_str_end_part" -> inj
                          (Proxy :: Proxy "raw_str_end_part")
                          (parseRawStrEndPart field)
                      "raw_str_part" -> inj (Proxy :: Proxy "raw_str_part")
                          (parseRawStrPart field)
                ) <$> arrayField "text" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "raw_str_continuing_indicator" -> inj
                    (Proxy :: Proxy "raw_str_continuing_indicator")
                    (parseRawStrContinuingIndicator child)
          ) <$> children syntaxNode
    }

newtype RepeatWhileStatement a = RepeatWhileStatement
    { value :: a
    , child :: Maybe (VariantF (statements :: Statements) a)
    , fields ::
          { condition ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , availability_condition :: AvailabilityCondition
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , binding_pattern :: BindingPattern
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          , type_annotation :: TypeAnnotation
                          , value_binding_pattern :: ValueBindingPattern
                          )
                          a
                    )
          }
    }

derive instance Functor RepeatWhileStatement

parseRepeatWhileStatement
    :: Partial => SyntaxNode -> RepeatWhileStatement SyntaxNode
parseRepeatWhileStatement syntaxNode = RepeatWhileStatement
    { value: syntaxNode
    , fields:
          { condition:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "availability_condition" -> inj
                          (Proxy :: Proxy "availability_condition")
                          (parseAvailabilityCondition field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "binding_pattern" -> inj
                          (Proxy :: Proxy "binding_pattern")
                          (parseBindingPattern field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                      "type_annotation" -> inj
                          (Proxy :: Proxy "type_annotation")
                          (parseTypeAnnotation field)
                      "value_binding_pattern" -> inj
                          (Proxy :: Proxy "value_binding_pattern")
                          (parseValueBindingPattern field)
                ) <$> arrayField "condition" syntaxNode
          }
    , child:
          ( \child -> case type' child of
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
          ) <$> head (children syntaxNode)
    }

newtype SelectorExpression a = SelectorExpression
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , check_expression :: CheckExpression
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , custom_operator :: CustomOperator
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , equality_expression :: EqualityExpression
                    , fully_open_range :: FullyOpenRange
                    , hex_literal :: HexLiteral
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , postfix_expression :: PostfixExpression
                    , prefix_expression :: PrefixExpression
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    , super_expression :: SuperExpression
                    , ternary_expression :: TernaryExpression
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor SelectorExpression

parseSelectorExpression
    :: Partial => SyntaxNode -> SelectorExpression SyntaxNode
parseSelectorExpression syntaxNode = SelectorExpression
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
          ) <$> children syntaxNode
    }

newtype SelfExpression a = SelfExpression { value :: a, fields :: {} }

derive instance Functor SelfExpression

parseSelfExpression :: Partial => SyntaxNode -> SelfExpression SyntaxNode
parseSelfExpression syntaxNode = SelfExpression
    { value: syntaxNode, fields: {} }

newtype SetterSpecifier a = SetterSpecifier
    { value :: a
    , child :: Maybe (VariantF (mutation_modifier :: MutationModifier) a)
    , fields :: {}
    }

derive instance Functor SetterSpecifier

parseSetterSpecifier :: Partial => SyntaxNode -> SetterSpecifier SyntaxNode
parseSetterSpecifier syntaxNode = SetterSpecifier
    { value: syntaxNode
    , fields: {}
    , child:
          ( \child -> case type' child of
                "mutation_modifier" -> inj (Proxy :: Proxy "mutation_modifier")
                    (parseMutationModifier child)
          ) <$> head (children syntaxNode)
    }

newtype ShebangLine a = ShebangLine { value :: a, fields :: {} }

derive instance Functor ShebangLine

parseShebangLine :: Partial => SyntaxNode -> ShebangLine SyntaxNode
parseShebangLine syntaxNode = ShebangLine { value: syntaxNode, fields: {} }

newtype SimpleIdentifier a = SimpleIdentifier { value :: a, fields :: {} }

derive instance Functor SimpleIdentifier

parseSimpleIdentifier :: Partial => SyntaxNode -> SimpleIdentifier SyntaxNode
parseSimpleIdentifier syntaxNode = SimpleIdentifier
    { value: syntaxNode, fields: {} }

newtype SourceFile a = SourceFile
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , associatedtype_declaration :: AssociatedtypeDeclaration
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , check_expression :: CheckExpression
                    , class_declaration :: ClassDeclaration
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , custom_operator :: CustomOperator
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , do_statement :: DoStatement
                    , equality_expression :: EqualityExpression
                    , for_statement :: ForStatement
                    , fully_open_range :: FullyOpenRange
                    , function_declaration :: FunctionDeclaration
                    , guard_statement :: GuardStatement
                    , hex_literal :: HexLiteral
                    , if_statement :: IfStatement
                    , import_declaration :: ImportDeclaration
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , operator_declaration :: OperatorDeclaration
                    , postfix_expression :: PostfixExpression
                    , precedence_group_declaration :: PrecedenceGroupDeclaration
                    , prefix_expression :: PrefixExpression
                    , property_declaration :: PropertyDeclaration
                    , protocol_declaration :: ProtocolDeclaration
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , repeat_while_statement :: RepeatWhileStatement
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , shebang_line :: ShebangLine
                    , simple_identifier :: SimpleIdentifier
                    , statement_label :: StatementLabel
                    , super_expression :: SuperExpression
                    , switch_statement :: SwitchStatement
                    , ternary_expression :: TernaryExpression
                    , throw_keyword :: ThrowKeyword
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    , typealias_declaration :: TypealiasDeclaration
                    , while_statement :: WhileStatement
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor SourceFile

parseSourceFile :: Partial => SyntaxNode -> SourceFile SyntaxNode
parseSourceFile syntaxNode = SourceFile
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "associatedtype_declaration" -> inj
                    (Proxy :: Proxy "associatedtype_declaration")
                    (parseAssociatedtypeDeclaration child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "class_declaration" -> inj (Proxy :: Proxy "class_declaration")
                    (parseClassDeclaration child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "do_statement" -> inj (Proxy :: Proxy "do_statement")
                    (parseDoStatement child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "for_statement" -> inj (Proxy :: Proxy "for_statement")
                    (parseForStatement child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "function_declaration" -> inj
                    (Proxy :: Proxy "function_declaration")
                    (parseFunctionDeclaration child)
                "guard_statement" -> inj (Proxy :: Proxy "guard_statement")
                    (parseGuardStatement child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "if_statement" -> inj (Proxy :: Proxy "if_statement")
                    (parseIfStatement child)
                "import_declaration" -> inj
                    (Proxy :: Proxy "import_declaration")
                    (parseImportDeclaration child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "operator_declaration" -> inj
                    (Proxy :: Proxy "operator_declaration")
                    (parseOperatorDeclaration child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "precedence_group_declaration" -> inj
                    (Proxy :: Proxy "precedence_group_declaration")
                    (parsePrecedenceGroupDeclaration child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "property_declaration" -> inj
                    (Proxy :: Proxy "property_declaration")
                    (parsePropertyDeclaration child)
                "protocol_declaration" -> inj
                    (Proxy :: Proxy "protocol_declaration")
                    (parseProtocolDeclaration child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "repeat_while_statement" -> inj
                    (Proxy :: Proxy "repeat_while_statement")
                    (parseRepeatWhileStatement child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "shebang_line" -> inj (Proxy :: Proxy "shebang_line")
                    (parseShebangLine child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "statement_label" -> inj (Proxy :: Proxy "statement_label")
                    (parseStatementLabel child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "switch_statement" -> inj (Proxy :: Proxy "switch_statement")
                    (parseSwitchStatement child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "throw_keyword" -> inj (Proxy :: Proxy "throw_keyword")
                    (parseThrowKeyword child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
                "typealias_declaration" -> inj
                    (Proxy :: Proxy "typealias_declaration")
                    (parseTypealiasDeclaration child)
                "while_statement" -> inj (Proxy :: Proxy "while_statement")
                    (parseWhileStatement child)
          ) <$> children syntaxNode
    }

newtype Statements a = Statements
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , check_expression :: CheckExpression
                    , class_declaration :: ClassDeclaration
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , control_transfer_statement :: ControlTransferStatement
                    , custom_operator :: CustomOperator
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , do_statement :: DoStatement
                    , equality_expression :: EqualityExpression
                    , for_statement :: ForStatement
                    , fully_open_range :: FullyOpenRange
                    , function_declaration :: FunctionDeclaration
                    , guard_statement :: GuardStatement
                    , hex_literal :: HexLiteral
                    , if_statement :: IfStatement
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , postfix_expression :: PostfixExpression
                    , prefix_expression :: PrefixExpression
                    , property_declaration :: PropertyDeclaration
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , repeat_while_statement :: RepeatWhileStatement
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    , statement_label :: StatementLabel
                    , super_expression :: SuperExpression
                    , switch_statement :: SwitchStatement
                    , ternary_expression :: TernaryExpression
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    , typealias_declaration :: TypealiasDeclaration
                    , while_statement :: WhileStatement
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor Statements

parseStatements :: Partial => SyntaxNode -> Statements SyntaxNode
parseStatements syntaxNode = Statements
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "class_declaration" -> inj (Proxy :: Proxy "class_declaration")
                    (parseClassDeclaration child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "control_transfer_statement" -> inj
                    (Proxy :: Proxy "control_transfer_statement")
                    (parseControlTransferStatement child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "do_statement" -> inj (Proxy :: Proxy "do_statement")
                    (parseDoStatement child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "for_statement" -> inj (Proxy :: Proxy "for_statement")
                    (parseForStatement child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "function_declaration" -> inj
                    (Proxy :: Proxy "function_declaration")
                    (parseFunctionDeclaration child)
                "guard_statement" -> inj (Proxy :: Proxy "guard_statement")
                    (parseGuardStatement child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "if_statement" -> inj (Proxy :: Proxy "if_statement")
                    (parseIfStatement child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "property_declaration" -> inj
                    (Proxy :: Proxy "property_declaration")
                    (parsePropertyDeclaration child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "repeat_while_statement" -> inj
                    (Proxy :: Proxy "repeat_while_statement")
                    (parseRepeatWhileStatement child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "statement_label" -> inj (Proxy :: Proxy "statement_label")
                    (parseStatementLabel child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "switch_statement" -> inj (Proxy :: Proxy "switch_statement")
                    (parseSwitchStatement child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
                "typealias_declaration" -> inj
                    (Proxy :: Proxy "typealias_declaration")
                    (parseTypealiasDeclaration child)
                "while_statement" -> inj (Proxy :: Proxy "while_statement")
                    (parseWhileStatement child)
          ) <$> children syntaxNode
    }

newtype StrEscapedChar a = StrEscapedChar { value :: a, fields :: {} }

derive instance Functor StrEscapedChar

parseStrEscapedChar :: Partial => SyntaxNode -> StrEscapedChar SyntaxNode
parseStrEscapedChar syntaxNode = StrEscapedChar
    { value: syntaxNode, fields: {} }

newtype SubscriptDeclaration a = SubscriptDeclaration
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( attribute :: Attribute
                    , computed_getter :: ComputedGetter
                    , computed_modify :: ComputedModify
                    , computed_setter :: ComputedSetter
                    , modifiers :: Modifiers
                    , parameter :: Parameter
                    , statements :: Statements
                    , type_constraints :: TypeConstraints
                    , type_parameters :: TypeParameters
                    )
                    a
              )
    , fields ::
          { default_value ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , name ::
                Maybe
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          , return_type ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor SubscriptDeclaration

parseSubscriptDeclaration
    :: Partial => SyntaxNode -> SubscriptDeclaration SyntaxNode
parseSubscriptDeclaration syntaxNode = SubscriptDeclaration
    { value: syntaxNode
    , fields:
          { default_value:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "default_value" syntaxNode
          , name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> nodeField "name" syntaxNode
          , return_type:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "return_type" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "computed_getter" -> inj (Proxy :: Proxy "computed_getter")
                    (parseComputedGetter child)
                "computed_modify" -> inj (Proxy :: Proxy "computed_modify")
                    (parseComputedModify child)
                "computed_setter" -> inj (Proxy :: Proxy "computed_setter")
                    (parseComputedSetter child)
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
                "parameter" -> inj (Proxy :: Proxy "parameter")
                    (parseParameter child)
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
                "type_constraints" -> inj (Proxy :: Proxy "type_constraints")
                    (parseTypeConstraints child)
                "type_parameters" -> inj (Proxy :: Proxy "type_parameters")
                    (parseTypeParameters child)
          ) <$> children syntaxNode
    }

newtype SuperExpression a = SuperExpression { value :: a, fields :: {} }

derive instance Functor SuperExpression

parseSuperExpression :: Partial => SyntaxNode -> SuperExpression SyntaxNode
parseSuperExpression syntaxNode = SuperExpression
    { value: syntaxNode, fields: {} }

newtype SwitchEntry a = SwitchEntry
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , check_expression :: CheckExpression
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , custom_operator :: CustomOperator
                    , default_keyword :: DefaultKeyword
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , equality_expression :: EqualityExpression
                    , fully_open_range :: FullyOpenRange
                    , hex_literal :: HexLiteral
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , modifiers :: Modifiers
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , postfix_expression :: PostfixExpression
                    , prefix_expression :: PrefixExpression
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    , statements :: Statements
                    , super_expression :: SuperExpression
                    , switch_pattern :: SwitchPattern
                    , ternary_expression :: TernaryExpression
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    , where_keyword :: WhereKeyword
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor SwitchEntry

parseSwitchEntry :: Partial => SyntaxNode -> SwitchEntry SyntaxNode
parseSwitchEntry syntaxNode = SwitchEntry
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "default_keyword" -> inj (Proxy :: Proxy "default_keyword")
                    (parseDefaultKeyword child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "switch_pattern" -> inj (Proxy :: Proxy "switch_pattern")
                    (parseSwitchPattern child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
                "where_keyword" -> inj (Proxy :: Proxy "where_keyword")
                    (parseWhereKeyword child)
          ) <$> children syntaxNode
    }

newtype SwitchPattern a = SwitchPattern
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , binding_pattern :: BindingPattern
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , check_expression :: CheckExpression
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , custom_operator :: CustomOperator
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , equality_expression :: EqualityExpression
                    , fully_open_range :: FullyOpenRange
                    , hex_literal :: HexLiteral
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , non_binding_pattern :: NonBindingPattern
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , postfix_expression :: PostfixExpression
                    , prefix_expression :: PrefixExpression
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    , super_expression :: SuperExpression
                    , ternary_expression :: TernaryExpression
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    , type_modifiers :: TypeModifiers
                    , user_type :: UserType
                    , wildcard_pattern :: WildcardPattern
                    )
                    a
              )
    , fields ::
          { name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor SwitchPattern

parseSwitchPattern :: Partial => SyntaxNode -> SwitchPattern SyntaxNode
parseSwitchPattern syntaxNode = SwitchPattern
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "binding_pattern" -> inj (Proxy :: Proxy "binding_pattern")
                    (parseBindingPattern child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "non_binding_pattern" -> inj
                    (Proxy :: Proxy "non_binding_pattern")
                    (parseNonBindingPattern child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
                "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                    (parseTypeModifiers child)
                "user_type" -> inj (Proxy :: Proxy "user_type")
                    (parseUserType child)
                "wildcard_pattern" -> inj (Proxy :: Proxy "wildcard_pattern")
                    (parseWildcardPattern child)
          ) <$> children syntaxNode
    }

newtype SwitchStatement a = SwitchStatement
    { value :: a
    , children :: Array (VariantF (switch_entry :: SwitchEntry) a)
    , fields ::
          { expr ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor SwitchStatement

parseSwitchStatement :: Partial => SyntaxNode -> SwitchStatement SyntaxNode
parseSwitchStatement syntaxNode = SwitchStatement
    { value: syntaxNode
    , fields:
          { expr:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "expr" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "switch_entry" -> inj (Proxy :: Proxy "switch_entry")
                    (parseSwitchEntry child)
          ) <$> children syntaxNode
    }

newtype TernaryExpression a = TernaryExpression
    { value :: a
    , fields ::
          { condition ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , if_false ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          , if_true ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor TernaryExpression

parseTernaryExpression :: Partial => SyntaxNode -> TernaryExpression SyntaxNode
parseTernaryExpression syntaxNode = TernaryExpression
    { value: syntaxNode
    , fields:
          { condition:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "condition" syntaxNode
          , if_false:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "if_false" syntaxNode
          , if_true:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "if_true" syntaxNode
          }
    }

newtype Throws a = Throws { value :: a, fields :: {} }

derive instance Functor Throws

parseThrows :: Partial => SyntaxNode -> Throws SyntaxNode
parseThrows syntaxNode = Throws { value: syntaxNode, fields: {} }

newtype TryExpression a = TryExpression
    { value :: a
    , fields ::
          { expr ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor TryExpression

parseTryExpression :: Partial => SyntaxNode -> TryExpression SyntaxNode
parseTryExpression syntaxNode = TryExpression
    { value: syntaxNode
    , fields:
          { expr:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "expr" syntaxNode
          }
    }

newtype TupleExpression a = TupleExpression
    { value :: a
    , fields ::
          { name :: Array (VariantF (simple_identifier :: SimpleIdentifier) a)
          , value ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor TupleExpression

parseTupleExpression :: Partial => SyntaxNode -> TupleExpression SyntaxNode
parseTupleExpression syntaxNode = TupleExpression
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                ) <$> arrayField "name" syntaxNode
          , value:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "value" syntaxNode
          }
    }

newtype TupleType a = TupleType
    { value :: a
    , fields ::
          { element :: Array (VariantF (tuple_type_item :: TupleTypeItem) a) }
    }

derive instance Functor TupleType

parseTupleType :: Partial => SyntaxNode -> TupleType SyntaxNode
parseTupleType syntaxNode = TupleType
    { value: syntaxNode
    , fields:
          { element:
                ( \field -> case type' field of
                      "tuple_type_item" -> inj
                          (Proxy :: Proxy "tuple_type_item")
                          (parseTupleTypeItem field)
                ) <$> arrayField "element" syntaxNode
          }
    }

newtype TupleTypeItem a = TupleTypeItem
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( parameter_modifiers :: ParameterModifiers
                    , wildcard_pattern :: WildcardPattern
                    )
                    a
              )
    , fields ::
          { name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , simple_identifier :: SimpleIdentifier
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          , type ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor TupleTypeItem

parseTupleTypeItem :: Partial => SyntaxNode -> TupleTypeItem SyntaxNode
parseTupleTypeItem syntaxNode = TupleTypeItem
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          , type:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "type" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "parameter_modifiers" -> inj
                    (Proxy :: Proxy "parameter_modifiers")
                    (parseParameterModifiers child)
                "wildcard_pattern" -> inj (Proxy :: Proxy "wildcard_pattern")
                    (parseWildcardPattern child)
          ) <$> children syntaxNode
    }

newtype TypeAnnotation a = TypeAnnotation
    { value :: a
    , fields ::
          { name ::
                VariantF
                    ( array_type :: ArrayType
                    , dictionary_type :: DictionaryType
                    , function_type :: FunctionType
                    , metatype :: Metatype
                    , opaque_type :: OpaqueType
                    , optional_type :: OptionalType
                    , protocol_composition_type :: ProtocolCompositionType
                    , tuple_type :: TupleType
                    , user_type :: UserType
                    )
                    a
          , type ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor TypeAnnotation

parseTypeAnnotation :: Partial => SyntaxNode -> TypeAnnotation SyntaxNode
parseTypeAnnotation syntaxNode = TypeAnnotation
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) (fromJust (nodeField "name" syntaxNode))
          , type:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "type" syntaxNode
          }
    }

newtype TypeArguments a = TypeArguments
    { value :: a
    , children :: Array (VariantF (type_modifiers :: TypeModifiers) a)
    , fields ::
          { name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor TypeArguments

parseTypeArguments :: Partial => SyntaxNode -> TypeArguments SyntaxNode
parseTypeArguments syntaxNode = TypeArguments
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                    (parseTypeModifiers child)
          ) <$> children syntaxNode
    }

newtype TypeConstraint a = TypeConstraint
    { value :: a
    , child ::
          VariantF
              ( equality_constraint :: EqualityConstraint
              , inheritance_constraint :: InheritanceConstraint
              )
              a
    , fields :: {}
    }

derive instance Functor TypeConstraint

parseTypeConstraint :: Partial => SyntaxNode -> TypeConstraint SyntaxNode
parseTypeConstraint syntaxNode = TypeConstraint
    { value: syntaxNode
    , fields: {}
    , child:
          ( \child -> case type' child of
                "equality_constraint" -> inj
                    (Proxy :: Proxy "equality_constraint")
                    (parseEqualityConstraint child)
                "inheritance_constraint" -> inj
                    (Proxy :: Proxy "inheritance_constraint")
                    (parseInheritanceConstraint child)
          ) (Partial.head (children syntaxNode))
    }

newtype TypeConstraints a = TypeConstraints
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( type_constraint :: TypeConstraint
                    , where_keyword :: WhereKeyword
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor TypeConstraints

parseTypeConstraints :: Partial => SyntaxNode -> TypeConstraints SyntaxNode
parseTypeConstraints syntaxNode = TypeConstraints
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "type_constraint" -> inj (Proxy :: Proxy "type_constraint")
                    (parseTypeConstraint child)
                "where_keyword" -> inj (Proxy :: Proxy "where_keyword")
                    (parseWhereKeyword child)
          ) <$> children syntaxNode
    }

newtype TypeIdentifier a = TypeIdentifier { value :: a, fields :: {} }

derive instance Functor TypeIdentifier

parseTypeIdentifier :: Partial => SyntaxNode -> TypeIdentifier SyntaxNode
parseTypeIdentifier syntaxNode = TypeIdentifier
    { value: syntaxNode, fields: {} }

newtype TypeModifiers a = TypeModifiers
    { value :: a
    , children :: Array (VariantF (attribute :: Attribute) a)
    , fields :: {}
    }

derive instance Functor TypeModifiers

parseTypeModifiers :: Partial => SyntaxNode -> TypeModifiers SyntaxNode
parseTypeModifiers syntaxNode = TypeModifiers
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
          ) <$> children syntaxNode
    }

newtype TypeParameter a = TypeParameter
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( type_identifier :: TypeIdentifier
                    , type_modifiers :: TypeModifiers
                    , type_parameter_modifiers :: TypeParameterModifiers
                    )
                    a
              )
    , fields ::
          { name ::
                Maybe
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor TypeParameter

parseTypeParameter :: Partial => SyntaxNode -> TypeParameter SyntaxNode
parseTypeParameter syntaxNode = TypeParameter
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> nodeField "name" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "type_identifier" -> inj (Proxy :: Proxy "type_identifier")
                    (parseTypeIdentifier child)
                "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                    (parseTypeModifiers child)
                "type_parameter_modifiers" -> inj
                    (Proxy :: Proxy "type_parameter_modifiers")
                    (parseTypeParameterModifiers child)
          ) <$> children syntaxNode
    }

newtype TypeParameterModifiers a = TypeParameterModifiers
    { value :: a
    , children :: Array (VariantF (attribute :: Attribute) a)
    , fields :: {}
    }

derive instance Functor TypeParameterModifiers

parseTypeParameterModifiers
    :: Partial => SyntaxNode -> TypeParameterModifiers SyntaxNode
parseTypeParameterModifiers syntaxNode = TypeParameterModifiers
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
          ) <$> children syntaxNode
    }

newtype TypeParameters a = TypeParameters
    { value :: a
    , children :: Array (VariantF (type_parameter :: TypeParameter) a)
    , fields :: {}
    }

derive instance Functor TypeParameters

parseTypeParameters :: Partial => SyntaxNode -> TypeParameters SyntaxNode
parseTypeParameters syntaxNode = TypeParameters
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "type_parameter" -> inj (Proxy :: Proxy "type_parameter")
                    (parseTypeParameter child)
          ) <$> children syntaxNode
    }

newtype TypealiasDeclaration a = TypealiasDeclaration
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( attribute :: Attribute
                    , inheritance_modifier :: InheritanceModifier
                    , modifiers :: Modifiers
                    , ownership_modifier :: OwnershipModifier
                    , property_behavior_modifier :: PropertyBehaviorModifier
                    , type_parameters :: TypeParameters
                    )
                    a
              )
    , fields ::
          { name ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_identifier :: TypeIdentifier
                          , user_type :: UserType
                          )
                          a
                    )
          , value ::
                Array
                    ( VariantF
                          ( array_type :: ArrayType
                          , dictionary_type :: DictionaryType
                          , function_type :: FunctionType
                          , metatype :: Metatype
                          , opaque_type :: OpaqueType
                          , optional_type :: OptionalType
                          , protocol_composition_type :: ProtocolCompositionType
                          , tuple_type :: TupleType
                          , type_modifiers :: TypeModifiers
                          , user_type :: UserType
                          )
                          a
                    )
          }
    }

derive instance Functor TypealiasDeclaration

parseTypealiasDeclaration
    :: Partial => SyntaxNode -> TypealiasDeclaration SyntaxNode
parseTypealiasDeclaration syntaxNode = TypealiasDeclaration
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_identifier" -> inj
                          (Proxy :: Proxy "type_identifier")
                          (parseTypeIdentifier field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "name" syntaxNode
          , value:
                ( \field -> case type' field of
                      "array_type" -> inj (Proxy :: Proxy "array_type")
                          (parseArrayType field)
                      "dictionary_type" -> inj
                          (Proxy :: Proxy "dictionary_type")
                          (parseDictionaryType field)
                      "function_type" -> inj (Proxy :: Proxy "function_type")
                          (parseFunctionType field)
                      "metatype" -> inj (Proxy :: Proxy "metatype")
                          (parseMetatype field)
                      "opaque_type" -> inj (Proxy :: Proxy "opaque_type")
                          (parseOpaqueType field)
                      "optional_type" -> inj (Proxy :: Proxy "optional_type")
                          (parseOptionalType field)
                      "protocol_composition_type" -> inj
                          (Proxy :: Proxy "protocol_composition_type")
                          (parseProtocolCompositionType field)
                      "tuple_type" -> inj (Proxy :: Proxy "tuple_type")
                          (parseTupleType field)
                      "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                          (parseTypeModifiers field)
                      "user_type" -> inj (Proxy :: Proxy "user_type")
                          (parseUserType field)
                ) <$> arrayField "value" syntaxNode
          }
    , children:
          ( \child -> case type' child of
                "attribute" -> inj (Proxy :: Proxy "attribute")
                    (parseAttribute child)
                "inheritance_modifier" -> inj
                    (Proxy :: Proxy "inheritance_modifier")
                    (parseInheritanceModifier child)
                "modifiers" -> inj (Proxy :: Proxy "modifiers")
                    (parseModifiers child)
                "ownership_modifier" -> inj
                    (Proxy :: Proxy "ownership_modifier")
                    (parseOwnershipModifier child)
                "property_behavior_modifier" -> inj
                    (Proxy :: Proxy "property_behavior_modifier")
                    (parsePropertyBehaviorModifier child)
                "type_parameters" -> inj (Proxy :: Proxy "type_parameters")
                    (parseTypeParameters child)
          ) <$> children syntaxNode
    }

newtype UserType a = UserType
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( type_arguments :: TypeArguments
                    , type_identifier :: TypeIdentifier
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor UserType

parseUserType :: Partial => SyntaxNode -> UserType SyntaxNode
parseUserType syntaxNode = UserType
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "type_arguments" -> inj (Proxy :: Proxy "type_arguments")
                    (parseTypeArguments child)
                "type_identifier" -> inj (Proxy :: Proxy "type_identifier")
                    (parseTypeIdentifier child)
          ) <$> children syntaxNode
    }

newtype ValueArgument a = ValueArgument
    { value :: a
    , child :: Maybe (VariantF (type_modifiers :: TypeModifiers) a)
    , fields ::
          { name :: Maybe (VariantF (simple_identifier :: SimpleIdentifier) a)
          , reference_specifier ::
                Array (VariantF (simple_identifier :: SimpleIdentifier) a)
          , value ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          )
                          a
                    )
          }
    }

derive instance Functor ValueArgument

parseValueArgument :: Partial => SyntaxNode -> ValueArgument SyntaxNode
parseValueArgument syntaxNode = ValueArgument
    { value: syntaxNode
    , fields:
          { name:
                ( \field -> case type' field of
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                ) <$> nodeField "name" syntaxNode
          , reference_specifier:
                ( \field -> case type' field of
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                ) <$> arrayField "reference_specifier" syntaxNode
          , value:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                ) <$> arrayField "value" syntaxNode
          }
    , child:
          ( \child -> case type' child of
                "type_modifiers" -> inj (Proxy :: Proxy "type_modifiers")
                    (parseTypeModifiers child)
          ) <$> head (children syntaxNode)
    }

newtype ValueArguments a = ValueArguments
    { value :: a
    , children :: Array (VariantF (value_argument :: ValueArgument) a)
    , fields :: {}
    }

derive instance Functor ValueArguments

parseValueArguments :: Partial => SyntaxNode -> ValueArguments SyntaxNode
parseValueArguments syntaxNode = ValueArguments
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "value_argument" -> inj (Proxy :: Proxy "value_argument")
                    (parseValueArgument child)
          ) <$> children syntaxNode
    }

newtype ValueBindingPattern a = ValueBindingPattern
    { value :: a
    , child :: VariantF (non_binding_pattern :: NonBindingPattern) a
    , fields :: {}
    }

derive instance Functor ValueBindingPattern

parseValueBindingPattern
    :: Partial => SyntaxNode -> ValueBindingPattern SyntaxNode
parseValueBindingPattern syntaxNode = ValueBindingPattern
    { value: syntaxNode
    , fields: {}
    , child:
          ( \child -> case type' child of
                "non_binding_pattern" -> inj
                    (Proxy :: Proxy "non_binding_pattern")
                    (parseNonBindingPattern child)
          ) (Partial.head (children syntaxNode))
    }

newtype VisibilityModifier a = VisibilityModifier { value :: a, fields :: {} }

derive instance Functor VisibilityModifier

parseVisibilityModifier
    :: Partial => SyntaxNode -> VisibilityModifier SyntaxNode
parseVisibilityModifier syntaxNode = VisibilityModifier
    { value: syntaxNode, fields: {} }

newtype WhereClause a = WhereClause
    { value :: a
    , children ::
          Array
              ( VariantF
                    ( additive_expression :: AdditiveExpression
                    , array_literal :: ArrayLiteral
                    , as_expression :: AsExpression
                    , assignment :: Assignment
                    , await_expression :: AwaitExpression
                    , bang :: Bang
                    , bin_literal :: BinLiteral
                    , bitwise_operation :: BitwiseOperation
                    , boolean_literal :: BooleanLiteral
                    , call_expression :: CallExpression
                    , check_expression :: CheckExpression
                    , comparison_expression :: ComparisonExpression
                    , conjunction_expression :: ConjunctionExpression
                    , constructor_expression :: ConstructorExpression
                    , custom_operator :: CustomOperator
                    , dictionary_literal :: DictionaryLiteral
                    , disjunction_expression :: DisjunctionExpression
                    , equality_expression :: EqualityExpression
                    , fully_open_range :: FullyOpenRange
                    , hex_literal :: HexLiteral
                    , infix_expression :: InfixExpression
                    , integer_literal :: IntegerLiteral
                    , key_path_expression :: KeyPathExpression
                    , key_path_string_expression :: KeyPathStringExpression
                    , lambda_literal :: LambdaLiteral
                    , line_string_literal :: LineStringLiteral
                    , multi_line_string_literal :: MultiLineStringLiteral
                    , multiplicative_expression :: MultiplicativeExpression
                    , navigation_expression :: NavigationExpression
                    , nil_coalescing_expression :: NilCoalescingExpression
                    , oct_literal :: OctLiteral
                    , open_end_range_expression :: OpenEndRangeExpression
                    , open_start_range_expression :: OpenStartRangeExpression
                    , postfix_expression :: PostfixExpression
                    , prefix_expression :: PrefixExpression
                    , range_expression :: RangeExpression
                    , raw_string_literal :: RawStringLiteral
                    , real_literal :: RealLiteral
                    , selector_expression :: SelectorExpression
                    , self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    , super_expression :: SuperExpression
                    , ternary_expression :: TernaryExpression
                    , try_expression :: TryExpression
                    , tuple_expression :: TupleExpression
                    , where_keyword :: WhereKeyword
                    )
                    a
              )
    , fields :: {}
    }

derive instance Functor WhereClause

parseWhereClause :: Partial => SyntaxNode -> WhereClause SyntaxNode
parseWhereClause syntaxNode = WhereClause
    { value: syntaxNode
    , fields: {}
    , children:
          ( \child -> case type' child of
                "additive_expression" -> inj
                    (Proxy :: Proxy "additive_expression")
                    (parseAdditiveExpression child)
                "array_literal" -> inj (Proxy :: Proxy "array_literal")
                    (parseArrayLiteral child)
                "as_expression" -> inj (Proxy :: Proxy "as_expression")
                    (parseAsExpression child)
                "assignment" -> inj (Proxy :: Proxy "assignment")
                    (parseAssignment child)
                "await_expression" -> inj (Proxy :: Proxy "await_expression")
                    (parseAwaitExpression child)
                "bang" -> inj (Proxy :: Proxy "bang") (parseBang child)
                "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                    (parseBinLiteral child)
                "bitwise_operation" -> inj (Proxy :: Proxy "bitwise_operation")
                    (parseBitwiseOperation child)
                "boolean_literal" -> inj (Proxy :: Proxy "boolean_literal")
                    (parseBooleanLiteral child)
                "call_expression" -> inj (Proxy :: Proxy "call_expression")
                    (parseCallExpression child)
                "check_expression" -> inj (Proxy :: Proxy "check_expression")
                    (parseCheckExpression child)
                "comparison_expression" -> inj
                    (Proxy :: Proxy "comparison_expression")
                    (parseComparisonExpression child)
                "conjunction_expression" -> inj
                    (Proxy :: Proxy "conjunction_expression")
                    (parseConjunctionExpression child)
                "constructor_expression" -> inj
                    (Proxy :: Proxy "constructor_expression")
                    (parseConstructorExpression child)
                "custom_operator" -> inj (Proxy :: Proxy "custom_operator")
                    (parseCustomOperator child)
                "dictionary_literal" -> inj
                    (Proxy :: Proxy "dictionary_literal")
                    (parseDictionaryLiteral child)
                "disjunction_expression" -> inj
                    (Proxy :: Proxy "disjunction_expression")
                    (parseDisjunctionExpression child)
                "equality_expression" -> inj
                    (Proxy :: Proxy "equality_expression")
                    (parseEqualityExpression child)
                "fully_open_range" -> inj (Proxy :: Proxy "fully_open_range")
                    (parseFullyOpenRange child)
                "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                    (parseHexLiteral child)
                "infix_expression" -> inj (Proxy :: Proxy "infix_expression")
                    (parseInfixExpression child)
                "integer_literal" -> inj (Proxy :: Proxy "integer_literal")
                    (parseIntegerLiteral child)
                "key_path_expression" -> inj
                    (Proxy :: Proxy "key_path_expression")
                    (parseKeyPathExpression child)
                "key_path_string_expression" -> inj
                    (Proxy :: Proxy "key_path_string_expression")
                    (parseKeyPathStringExpression child)
                "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                    (parseLambdaLiteral child)
                "line_string_literal" -> inj
                    (Proxy :: Proxy "line_string_literal")
                    (parseLineStringLiteral child)
                "multi_line_string_literal" -> inj
                    (Proxy :: Proxy "multi_line_string_literal")
                    (parseMultiLineStringLiteral child)
                "multiplicative_expression" -> inj
                    (Proxy :: Proxy "multiplicative_expression")
                    (parseMultiplicativeExpression child)
                "navigation_expression" -> inj
                    (Proxy :: Proxy "navigation_expression")
                    (parseNavigationExpression child)
                "nil_coalescing_expression" -> inj
                    (Proxy :: Proxy "nil_coalescing_expression")
                    (parseNilCoalescingExpression child)
                "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                    (parseOctLiteral child)
                "open_end_range_expression" -> inj
                    (Proxy :: Proxy "open_end_range_expression")
                    (parseOpenEndRangeExpression child)
                "open_start_range_expression" -> inj
                    (Proxy :: Proxy "open_start_range_expression")
                    (parseOpenStartRangeExpression child)
                "postfix_expression" -> inj
                    (Proxy :: Proxy "postfix_expression")
                    (parsePostfixExpression child)
                "prefix_expression" -> inj (Proxy :: Proxy "prefix_expression")
                    (parsePrefixExpression child)
                "range_expression" -> inj (Proxy :: Proxy "range_expression")
                    (parseRangeExpression child)
                "raw_string_literal" -> inj
                    (Proxy :: Proxy "raw_string_literal")
                    (parseRawStringLiteral child)
                "real_literal" -> inj (Proxy :: Proxy "real_literal")
                    (parseRealLiteral child)
                "selector_expression" -> inj
                    (Proxy :: Proxy "selector_expression")
                    (parseSelectorExpression child)
                "self_expression" -> inj (Proxy :: Proxy "self_expression")
                    (parseSelfExpression child)
                "simple_identifier" -> inj (Proxy :: Proxy "simple_identifier")
                    (parseSimpleIdentifier child)
                "super_expression" -> inj (Proxy :: Proxy "super_expression")
                    (parseSuperExpression child)
                "ternary_expression" -> inj
                    (Proxy :: Proxy "ternary_expression")
                    (parseTernaryExpression child)
                "try_expression" -> inj (Proxy :: Proxy "try_expression")
                    (parseTryExpression child)
                "tuple_expression" -> inj (Proxy :: Proxy "tuple_expression")
                    (parseTupleExpression child)
                "where_keyword" -> inj (Proxy :: Proxy "where_keyword")
                    (parseWhereKeyword child)
          ) <$> children syntaxNode
    }

newtype WhileStatement a = WhileStatement
    { value :: a
    , child :: Maybe (VariantF (statements :: Statements) a)
    , fields ::
          { condition ::
                Array
                    ( VariantF
                          ( additive_expression :: AdditiveExpression
                          , array_literal :: ArrayLiteral
                          , as_expression :: AsExpression
                          , assignment :: Assignment
                          , availability_condition :: AvailabilityCondition
                          , await_expression :: AwaitExpression
                          , bang :: Bang
                          , bin_literal :: BinLiteral
                          , binding_pattern :: BindingPattern
                          , bitwise_operation :: BitwiseOperation
                          , boolean_literal :: BooleanLiteral
                          , call_expression :: CallExpression
                          , check_expression :: CheckExpression
                          , comparison_expression :: ComparisonExpression
                          , conjunction_expression :: ConjunctionExpression
                          , constructor_expression :: ConstructorExpression
                          , custom_operator :: CustomOperator
                          , dictionary_literal :: DictionaryLiteral
                          , disjunction_expression :: DisjunctionExpression
                          , equality_expression :: EqualityExpression
                          , fully_open_range :: FullyOpenRange
                          , hex_literal :: HexLiteral
                          , infix_expression :: InfixExpression
                          , integer_literal :: IntegerLiteral
                          , key_path_expression :: KeyPathExpression
                          , key_path_string_expression ::
                                KeyPathStringExpression
                          , lambda_literal :: LambdaLiteral
                          , line_string_literal :: LineStringLiteral
                          , multi_line_string_literal :: MultiLineStringLiteral
                          , multiplicative_expression ::
                                MultiplicativeExpression
                          , navigation_expression :: NavigationExpression
                          , nil_coalescing_expression :: NilCoalescingExpression
                          , oct_literal :: OctLiteral
                          , open_end_range_expression :: OpenEndRangeExpression
                          , open_start_range_expression ::
                                OpenStartRangeExpression
                          , postfix_expression :: PostfixExpression
                          , prefix_expression :: PrefixExpression
                          , range_expression :: RangeExpression
                          , raw_string_literal :: RawStringLiteral
                          , real_literal :: RealLiteral
                          , selector_expression :: SelectorExpression
                          , self_expression :: SelfExpression
                          , simple_identifier :: SimpleIdentifier
                          , super_expression :: SuperExpression
                          , ternary_expression :: TernaryExpression
                          , try_expression :: TryExpression
                          , tuple_expression :: TupleExpression
                          , type_annotation :: TypeAnnotation
                          , value_binding_pattern :: ValueBindingPattern
                          )
                          a
                    )
          }
    }

derive instance Functor WhileStatement

parseWhileStatement :: Partial => SyntaxNode -> WhileStatement SyntaxNode
parseWhileStatement syntaxNode = WhileStatement
    { value: syntaxNode
    , fields:
          { condition:
                ( \field -> case type' field of
                      "additive_expression" -> inj
                          (Proxy :: Proxy "additive_expression")
                          (parseAdditiveExpression field)
                      "array_literal" -> inj (Proxy :: Proxy "array_literal")
                          (parseArrayLiteral field)
                      "as_expression" -> inj (Proxy :: Proxy "as_expression")
                          (parseAsExpression field)
                      "assignment" -> inj (Proxy :: Proxy "assignment")
                          (parseAssignment field)
                      "availability_condition" -> inj
                          (Proxy :: Proxy "availability_condition")
                          (parseAvailabilityCondition field)
                      "await_expression" -> inj
                          (Proxy :: Proxy "await_expression")
                          (parseAwaitExpression field)
                      "bang" -> inj (Proxy :: Proxy "bang") (parseBang field)
                      "bin_literal" -> inj (Proxy :: Proxy "bin_literal")
                          (parseBinLiteral field)
                      "binding_pattern" -> inj
                          (Proxy :: Proxy "binding_pattern")
                          (parseBindingPattern field)
                      "bitwise_operation" -> inj
                          (Proxy :: Proxy "bitwise_operation")
                          (parseBitwiseOperation field)
                      "boolean_literal" -> inj
                          (Proxy :: Proxy "boolean_literal")
                          (parseBooleanLiteral field)
                      "call_expression" -> inj
                          (Proxy :: Proxy "call_expression")
                          (parseCallExpression field)
                      "check_expression" -> inj
                          (Proxy :: Proxy "check_expression")
                          (parseCheckExpression field)
                      "comparison_expression" -> inj
                          (Proxy :: Proxy "comparison_expression")
                          (parseComparisonExpression field)
                      "conjunction_expression" -> inj
                          (Proxy :: Proxy "conjunction_expression")
                          (parseConjunctionExpression field)
                      "constructor_expression" -> inj
                          (Proxy :: Proxy "constructor_expression")
                          (parseConstructorExpression field)
                      "custom_operator" -> inj
                          (Proxy :: Proxy "custom_operator")
                          (parseCustomOperator field)
                      "dictionary_literal" -> inj
                          (Proxy :: Proxy "dictionary_literal")
                          (parseDictionaryLiteral field)
                      "disjunction_expression" -> inj
                          (Proxy :: Proxy "disjunction_expression")
                          (parseDisjunctionExpression field)
                      "equality_expression" -> inj
                          (Proxy :: Proxy "equality_expression")
                          (parseEqualityExpression field)
                      "fully_open_range" -> inj
                          (Proxy :: Proxy "fully_open_range")
                          (parseFullyOpenRange field)
                      "hex_literal" -> inj (Proxy :: Proxy "hex_literal")
                          (parseHexLiteral field)
                      "infix_expression" -> inj
                          (Proxy :: Proxy "infix_expression")
                          (parseInfixExpression field)
                      "integer_literal" -> inj
                          (Proxy :: Proxy "integer_literal")
                          (parseIntegerLiteral field)
                      "key_path_expression" -> inj
                          (Proxy :: Proxy "key_path_expression")
                          (parseKeyPathExpression field)
                      "key_path_string_expression" -> inj
                          (Proxy :: Proxy "key_path_string_expression")
                          (parseKeyPathStringExpression field)
                      "lambda_literal" -> inj (Proxy :: Proxy "lambda_literal")
                          (parseLambdaLiteral field)
                      "line_string_literal" -> inj
                          (Proxy :: Proxy "line_string_literal")
                          (parseLineStringLiteral field)
                      "multi_line_string_literal" -> inj
                          (Proxy :: Proxy "multi_line_string_literal")
                          (parseMultiLineStringLiteral field)
                      "multiplicative_expression" -> inj
                          (Proxy :: Proxy "multiplicative_expression")
                          (parseMultiplicativeExpression field)
                      "navigation_expression" -> inj
                          (Proxy :: Proxy "navigation_expression")
                          (parseNavigationExpression field)
                      "nil_coalescing_expression" -> inj
                          (Proxy :: Proxy "nil_coalescing_expression")
                          (parseNilCoalescingExpression field)
                      "oct_literal" -> inj (Proxy :: Proxy "oct_literal")
                          (parseOctLiteral field)
                      "open_end_range_expression" -> inj
                          (Proxy :: Proxy "open_end_range_expression")
                          (parseOpenEndRangeExpression field)
                      "open_start_range_expression" -> inj
                          (Proxy :: Proxy "open_start_range_expression")
                          (parseOpenStartRangeExpression field)
                      "postfix_expression" -> inj
                          (Proxy :: Proxy "postfix_expression")
                          (parsePostfixExpression field)
                      "prefix_expression" -> inj
                          (Proxy :: Proxy "prefix_expression")
                          (parsePrefixExpression field)
                      "range_expression" -> inj
                          (Proxy :: Proxy "range_expression")
                          (parseRangeExpression field)
                      "raw_string_literal" -> inj
                          (Proxy :: Proxy "raw_string_literal")
                          (parseRawStringLiteral field)
                      "real_literal" -> inj (Proxy :: Proxy "real_literal")
                          (parseRealLiteral field)
                      "selector_expression" -> inj
                          (Proxy :: Proxy "selector_expression")
                          (parseSelectorExpression field)
                      "self_expression" -> inj
                          (Proxy :: Proxy "self_expression")
                          (parseSelfExpression field)
                      "simple_identifier" -> inj
                          (Proxy :: Proxy "simple_identifier")
                          (parseSimpleIdentifier field)
                      "super_expression" -> inj
                          (Proxy :: Proxy "super_expression")
                          (parseSuperExpression field)
                      "ternary_expression" -> inj
                          (Proxy :: Proxy "ternary_expression")
                          (parseTernaryExpression field)
                      "try_expression" -> inj (Proxy :: Proxy "try_expression")
                          (parseTryExpression field)
                      "tuple_expression" -> inj
                          (Proxy :: Proxy "tuple_expression")
                          (parseTupleExpression field)
                      "type_annotation" -> inj
                          (Proxy :: Proxy "type_annotation")
                          (parseTypeAnnotation field)
                      "value_binding_pattern" -> inj
                          (Proxy :: Proxy "value_binding_pattern")
                          (parseValueBindingPattern field)
                ) <$> arrayField "condition" syntaxNode
          }
    , child:
          ( \child -> case type' child of
                "statements" -> inj (Proxy :: Proxy "statements")
                    (parseStatements child)
          ) <$> head (children syntaxNode)
    }

newtype Bang a = Bang { value :: a }

derive instance Functor Bang

parseBang :: Partial => SyntaxNode -> Bang SyntaxNode
parseBang syntaxNode = Bang { value: syntaxNode }

newtype BinLiteral a = BinLiteral { value :: a }

derive instance Functor BinLiteral

parseBinLiteral :: Partial => SyntaxNode -> BinLiteral SyntaxNode
parseBinLiteral syntaxNode = BinLiteral { value: syntaxNode }

newtype CatchKeyword a = CatchKeyword { value :: a }

derive instance Functor CatchKeyword

parseCatchKeyword :: Partial => SyntaxNode -> CatchKeyword SyntaxNode
parseCatchKeyword syntaxNode = CatchKeyword { value: syntaxNode }

newtype Comment a = Comment { value :: a }

derive instance Functor Comment

parseComment :: Partial => SyntaxNode -> Comment SyntaxNode
parseComment syntaxNode = Comment { value: syntaxNode }

newtype DefaultKeyword a = DefaultKeyword { value :: a }

derive instance Functor DefaultKeyword

parseDefaultKeyword :: Partial => SyntaxNode -> DefaultKeyword SyntaxNode
parseDefaultKeyword syntaxNode = DefaultKeyword { value: syntaxNode }

newtype Diagnostic a = Diagnostic { value :: a }

derive instance Functor Diagnostic

parseDiagnostic :: Partial => SyntaxNode -> Diagnostic SyntaxNode
parseDiagnostic syntaxNode = Diagnostic { value: syntaxNode }

newtype Directive a = Directive { value :: a }

derive instance Functor Directive

parseDirective :: Partial => SyntaxNode -> Directive SyntaxNode
parseDirective syntaxNode = Directive { value: syntaxNode }

newtype Else a = Else { value :: a }

derive instance Functor Else

parseElse :: Partial => SyntaxNode -> Else SyntaxNode
parseElse syntaxNode = Else { value: syntaxNode }

newtype HexLiteral a = HexLiteral { value :: a }

derive instance Functor HexLiteral

parseHexLiteral :: Partial => SyntaxNode -> HexLiteral SyntaxNode
parseHexLiteral syntaxNode = HexLiteral { value: syntaxNode }

newtype IntegerLiteral a = IntegerLiteral { value :: a }

derive instance Functor IntegerLiteral

parseIntegerLiteral :: Partial => SyntaxNode -> IntegerLiteral SyntaxNode
parseIntegerLiteral syntaxNode = IntegerLiteral { value: syntaxNode }

newtype MultilineComment a = MultilineComment { value :: a }

derive instance Functor MultilineComment

parseMultilineComment :: Partial => SyntaxNode -> MultilineComment SyntaxNode
parseMultilineComment syntaxNode = MultilineComment { value: syntaxNode }

newtype OctLiteral a = OctLiteral { value :: a }

derive instance Functor OctLiteral

parseOctLiteral :: Partial => SyntaxNode -> OctLiteral SyntaxNode
parseOctLiteral syntaxNode = OctLiteral { value: syntaxNode }

newtype PropertyBehaviorModifier a = PropertyBehaviorModifier { value :: a }

derive instance Functor PropertyBehaviorModifier

parsePropertyBehaviorModifier
    :: Partial => SyntaxNode -> PropertyBehaviorModifier SyntaxNode
parsePropertyBehaviorModifier syntaxNode = PropertyBehaviorModifier
    { value: syntaxNode }

newtype RawStrContinuingIndicator a = RawStrContinuingIndicator { value :: a }

derive instance Functor RawStrContinuingIndicator

parseRawStrContinuingIndicator
    :: Partial => SyntaxNode -> RawStrContinuingIndicator SyntaxNode
parseRawStrContinuingIndicator syntaxNode = RawStrContinuingIndicator
    { value: syntaxNode }

newtype RawStrEndPart a = RawStrEndPart { value :: a }

derive instance Functor RawStrEndPart

parseRawStrEndPart :: Partial => SyntaxNode -> RawStrEndPart SyntaxNode
parseRawStrEndPart syntaxNode = RawStrEndPart { value: syntaxNode }

newtype RawStrInterpolationStart a = RawStrInterpolationStart { value :: a }

derive instance Functor RawStrInterpolationStart

parseRawStrInterpolationStart
    :: Partial => SyntaxNode -> RawStrInterpolationStart SyntaxNode
parseRawStrInterpolationStart syntaxNode = RawStrInterpolationStart
    { value: syntaxNode }

newtype RawStrPart a = RawStrPart { value :: a }

derive instance Functor RawStrPart

parseRawStrPart :: Partial => SyntaxNode -> RawStrPart SyntaxNode
parseRawStrPart syntaxNode = RawStrPart { value: syntaxNode }

newtype RealLiteral a = RealLiteral { value :: a }

derive instance Functor RealLiteral

parseRealLiteral :: Partial => SyntaxNode -> RealLiteral SyntaxNode
parseRealLiteral syntaxNode = RealLiteral { value: syntaxNode }

newtype StatementLabel a = StatementLabel { value :: a }

derive instance Functor StatementLabel

parseStatementLabel :: Partial => SyntaxNode -> StatementLabel SyntaxNode
parseStatementLabel syntaxNode = StatementLabel { value: syntaxNode }

newtype ThrowKeyword a = ThrowKeyword { value :: a }

derive instance Functor ThrowKeyword

parseThrowKeyword :: Partial => SyntaxNode -> ThrowKeyword SyntaxNode
parseThrowKeyword syntaxNode = ThrowKeyword { value: syntaxNode }

newtype WhereKeyword a = WhereKeyword { value :: a }

derive instance Functor WhereKeyword

parseWhereKeyword :: Partial => SyntaxNode -> WhereKeyword SyntaxNode
parseWhereKeyword syntaxNode = WhereKeyword { value: syntaxNode }

newtype WildcardPattern a = WildcardPattern { value :: a }

derive instance Functor WildcardPattern

parseWildcardPattern :: Partial => SyntaxNode -> WildcardPattern SyntaxNode
parseWildcardPattern syntaxNode = WildcardPattern { value: syntaxNode }
