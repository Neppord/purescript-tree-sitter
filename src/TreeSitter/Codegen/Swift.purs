module TreeSitteer.Codegen.Swift where

import Prelude
import Data.Array
import Data.Functor.Variant
import Data.Maybe

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
          , op :: VariantF () a
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
newtype AsOperator a = AsOperator { value :: a, fields :: {} }

derive instance Functor AsOperator
newtype Assignment a = Assignment
    { value :: a
    , fields ::
          { operator :: VariantF () a
          , result ::
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
newtype Async a = Async { value :: a, fields :: {} }

derive instance Functor Async
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
          , op :: VariantF () a
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
newtype BooleanLiteral a = BooleanLiteral { value :: a, fields :: {} }

derive instance Functor BooleanLiteral
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
          , op :: VariantF () a
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
          , declaration_kind :: VariantF () a
          , name ::
                VariantF
                    (type_identifier :: TypeIdentifier, user_type :: UserType)
                    a
          }
    }

derive instance Functor ClassDeclaration
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
          , op :: VariantF () a
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
          , op :: VariantF () a
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
newtype CustomOperator a = CustomOperator { value :: a, fields :: {} }

derive instance Functor CustomOperator
newtype DeinitDeclaration a = DeinitDeclaration
    { value :: a
    , child :: Maybe (VariantF (modifiers :: Modifiers) a)
    , fields :: { body :: VariantF (function_body :: FunctionBody) a }
    }

derive instance Functor DeinitDeclaration
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
          , op :: VariantF () a
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
newtype DoStatement a = DoStatement
    { value :: a
    , children ::
          Array
              (VariantF (catch_block :: CatchBlock, statements :: Statements) a)
    , fields :: {}
    }

derive instance Functor DoStatement
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
          , op :: VariantF () a
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
newtype FullyOpenRange a = FullyOpenRange { value :: a, fields :: {} }

derive instance Functor FullyOpenRange
newtype FunctionBody a = FunctionBody
    { value :: a
    , child :: Maybe (VariantF (statements :: Statements) a)
    , fields :: {}
    }

derive instance Functor FunctionBody
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
newtype FunctionModifier a = FunctionModifier { value :: a, fields :: {} }

derive instance Functor FunctionModifier
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
newtype Identifier a = Identifier
    { value :: a
    , children :: Array (VariantF (simple_identifier :: SimpleIdentifier) a)
    , fields :: {}
    }

derive instance Functor Identifier
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
newtype ImportDeclaration a = ImportDeclaration
    { value :: a
    , children ::
          Array (VariantF (identifier :: Identifier, modifiers :: Modifiers) a)
    , fields :: {}
    }

derive instance Functor ImportDeclaration
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
newtype InheritanceModifier a = InheritanceModifier { value :: a, fields :: {} }

derive instance Functor InheritanceModifier
newtype InheritanceSpecifier a = InheritanceSpecifier
    { value :: a
    , fields ::
          { inherits_from ::
                VariantF (function_type :: FunctionType, user_type :: UserType)
                    a
          }
    }

derive instance Functor InheritanceSpecifier
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
newtype LambdaFunctionTypeParameters a = LambdaFunctionTypeParameters
    { value :: a
    , children :: Array (VariantF (lambda_parameter :: LambdaParameter) a)
    , fields :: {}
    }

derive instance Functor LambdaFunctionTypeParameters
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
newtype LineStrText a = LineStrText { value :: a, fields :: {} }

derive instance Functor LineStrText
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
newtype MemberModifier a = MemberModifier { value :: a, fields :: {} }

derive instance Functor MemberModifier
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
newtype ModifySpecifier a = ModifySpecifier
    { value :: a
    , child :: Maybe (VariantF (mutation_modifier :: MutationModifier) a)
    , fields :: {}
    }

derive instance Functor ModifySpecifier
newtype MultiLineStrText a = MultiLineStrText { value :: a, fields :: {} }

derive instance Functor MultiLineStrText
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
          , op :: VariantF () a
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
newtype MutationModifier a = MutationModifier { value :: a, fields :: {} }

derive instance Functor MutationModifier
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
newtype OpaqueType a = OpaqueType
    { value :: a, child :: VariantF (user_type :: UserType) a, fields :: {} }

derive instance Functor OpaqueType
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
newtype OwnershipModifier a = OwnershipModifier { value :: a, fields :: {} }

derive instance Functor OwnershipModifier
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
newtype ParameterModifier a = ParameterModifier { value :: a, fields :: {} }

derive instance Functor ParameterModifier
newtype ParameterModifiers a = ParameterModifiers
    { value :: a
    , children :: Array (VariantF (parameter_modifier :: ParameterModifier) a)
    , fields :: {}
    }

derive instance Functor ParameterModifiers
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
newtype PropertyModifier a = PropertyModifier { value :: a, fields :: {} }

derive instance Functor PropertyModifier
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
          , declaration_kind :: VariantF () a
          , name :: VariantF (type_identifier :: TypeIdentifier) a
          }
    }

derive instance Functor ProtocolDeclaration
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
          , op :: VariantF () a
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
newtype SelfExpression a = SelfExpression { value :: a, fields :: {} }

derive instance Functor SelfExpression
newtype SetterSpecifier a = SetterSpecifier
    { value :: a
    , child :: Maybe (VariantF (mutation_modifier :: MutationModifier) a)
    , fields :: {}
    }

derive instance Functor SetterSpecifier
newtype ShebangLine a = ShebangLine { value :: a, fields :: {} }

derive instance Functor ShebangLine
newtype SimpleIdentifier a = SimpleIdentifier { value :: a, fields :: {} }

derive instance Functor SimpleIdentifier
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
newtype StrEscapedChar a = StrEscapedChar { value :: a, fields :: {} }

derive instance Functor StrEscapedChar
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
newtype SuperExpression a = SuperExpression { value :: a, fields :: {} }

derive instance Functor SuperExpression
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
newtype Throws a = Throws { value :: a, fields :: {} }

derive instance Functor Throws
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
newtype TupleType a = TupleType
    { value :: a
    , fields ::
          { element :: Array (VariantF (tuple_type_item :: TupleTypeItem) a) }
    }

derive instance Functor TupleType
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
newtype TypeIdentifier a = TypeIdentifier { value :: a, fields :: {} }

derive instance Functor TypeIdentifier
newtype TypeModifiers a = TypeModifiers
    { value :: a
    , children :: Array (VariantF (attribute :: Attribute) a)
    , fields :: {}
    }

derive instance Functor TypeModifiers
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
newtype TypeParameterModifiers a = TypeParameterModifiers
    { value :: a
    , children :: Array (VariantF (attribute :: Attribute) a)
    , fields :: {}
    }

derive instance Functor TypeParameterModifiers
newtype TypeParameters a = TypeParameters
    { value :: a
    , children :: Array (VariantF (type_parameter :: TypeParameter) a)
    , fields :: {}
    }

derive instance Functor TypeParameters
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
newtype ValueArguments a = ValueArguments
    { value :: a
    , children :: Array (VariantF (value_argument :: ValueArgument) a)
    , fields :: {}
    }

derive instance Functor ValueArguments
newtype ValueBindingPattern a = ValueBindingPattern
    { value :: a
    , child :: VariantF (non_binding_pattern :: NonBindingPattern) a
    , fields :: {}
    }

derive instance Functor ValueBindingPattern
newtype VisibilityModifier a = VisibilityModifier { value :: a, fields :: {} }

derive instance Functor VisibilityModifier
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
newtype Bang a = Bang { value :: a }

derive instance Functor Bang
newtype BinLiteral a = BinLiteral { value :: a }

derive instance Functor BinLiteral
newtype CatchKeyword a = CatchKeyword { value :: a }

derive instance Functor CatchKeyword
newtype Comment a = Comment { value :: a }

derive instance Functor Comment
newtype DefaultKeyword a = DefaultKeyword { value :: a }

derive instance Functor DefaultKeyword
newtype Diagnostic a = Diagnostic { value :: a }

derive instance Functor Diagnostic
newtype Directive a = Directive { value :: a }

derive instance Functor Directive
newtype Else a = Else { value :: a }

derive instance Functor Else
newtype HexLiteral a = HexLiteral { value :: a }

derive instance Functor HexLiteral
newtype IntegerLiteral a = IntegerLiteral { value :: a }

derive instance Functor IntegerLiteral
newtype MultilineComment a = MultilineComment { value :: a }

derive instance Functor MultilineComment
newtype OctLiteral a = OctLiteral { value :: a }

derive instance Functor OctLiteral
newtype PropertyBehaviorModifier a = PropertyBehaviorModifier { value :: a }

derive instance Functor PropertyBehaviorModifier
newtype RawStrContinuingIndicator a = RawStrContinuingIndicator { value :: a }

derive instance Functor RawStrContinuingIndicator
newtype RawStrEndPart a = RawStrEndPart { value :: a }

derive instance Functor RawStrEndPart
newtype RawStrInterpolationStart a = RawStrInterpolationStart { value :: a }

derive instance Functor RawStrInterpolationStart
newtype RawStrPart a = RawStrPart { value :: a }

derive instance Functor RawStrPart
newtype RealLiteral a = RealLiteral { value :: a }

derive instance Functor RealLiteral
newtype StatementLabel a = StatementLabel { value :: a }

derive instance Functor StatementLabel
newtype ThrowKeyword a = ThrowKeyword { value :: a }

derive instance Functor ThrowKeyword
newtype WhereKeyword a = WhereKeyword { value :: a }

derive instance Functor WhereKeyword
newtype WildcardPattern a = WildcardPattern { value :: a }

derive instance Functor WildcardPattern
