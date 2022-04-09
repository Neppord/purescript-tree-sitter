module TreeSitteer.Codegen.Swift where

import Data.Array
import Data.Maybe
import Data.Variant

newtype AdditiveExpression = AdditiveExpression
    { fields ::
          { lhs ::
                Array
                    ( Variant
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
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype ArrayLiteral = ArrayLiteral
    { fields ::
          { element ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype ArrayType = ArrayType
    { fields ::
          { element ::
                Array
                    ( Variant
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
                    )
          , name ::
                Variant
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
          }
    }

newtype AsExpression = AsExpression
    { child :: Variant (as_operator :: AsOperator)
    , fields ::
          { expr ::
                Array
                    ( Variant
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
                    )
          , name ::
                Variant
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
          , type ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype AsOperator = AsOperator { fields :: {} }
newtype Assignment = Assignment
    { fields ::
          { operator :: Variant ()
          , result ::
                Array
                    ( Variant
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
                    )
          , target ::
                Variant
                    ( directly_assignable_expression ::
                          DirectlyAssignableExpression
                    )
          }
    }

newtype AssociatedtypeDeclaration = AssociatedtypeDeclaration
    { children ::
          Array
              ( Variant
                    ( modifiers :: Modifiers
                    , type_constraints :: TypeConstraints
                    )
              )
    , fields ::
          { default_value ::
                Array
                    ( Variant
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
                    )
          , must_inherit ::
                Array
                    ( Variant
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
                    )
          , name ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype Async = Async { fields :: {} }
newtype Attribute = Attribute
    { children ::
          Array
              ( Variant
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
              )
    , fields :: {}
    }

newtype AvailabilityCondition = AvailabilityCondition
    { children ::
          Array
              ( Variant
                    ( identifier :: Identifier
                    , integer_literal :: IntegerLiteral
                    )
              )
    , fields :: {}
    }

newtype AwaitExpression = AwaitExpression
    { fields ::
          { expr ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype BindingPattern = BindingPattern
    { children ::
          Array
              ( Variant
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
              )
    , fields ::
          { name ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype BitwiseOperation = BitwiseOperation
    { fields ::
          { lhs ::
                Array
                    ( Variant
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
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype BooleanLiteral = BooleanLiteral { fields :: {} }
newtype CallExpression = CallExpression
    { children ::
          Array
              ( Variant
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
              )
    , fields :: {}
    }

newtype CallSuffix = CallSuffix
    { children ::
          Array
              ( Variant
                    ( lambda_literal :: LambdaLiteral
                    , value_arguments :: ValueArguments
                    )
              )
    , fields ::
          { name :: Array (Variant (simple_identifier :: SimpleIdentifier)) }
    }

newtype CaptureList = CaptureList
    { children ::
          Array
              ( Variant
                    ( attribute :: Attribute
                    , capture_list_item :: CaptureListItem
                    )
              )
    , fields :: {}
    }

newtype CaptureListItem = CaptureListItem
    { child :: Maybe (Variant (ownership_modifier :: OwnershipModifier))
    , fields ::
          { name ::
                Variant
                    ( self_expression :: SelfExpression
                    , simple_identifier :: SimpleIdentifier
                    )
          , value ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype CatchBlock = CatchBlock
    { children ::
          Array
              ( Variant
                    ( catch_keyword :: CatchKeyword
                    , statements :: Statements
                    , where_clause :: WhereClause
                    )
              )
    , fields :: { error :: Maybe (Variant (binding_pattern :: BindingPattern)) }
    }

newtype CheckExpression = CheckExpression
    { fields ::
          { name ::
                Variant
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
          , op :: Variant ()
          , target ::
                Array
                    ( Variant
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
                    )
          , type ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype ClassBody = ClassBody
    { children ::
          Array
              ( Variant
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
              )
    , fields :: {}
    }

newtype ClassDeclaration = ClassDeclaration
    { children ::
          Array
              ( Variant
                    ( attribute :: Attribute
                    , inheritance_modifier :: InheritanceModifier
                    , inheritance_specifier :: InheritanceSpecifier
                    , modifiers :: Modifiers
                    , ownership_modifier :: OwnershipModifier
                    , property_behavior_modifier :: PropertyBehaviorModifier
                    , type_constraints :: TypeConstraints
                    , type_parameters :: TypeParameters
                    )
              )
    , fields ::
          { body ::
                Variant
                    (class_body :: ClassBody, enum_class_body :: EnumClassBody)
          , declaration_kind :: Variant ()
          , name ::
                Variant
                    (type_identifier :: TypeIdentifier, user_type :: UserType)
          }
    }

newtype ComparisonExpression = ComparisonExpression
    { fields ::
          { lhs ::
                Array
                    ( Variant
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
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype ComputedGetter = ComputedGetter
    { children ::
          Array
              ( Variant
                    ( attribute :: Attribute
                    , getter_specifier :: GetterSpecifier
                    , statements :: Statements
                    )
              )
    , fields :: {}
    }

newtype ComputedModify = ComputedModify
    { children ::
          Array
              ( Variant
                    ( attribute :: Attribute
                    , modify_specifier :: ModifySpecifier
                    , statements :: Statements
                    )
              )
    , fields :: {}
    }

newtype ComputedProperty = ComputedProperty
    { children ::
          Array
              ( Variant
                    ( computed_getter :: ComputedGetter
                    , computed_modify :: ComputedModify
                    , computed_setter :: ComputedSetter
                    , statements :: Statements
                    )
              )
    , fields :: {}
    }

newtype ComputedSetter = ComputedSetter
    { children ::
          Array
              ( Variant
                    ( attribute :: Attribute
                    , setter_specifier :: SetterSpecifier
                    , simple_identifier :: SimpleIdentifier
                    , statements :: Statements
                    )
              )
    , fields :: {}
    }

newtype ConjunctionExpression = ConjunctionExpression
    { fields ::
          { lhs ::
                Array
                    ( Variant
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
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype ConstructorExpression = ConstructorExpression
    { child :: Variant (constructor_suffix :: ConstructorSuffix)
    , fields ::
          { constructed_type ::
                Variant
                    ( array_type :: ArrayType
                    , dictionary_type :: DictionaryType
                    , user_type :: UserType
                    )
          }
    }

newtype ConstructorSuffix = ConstructorSuffix
    { child ::
          Variant
              ( lambda_literal :: LambdaLiteral
              , value_arguments :: ValueArguments
              )
    , fields :: {}
    }

newtype ControlTransferStatement = ControlTransferStatement
    { children ::
          Array
              ( Variant
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
              )
    , fields ::
          { result ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype CustomOperator = CustomOperator { fields :: {} }
newtype DeinitDeclaration = DeinitDeclaration
    { child :: Maybe (Variant (modifiers :: Modifiers))
    , fields :: { body :: Variant (function_body :: FunctionBody) }
    }

newtype DictionaryLiteral = DictionaryLiteral
    { fields ::
          { key ::
                Array
                    ( Variant
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
                    )
          , value ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype DictionaryType = DictionaryType
    { fields ::
          { key ::
                Array
                    ( Variant
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
                    )
          , name ::
                Array
                    ( Variant
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
                    )
          , value ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype DirectlyAssignableExpression = DirectlyAssignableExpression
    { child ::
          Variant
              ( call_expression :: CallExpression
              , navigation_expression :: NavigationExpression
              , self_expression :: SelfExpression
              , simple_identifier :: SimpleIdentifier
              , tuple_expression :: TupleExpression
              )
    , fields :: {}
    }

newtype DisjunctionExpression = DisjunctionExpression
    { fields ::
          { lhs ::
                Array
                    ( Variant
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
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype DoStatement = DoStatement
    { children ::
          Array (Variant (catch_block :: CatchBlock, statements :: Statements))
    , fields :: {}
    }

newtype EnumClassBody = EnumClassBody
    { children ::
          Array
              ( Variant
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
              )
    , fields :: {}
    }

newtype EnumEntry = EnumEntry
    { child :: Maybe (Variant (modifiers :: Modifiers))
    , fields ::
          { data_contents ::
                Array (Variant (enum_type_parameters :: EnumTypeParameters))
          , name :: Array (Variant (simple_identifier :: SimpleIdentifier))
          , raw_value ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype EnumTypeParameters = EnumTypeParameters
    { children ::
          Array
              ( Variant
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
              )
    , fields ::
          { name ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype EqualityConstraint = EqualityConstraint
    { children :: Array (Variant (attribute :: Attribute))
    , fields ::
          { constrained_type :: Variant (identifier :: Identifier)
          , must_equal ::
                Array
                    ( Variant
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
                    )
          , name ::
                Variant
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
          }
    }

newtype EqualityExpression = EqualityExpression
    { fields ::
          { lhs ::
                Array
                    ( Variant
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
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype ForStatement = ForStatement
    { children ::
          Array
              ( Variant
                    ( statements :: Statements
                    , type_annotation :: TypeAnnotation
                    , where_clause :: WhereClause
                    )
              )
    , fields ::
          { collection ::
                Array
                    ( Variant
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
                    )
          , item ::
                Array
                    ( Variant
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
                    )
          , name ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype FullyOpenRange = FullyOpenRange { fields :: {} }
newtype FunctionBody = FunctionBody
    { child :: Maybe (Variant (statements :: Statements)), fields :: {} }

newtype FunctionDeclaration = FunctionDeclaration
    { children ::
          Array
              ( Variant
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
              )
    , fields ::
          { body :: Variant (function_body :: FunctionBody)
          , default_value ::
                Array
                    ( Variant
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
                    )
          , name ::
                Array
                    ( Variant
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
                    )
          , return_type ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype FunctionModifier = FunctionModifier { fields :: {} }
newtype FunctionType = FunctionType
    { children :: Array (Variant (async :: Async, throws :: Throws))
    , fields ::
          { name ::
                Variant
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
          , params :: Variant (tuple_type :: TupleType)
          , return_type ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype GetterSpecifier = GetterSpecifier
    { children ::
          Array
              ( Variant
                    ( async :: Async
                    , mutation_modifier :: MutationModifier
                    , throws :: Throws
                    )
              )
    , fields :: {}
    }

newtype GuardStatement = GuardStatement
    { children :: Array (Variant (else :: Else, statements :: Statements))
    , fields ::
          { condition ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype Identifier = Identifier
    { children :: Array (Variant (simple_identifier :: SimpleIdentifier))
    , fields :: {}
    }

newtype IfStatement = IfStatement
    { children ::
          Array
              ( Variant
                    ( else :: Else
                    , if_statement :: IfStatement
                    , statements :: Statements
                    )
              )
    , fields ::
          { condition ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype ImportDeclaration = ImportDeclaration
    { children ::
          Array (Variant (identifier :: Identifier, modifiers :: Modifiers))
    , fields :: {}
    }

newtype InfixExpression = InfixExpression
    { fields ::
          { lhs ::
                Array
                    ( Variant
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
                    )
          , op :: Variant (custom_operator :: CustomOperator)
          , rhs ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype InheritanceConstraint = InheritanceConstraint
    { children :: Array (Variant (attribute :: Attribute))
    , fields ::
          { constrained_type :: Variant (identifier :: Identifier)
          , inherits_from ::
                Array
                    ( Variant
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
                    )
          , name ::
                Variant
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
          }
    }

newtype InheritanceModifier = InheritanceModifier { fields :: {} }
newtype InheritanceSpecifier = InheritanceSpecifier
    { fields ::
          { inherits_from ::
                Variant (function_type :: FunctionType, user_type :: UserType)
          }
    }

newtype InterpolatedExpression = InterpolatedExpression
    { child :: Maybe (Variant (type_modifiers :: TypeModifiers))
    , fields ::
          { name :: Maybe (Variant (simple_identifier :: SimpleIdentifier))
          , reference_specifier ::
                Array (Variant (simple_identifier :: SimpleIdentifier))
          , value ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype KeyPathExpression = KeyPathExpression
    { children ::
          Array
              ( Variant
                    ( array_type :: ArrayType
                    , bang :: Bang
                    , dictionary_type :: DictionaryType
                    , simple_identifier :: SimpleIdentifier
                    , type_arguments :: TypeArguments
                    , type_identifier :: TypeIdentifier
                    , value_argument :: ValueArgument
                    )
              )
    , fields :: {}
    }

newtype KeyPathStringExpression = KeyPathStringExpression
    { children ::
          Array
              ( Variant
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
              )
    , fields :: {}
    }

newtype LambdaFunctionType = LambdaFunctionType
    { children ::
          Array
              ( Variant
                    ( async :: Async
                    , lambda_function_type_parameters ::
                          LambdaFunctionTypeParameters
                    , throws :: Throws
                    )
              )
    , fields ::
          { name ::
                Maybe
                    ( Variant
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
                    )
          , return_type ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype LambdaFunctionTypeParameters = LambdaFunctionTypeParameters
    { children :: Array (Variant (lambda_parameter :: LambdaParameter))
    , fields :: {}
    }

newtype LambdaLiteral = LambdaLiteral
    { child :: Maybe (Variant (statements :: Statements))
    , fields ::
          { captures :: Maybe (Variant (capture_list :: CaptureList))
          , type :: Maybe (Variant (lambda_function_type :: LambdaFunctionType))
          }
    }

newtype LambdaParameter = LambdaParameter
    { children ::
          Array
              ( Variant
                    ( attribute :: Attribute
                    , parameter_modifiers :: ParameterModifiers
                    , self_expression :: SelfExpression
                    )
              )
    , fields ::
          { external_name ::
                Maybe (Variant (simple_identifier :: SimpleIdentifier))
          , name ::
                Array
                    ( Variant
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
                    )
          , type ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype LineStrText = LineStrText { fields :: {} }
newtype LineStringLiteral = LineStringLiteral
    { fields ::
          { interpolation ::
                Array
                    ( Variant
                          (interpolated_expression :: InterpolatedExpression)
                    )
          , text ::
                Array
                    ( Variant
                          ( line_str_text :: LineStrText
                          , str_escaped_char :: StrEscapedChar
                          )
                    )
          }
    }

newtype MemberModifier = MemberModifier { fields :: {} }
newtype Metatype = Metatype
    { child ::
          Variant
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
    , fields :: {}
    }

newtype Modifiers = Modifiers
    { children ::
          Array
              ( Variant
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
              )
    , fields :: {}
    }

newtype ModifySpecifier = ModifySpecifier
    { child :: Maybe (Variant (mutation_modifier :: MutationModifier))
    , fields :: {}
    }

newtype MultiLineStrText = MultiLineStrText { fields :: {} }
newtype MultiLineStringLiteral = MultiLineStringLiteral
    { fields ::
          { interpolation ::
                Array
                    ( Variant
                          (interpolated_expression :: InterpolatedExpression)
                    )
          , text ::
                Array
                    ( Variant
                          ( multi_line_str_text :: MultiLineStrText
                          , str_escaped_char :: StrEscapedChar
                          )
                    )
          }
    }

newtype MultiplicativeExpression = MultiplicativeExpression
    { fields ::
          { lhs ::
                Array
                    ( Variant
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
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype MutationModifier = MutationModifier { fields :: {} }
newtype NavigationExpression = NavigationExpression
    { fields ::
          { suffix :: Variant (navigation_suffix :: NavigationSuffix)
          , target ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype NavigationSuffix = NavigationSuffix
    { fields ::
          { suffix ::
                Variant
                    ( integer_literal :: IntegerLiteral
                    , simple_identifier :: SimpleIdentifier
                    )
          }
    }

newtype NilCoalescingExpression = NilCoalescingExpression
    { fields ::
          { if_nil ::
                Array
                    ( Variant
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
                    )
          , value ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype NonBindingPattern = NonBindingPattern
    { children ::
          Array
              ( Variant
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
              )
    , fields ::
          { name ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype OpaqueType = OpaqueType
    { child :: Variant (user_type :: UserType), fields :: {} }

newtype OpenEndRangeExpression = OpenEndRangeExpression
    { fields ::
          { start ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype OpenStartRangeExpression = OpenStartRangeExpression
    { fields ::
          { end ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype OperatorDeclaration = OperatorDeclaration
    { children ::
          Array
              ( Variant
                    ( custom_operator :: CustomOperator
                    , simple_identifier :: SimpleIdentifier
                    )
              )
    , fields :: {}
    }

newtype OptionalType = OptionalType
    { fields ::
          { wrapped ::
                Variant
                    ( array_type :: ArrayType
                    , dictionary_type :: DictionaryType
                    , tuple_type :: TupleType
                    , user_type :: UserType
                    )
          }
    }

newtype OwnershipModifier = OwnershipModifier { fields :: {} }
newtype Parameter = Parameter
    { child :: Maybe (Variant (parameter_modifiers :: ParameterModifiers))
    , fields ::
          { external_name ::
                Maybe (Variant (simple_identifier :: SimpleIdentifier))
          , name ::
                Array
                    ( Variant
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
                    )
          , type ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype ParameterModifier = ParameterModifier { fields :: {} }
newtype ParameterModifiers = ParameterModifiers
    { children :: Array (Variant (parameter_modifier :: ParameterModifier))
    , fields :: {}
    }

newtype PostfixExpression = PostfixExpression
    { fields ::
          { operation :: Variant (bang :: Bang)
          , target ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype PrecedenceGroupAttribute = PrecedenceGroupAttribute
    { children ::
          Array
              ( Variant
                    ( boolean_literal :: BooleanLiteral
                    , simple_identifier :: SimpleIdentifier
                    )
              )
    , fields :: {}
    }

newtype PrecedenceGroupAttributes = PrecedenceGroupAttributes
    { children ::
          Array
              (Variant (precedence_group_attribute :: PrecedenceGroupAttribute))
    , fields :: {}
    }

newtype PrecedenceGroupDeclaration = PrecedenceGroupDeclaration
    { children ::
          Array
              ( Variant
                    ( precedence_group_attributes :: PrecedenceGroupAttributes
                    , simple_identifier :: SimpleIdentifier
                    )
              )
    , fields :: {}
    }

newtype PrefixExpression = PrefixExpression
    { fields ::
          { operation ::
                Variant (bang :: Bang, custom_operator :: CustomOperator)
          , target ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype PropertyDeclaration = PropertyDeclaration
    { children ::
          Array
              ( Variant
                    ( attribute :: Attribute
                    , inheritance_modifier :: InheritanceModifier
                    , modifiers :: Modifiers
                    , ownership_modifier :: OwnershipModifier
                    , property_behavior_modifier :: PropertyBehaviorModifier
                    , type_annotation :: TypeAnnotation
                    , type_constraints :: TypeConstraints
                    )
              )
    , fields ::
          { computed_value ::
                Array (Variant (computed_property :: ComputedProperty))
          , name ::
                Array (Variant (value_binding_pattern :: ValueBindingPattern))
          , value ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype PropertyModifier = PropertyModifier { fields :: {} }
newtype ProtocolBody = ProtocolBody
    { children ::
          Array
              ( Variant
                    ( associatedtype_declaration :: AssociatedtypeDeclaration
                    , deinit_declaration :: DeinitDeclaration
                    , protocol_function_declaration ::
                          ProtocolFunctionDeclaration
                    , protocol_property_declaration ::
                          ProtocolPropertyDeclaration
                    , subscript_declaration :: SubscriptDeclaration
                    , typealias_declaration :: TypealiasDeclaration
                    )
              )
    , fields ::
          { body ::
                Array
                    ( Variant
                          ( protocol_function_declaration ::
                                ProtocolFunctionDeclaration
                          )
                    )
          }
    }

newtype ProtocolCompositionType = ProtocolCompositionType
    { children ::
          Array
              ( Variant
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
              )
    , fields :: {}
    }

newtype ProtocolDeclaration = ProtocolDeclaration
    { children ::
          Array
              ( Variant
                    ( attribute :: Attribute
                    , inheritance_specifier :: InheritanceSpecifier
                    , modifiers :: Modifiers
                    , type_constraints :: TypeConstraints
                    , type_parameters :: TypeParameters
                    )
              )
    , fields ::
          { body :: Variant (protocol_body :: ProtocolBody)
          , declaration_kind :: Variant ()
          , name :: Variant (type_identifier :: TypeIdentifier)
          }
    }

newtype ProtocolFunctionDeclaration = ProtocolFunctionDeclaration
    { children ::
          Array
              ( Variant
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
              )
    , fields ::
          { default_value ::
                Array
                    ( Variant
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
                    )
          , name ::
                Array
                    ( Variant
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
                    )
          , return_type ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype ProtocolPropertyDeclaration = ProtocolPropertyDeclaration
    { children ::
          Array
              ( Variant
                    ( modifiers :: Modifiers
                    , protocol_property_requirements ::
                          ProtocolPropertyRequirements
                    , type_annotation :: TypeAnnotation
                    , type_constraints :: TypeConstraints
                    )
              )
    , fields ::
          { name :: Variant (value_binding_pattern :: ValueBindingPattern) }
    }

newtype ProtocolPropertyRequirements = ProtocolPropertyRequirements
    { children ::
          Array
              ( Variant
                    ( getter_specifier :: GetterSpecifier
                    , setter_specifier :: SetterSpecifier
                    )
              )
    , fields :: {}
    }

newtype RangeExpression = RangeExpression
    { fields ::
          { end ::
                Array
                    ( Variant
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
                    )
          , op :: Variant ()
          , start ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype RawStrInterpolation = RawStrInterpolation
    { child :: Variant (raw_str_interpolation_start :: RawStrInterpolationStart)
    , fields ::
          { interpolation ::
                Array
                    ( Variant
                          (interpolated_expression :: InterpolatedExpression)
                    )
          }
    }

newtype RawStringLiteral = RawStringLiteral
    { children ::
          Array
              ( Variant
                    (raw_str_continuing_indicator :: RawStrContinuingIndicator)
              )
    , fields ::
          { interpolation ::
                Array (Variant (raw_str_interpolation :: RawStrInterpolation))
          , text ::
                Array
                    ( Variant
                          ( raw_str_end_part :: RawStrEndPart
                          , raw_str_part :: RawStrPart
                          )
                    )
          }
    }

newtype RepeatWhileStatement = RepeatWhileStatement
    { child :: Maybe (Variant (statements :: Statements))
    , fields ::
          { condition ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype SelectorExpression = SelectorExpression
    { children ::
          Array
              ( Variant
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
              )
    , fields :: {}
    }

newtype SelfExpression = SelfExpression { fields :: {} }
newtype SetterSpecifier = SetterSpecifier
    { child :: Maybe (Variant (mutation_modifier :: MutationModifier))
    , fields :: {}
    }

newtype ShebangLine = ShebangLine { fields :: {} }
newtype SimpleIdentifier = SimpleIdentifier { fields :: {} }
newtype SourceFile = SourceFile
    { children ::
          Array
              ( Variant
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
              )
    , fields :: {}
    }

newtype Statements = Statements
    { children ::
          Array
              ( Variant
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
              )
    , fields :: {}
    }

newtype StrEscapedChar = StrEscapedChar { fields :: {} }
newtype SubscriptDeclaration = SubscriptDeclaration
    { children ::
          Array
              ( Variant
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
              )
    , fields ::
          { default_value ::
                Array
                    ( Variant
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
                    )
          , name ::
                Maybe
                    ( Variant
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
                    )
          , return_type ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype SuperExpression = SuperExpression { fields :: {} }
newtype SwitchEntry = SwitchEntry
    { children ::
          Array
              ( Variant
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
              )
    , fields :: {}
    }

newtype SwitchPattern = SwitchPattern
    { children ::
          Array
              ( Variant
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
              )
    , fields ::
          { name ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype SwitchStatement = SwitchStatement
    { children :: Array (Variant (switch_entry :: SwitchEntry))
    , fields ::
          { expr ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype TernaryExpression = TernaryExpression
    { fields ::
          { condition ::
                Array
                    ( Variant
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
                    )
          , if_false ::
                Array
                    ( Variant
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
                    )
          , if_true ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype Throws = Throws { fields :: {} }
newtype TryExpression = TryExpression
    { fields ::
          { expr ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype TupleExpression = TupleExpression
    { fields ::
          { name :: Array (Variant (simple_identifier :: SimpleIdentifier))
          , value ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype TupleType = TupleType
    { fields ::
          { element :: Array (Variant (tuple_type_item :: TupleTypeItem)) }
    }

newtype TupleTypeItem = TupleTypeItem
    { children ::
          Array
              ( Variant
                    ( parameter_modifiers :: ParameterModifiers
                    , wildcard_pattern :: WildcardPattern
                    )
              )
    , fields ::
          { name ::
                Array
                    ( Variant
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
                    )
          , type ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype TypeAnnotation = TypeAnnotation
    { fields ::
          { name ::
                Variant
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
          , type ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype TypeArguments = TypeArguments
    { children :: Array (Variant (type_modifiers :: TypeModifiers))
    , fields ::
          { name ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype TypeConstraint = TypeConstraint
    { child ::
          Variant
              ( equality_constraint :: EqualityConstraint
              , inheritance_constraint :: InheritanceConstraint
              )
    , fields :: {}
    }

newtype TypeConstraints = TypeConstraints
    { children ::
          Array
              ( Variant
                    ( type_constraint :: TypeConstraint
                    , where_keyword :: WhereKeyword
                    )
              )
    , fields :: {}
    }

newtype TypeIdentifier = TypeIdentifier { fields :: {} }
newtype TypeModifiers = TypeModifiers
    { children :: Array (Variant (attribute :: Attribute)), fields :: {} }

newtype TypeParameter = TypeParameter
    { children ::
          Array
              ( Variant
                    ( type_identifier :: TypeIdentifier
                    , type_modifiers :: TypeModifiers
                    , type_parameter_modifiers :: TypeParameterModifiers
                    )
              )
    , fields ::
          { name ::
                Maybe
                    ( Variant
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
                    )
          }
    }

newtype TypeParameterModifiers = TypeParameterModifiers
    { children :: Array (Variant (attribute :: Attribute)), fields :: {} }

newtype TypeParameters = TypeParameters
    { children :: Array (Variant (type_parameter :: TypeParameter))
    , fields :: {}
    }

newtype TypealiasDeclaration = TypealiasDeclaration
    { children ::
          Array
              ( Variant
                    ( attribute :: Attribute
                    , inheritance_modifier :: InheritanceModifier
                    , modifiers :: Modifiers
                    , ownership_modifier :: OwnershipModifier
                    , property_behavior_modifier :: PropertyBehaviorModifier
                    , type_parameters :: TypeParameters
                    )
              )
    , fields ::
          { name ::
                Array
                    ( Variant
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
                    )
          , value ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype UserType = UserType
    { children ::
          Array
              ( Variant
                    ( type_arguments :: TypeArguments
                    , type_identifier :: TypeIdentifier
                    )
              )
    , fields :: {}
    }

newtype ValueArgument = ValueArgument
    { child :: Maybe (Variant (type_modifiers :: TypeModifiers))
    , fields ::
          { name :: Maybe (Variant (simple_identifier :: SimpleIdentifier))
          , reference_specifier ::
                Array (Variant (simple_identifier :: SimpleIdentifier))
          , value ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype ValueArguments = ValueArguments
    { children :: Array (Variant (value_argument :: ValueArgument))
    , fields :: {}
    }

newtype ValueBindingPattern = ValueBindingPattern
    { child :: Variant (non_binding_pattern :: NonBindingPattern)
    , fields :: {}
    }

newtype VisibilityModifier = VisibilityModifier { fields :: {} }
newtype WhereClause = WhereClause
    { children ::
          Array
              ( Variant
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
              )
    , fields :: {}
    }

newtype WhileStatement = WhileStatement
    { child :: Maybe (Variant (statements :: Statements))
    , fields ::
          { condition ::
                Array
                    ( Variant
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
                    )
          }
    }

newtype Bang = Bang {}
newtype BinLiteral = BinLiteral {}
newtype CatchKeyword = CatchKeyword {}
newtype Comment = Comment {}
newtype DefaultKeyword = DefaultKeyword {}
newtype Diagnostic = Diagnostic {}
newtype Directive = Directive {}
newtype Else = Else {}
newtype HexLiteral = HexLiteral {}
newtype IntegerLiteral = IntegerLiteral {}
newtype MultilineComment = MultilineComment {}
newtype OctLiteral = OctLiteral {}
newtype PropertyBehaviorModifier = PropertyBehaviorModifier {}
newtype RawStrContinuingIndicator = RawStrContinuingIndicator {}
newtype RawStrEndPart = RawStrEndPart {}
newtype RawStrInterpolationStart = RawStrInterpolationStart {}
newtype RawStrPart = RawStrPart {}
newtype RealLiteral = RealLiteral {}
newtype StatementLabel = StatementLabel {}
newtype ThrowKeyword = ThrowKeyword {}
newtype WhereKeyword = WhereKeyword {}
newtype WildcardPattern = WildcardPattern {}
