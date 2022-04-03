module TreeSitteer.Codegen.Swift where

import Data.Array
import Data.Maybe
import Data.Variant

newtype Nadditive_expression = Nadditive_expression
    { fields ::
          { lhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Narray_literal = Narray_literal
    { fields ::
          { element ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Narray_type = Narray_type
    { fields ::
          { element ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          , name ::
                Variant
                    ( array_type :: Narray_type
                    , dictionary_type :: Ndictionary_type
                    , function_type :: Nfunction_type
                    , metatype :: Nmetatype
                    , opaque_type :: Nopaque_type
                    , optional_type :: Noptional_type
                    , protocol_composition_type :: Nprotocol_composition_type
                    , tuple_type :: Ntuple_type
                    , user_type :: Nuser_type
                    )
          }
    }

newtype Nas_expression = Nas_expression
    { child :: Variant (as_operator :: Nas_operator)
    , fields ::
          { expr ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , name ::
                Variant
                    ( array_type :: Narray_type
                    , dictionary_type :: Ndictionary_type
                    , function_type :: Nfunction_type
                    , metatype :: Nmetatype
                    , opaque_type :: Nopaque_type
                    , optional_type :: Noptional_type
                    , protocol_composition_type :: Nprotocol_composition_type
                    , tuple_type :: Ntuple_type
                    , user_type :: Nuser_type
                    )
          , type ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nas_operator = Nas_operator { fields :: {} }
newtype Nassignment = Nassignment
    { fields ::
          { operator :: Variant ()
          , result ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , target ::
                Variant
                    ( directly_assignable_expression ::
                          Ndirectly_assignable_expression
                    )
          }
    }

newtype Nassociatedtype_declaration = Nassociatedtype_declaration
    { children ::
          Array
              ( Variant
                    ( modifiers :: Nmodifiers
                    , type_constraints :: Ntype_constraints
                    )
              )
    , fields ::
          { default_value ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          , must_inherit ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          , name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_identifier :: Ntype_identifier
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nasync = Nasync { fields :: {} }
newtype Nattribute = Nattribute
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , check_expression :: Ncheck_expression
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , custom_operator :: Ncustom_operator
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , equality_expression :: Nequality_expression
                    , fully_open_range :: Nfully_open_range
                    , hex_literal :: Nhex_literal
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , postfix_expression :: Npostfix_expression
                    , prefix_expression :: Nprefix_expression
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    , super_expression :: Nsuper_expression
                    , ternary_expression :: Nternary_expression
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    , user_type :: Nuser_type
                    )
              )
    , fields :: {}
    }

newtype Navailability_condition = Navailability_condition
    { children ::
          Array
              ( Variant
                    ( identifier :: Nidentifier
                    , integer_literal :: Ninteger_literal
                    )
              )
    , fields :: {}
    }

newtype Nawait_expression = Nawait_expression
    { fields ::
          { expr ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nbinding_pattern = Nbinding_pattern
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , binding_pattern :: Nbinding_pattern
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , check_expression :: Ncheck_expression
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , custom_operator :: Ncustom_operator
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , equality_expression :: Nequality_expression
                    , fully_open_range :: Nfully_open_range
                    , hex_literal :: Nhex_literal
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , non_binding_pattern :: Nnon_binding_pattern
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , postfix_expression :: Npostfix_expression
                    , prefix_expression :: Nprefix_expression
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    , super_expression :: Nsuper_expression
                    , ternary_expression :: Nternary_expression
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    , type_modifiers :: Ntype_modifiers
                    , user_type :: Nuser_type
                    , wildcard_pattern :: Nwildcard_pattern
                    )
              )
    , fields ::
          { name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nbitwise_operation = Nbitwise_operation
    { fields ::
          { lhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nboolean_literal = Nboolean_literal { fields :: {} }
newtype Ncall_expression = Ncall_expression
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , call_suffix :: Ncall_suffix
                    , check_expression :: Ncheck_expression
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , custom_operator :: Ncustom_operator
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , equality_expression :: Nequality_expression
                    , fully_open_range :: Nfully_open_range
                    , hex_literal :: Nhex_literal
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , postfix_expression :: Npostfix_expression
                    , prefix_expression :: Nprefix_expression
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    , super_expression :: Nsuper_expression
                    , ternary_expression :: Nternary_expression
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    )
              )
    , fields :: {}
    }

newtype Ncall_suffix = Ncall_suffix
    { children ::
          Array
              ( Variant
                    ( lambda_literal :: Nlambda_literal
                    , value_arguments :: Nvalue_arguments
                    )
              )
    , fields ::
          { name :: Array (Variant (simple_identifier :: Nsimple_identifier)) }
    }

newtype Ncapture_list = Ncapture_list
    { children ::
          Array
              ( Variant
                    ( attribute :: Nattribute
                    , capture_list_item :: Ncapture_list_item
                    )
              )
    , fields :: {}
    }

newtype Ncapture_list_item = Ncapture_list_item
    { child :: Maybe (Variant (ownership_modifier :: Nownership_modifier))
    , fields ::
          { name ::
                Variant
                    ( self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    )
          , value ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Ncatch_block = Ncatch_block
    { children ::
          Array
              ( Variant
                    ( catch_keyword :: Ncatch_keyword
                    , statements :: Nstatements
                    , where_clause :: Nwhere_clause
                    )
              )
    , fields ::
          { error :: Maybe (Variant (binding_pattern :: Nbinding_pattern)) }
    }

newtype Ncheck_expression = Ncheck_expression
    { fields ::
          { name ::
                Variant
                    ( array_type :: Narray_type
                    , dictionary_type :: Ndictionary_type
                    , function_type :: Nfunction_type
                    , metatype :: Nmetatype
                    , opaque_type :: Nopaque_type
                    , optional_type :: Noptional_type
                    , protocol_composition_type :: Nprotocol_composition_type
                    , tuple_type :: Ntuple_type
                    , user_type :: Nuser_type
                    )
          , op :: Variant ()
          , target ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , type ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nclass_body = Nclass_body
    { children ::
          Array
              ( Variant
                    ( associatedtype_declaration :: Nassociatedtype_declaration
                    , class_declaration :: Nclass_declaration
                    , deinit_declaration :: Ndeinit_declaration
                    , function_declaration :: Nfunction_declaration
                    , import_declaration :: Nimport_declaration
                    , multiline_comment :: Nmultiline_comment
                    , operator_declaration :: Noperator_declaration
                    , precedence_group_declaration ::
                          Nprecedence_group_declaration
                    , property_declaration :: Nproperty_declaration
                    , protocol_declaration :: Nprotocol_declaration
                    , subscript_declaration :: Nsubscript_declaration
                    , typealias_declaration :: Ntypealias_declaration
                    )
              )
    , fields :: {}
    }

newtype Nclass_declaration = Nclass_declaration
    { children ::
          Array
              ( Variant
                    ( attribute :: Nattribute
                    , inheritance_modifier :: Ninheritance_modifier
                    , inheritance_specifier :: Ninheritance_specifier
                    , modifiers :: Nmodifiers
                    , ownership_modifier :: Nownership_modifier
                    , property_behavior_modifier :: Nproperty_behavior_modifier
                    , type_constraints :: Ntype_constraints
                    , type_parameters :: Ntype_parameters
                    )
              )
    , fields ::
          { body ::
                Variant
                    ( class_body :: Nclass_body
                    , enum_class_body :: Nenum_class_body
                    )
          , declaration_kind :: Variant ()
          , name ::
                Variant
                    ( type_identifier :: Ntype_identifier
                    , user_type :: Nuser_type
                    )
          }
    }

newtype Ncomparison_expression = Ncomparison_expression
    { fields ::
          { lhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Ncomputed_getter = Ncomputed_getter
    { children ::
          Array
              ( Variant
                    ( attribute :: Nattribute
                    , getter_specifier :: Ngetter_specifier
                    , statements :: Nstatements
                    )
              )
    , fields :: {}
    }

newtype Ncomputed_modify = Ncomputed_modify
    { children ::
          Array
              ( Variant
                    ( attribute :: Nattribute
                    , modify_specifier :: Nmodify_specifier
                    , statements :: Nstatements
                    )
              )
    , fields :: {}
    }

newtype Ncomputed_property = Ncomputed_property
    { children ::
          Array
              ( Variant
                    ( computed_getter :: Ncomputed_getter
                    , computed_modify :: Ncomputed_modify
                    , computed_setter :: Ncomputed_setter
                    , statements :: Nstatements
                    )
              )
    , fields :: {}
    }

newtype Ncomputed_setter = Ncomputed_setter
    { children ::
          Array
              ( Variant
                    ( attribute :: Nattribute
                    , setter_specifier :: Nsetter_specifier
                    , simple_identifier :: Nsimple_identifier
                    , statements :: Nstatements
                    )
              )
    , fields :: {}
    }

newtype Nconjunction_expression = Nconjunction_expression
    { fields ::
          { lhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nconstructor_expression = Nconstructor_expression
    { child :: Variant (constructor_suffix :: Nconstructor_suffix)
    , fields ::
          { constructed_type ::
                Variant
                    ( array_type :: Narray_type
                    , dictionary_type :: Ndictionary_type
                    , user_type :: Nuser_type
                    )
          }
    }

newtype Nconstructor_suffix = Nconstructor_suffix
    { child ::
          Variant
              ( lambda_literal :: Nlambda_literal
              , value_arguments :: Nvalue_arguments
              )
    , fields :: {}
    }

newtype Ncontrol_transfer_statement = Ncontrol_transfer_statement
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , check_expression :: Ncheck_expression
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , custom_operator :: Ncustom_operator
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , equality_expression :: Nequality_expression
                    , fully_open_range :: Nfully_open_range
                    , hex_literal :: Nhex_literal
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , postfix_expression :: Npostfix_expression
                    , prefix_expression :: Nprefix_expression
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    , super_expression :: Nsuper_expression
                    , ternary_expression :: Nternary_expression
                    , throw_keyword :: Nthrow_keyword
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    )
              )
    , fields ::
          { result ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Ncustom_operator = Ncustom_operator { fields :: {} }
newtype Ndeinit_declaration = Ndeinit_declaration
    { child :: Maybe (Variant (modifiers :: Nmodifiers))
    , fields :: { body :: Variant (function_body :: Nfunction_body) }
    }

newtype Ndictionary_literal = Ndictionary_literal
    { fields ::
          { key ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , value ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Ndictionary_type = Ndictionary_type
    { fields ::
          { key ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          , name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          , value ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Ndirectly_assignable_expression = Ndirectly_assignable_expression
    { child ::
          Variant
              ( call_expression :: Ncall_expression
              , navigation_expression :: Nnavigation_expression
              , self_expression :: Nself_expression
              , simple_identifier :: Nsimple_identifier
              , tuple_expression :: Ntuple_expression
              )
    , fields :: {}
    }

newtype Ndisjunction_expression = Ndisjunction_expression
    { fields ::
          { lhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Ndo_statement = Ndo_statement
    { children ::
          Array
              (Variant (catch_block :: Ncatch_block, statements :: Nstatements))
    , fields :: {}
    }

newtype Nenum_class_body = Nenum_class_body
    { children ::
          Array
              ( Variant
                    ( associatedtype_declaration :: Nassociatedtype_declaration
                    , class_declaration :: Nclass_declaration
                    , deinit_declaration :: Ndeinit_declaration
                    , enum_entry :: Nenum_entry
                    , function_declaration :: Nfunction_declaration
                    , import_declaration :: Nimport_declaration
                    , operator_declaration :: Noperator_declaration
                    , precedence_group_declaration ::
                          Nprecedence_group_declaration
                    , property_declaration :: Nproperty_declaration
                    , protocol_declaration :: Nprotocol_declaration
                    , subscript_declaration :: Nsubscript_declaration
                    , typealias_declaration :: Ntypealias_declaration
                    )
              )
    , fields :: {}
    }

newtype Nenum_entry = Nenum_entry
    { child :: Maybe (Variant (modifiers :: Nmodifiers))
    , fields ::
          { data_contents ::
                Array (Variant (enum_type_parameters :: Nenum_type_parameters))
          , name :: Array (Variant (simple_identifier :: Nsimple_identifier))
          , raw_value ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nenum_type_parameters = Nenum_type_parameters
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , check_expression :: Ncheck_expression
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , custom_operator :: Ncustom_operator
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , equality_expression :: Nequality_expression
                    , fully_open_range :: Nfully_open_range
                    , hex_literal :: Nhex_literal
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , postfix_expression :: Npostfix_expression
                    , prefix_expression :: Nprefix_expression
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    , super_expression :: Nsuper_expression
                    , ternary_expression :: Nternary_expression
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    , type_modifiers :: Ntype_modifiers
                    , wildcard_pattern :: Nwildcard_pattern
                    )
              )
    , fields ::
          { name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nequality_constraint = Nequality_constraint
    { children :: Array (Variant (attribute :: Nattribute))
    , fields ::
          { constrained_type :: Variant (identifier :: Nidentifier)
          , must_equal ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          , name ::
                Variant
                    ( array_type :: Narray_type
                    , dictionary_type :: Ndictionary_type
                    , function_type :: Nfunction_type
                    , metatype :: Nmetatype
                    , opaque_type :: Nopaque_type
                    , optional_type :: Noptional_type
                    , protocol_composition_type :: Nprotocol_composition_type
                    , tuple_type :: Ntuple_type
                    , user_type :: Nuser_type
                    )
          }
    }

newtype Nequality_expression = Nequality_expression
    { fields ::
          { lhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nfor_statement = Nfor_statement
    { children ::
          Array
              ( Variant
                    ( statements :: Nstatements
                    , type_annotation :: Ntype_annotation
                    , where_clause :: Nwhere_clause
                    )
              )
    , fields ::
          { collection ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , item ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , array_type :: Narray_type
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , binding_pattern :: Nbinding_pattern
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , dictionary_type :: Ndictionary_type
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , function_type :: Nfunction_type
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , metatype :: Nmetatype
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , non_binding_pattern :: Nnon_binding_pattern
                          , oct_literal :: Noct_literal
                          , opaque_type :: Nopaque_type
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , optional_type :: Noptional_type
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          , wildcard_pattern :: Nwildcard_pattern
                          )
                    )
          , name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nfully_open_range = Nfully_open_range { fields :: {} }
newtype Nfunction_body = Nfunction_body
    { child :: Maybe (Variant (statements :: Nstatements)), fields :: {} }

newtype Nfunction_declaration = Nfunction_declaration
    { children ::
          Array
              ( Variant
                    ( async :: Nasync
                    , attribute :: Nattribute
                    , bang :: Nbang
                    , inheritance_modifier :: Ninheritance_modifier
                    , modifiers :: Nmodifiers
                    , ownership_modifier :: Nownership_modifier
                    , parameter :: Nparameter
                    , property_behavior_modifier :: Nproperty_behavior_modifier
                    , throws :: Nthrows
                    , type_constraints :: Ntype_constraints
                    , type_parameters :: Ntype_parameters
                    )
              )
    , fields ::
          { body :: Variant (function_body :: Nfunction_body)
          , default_value ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , bang :: Nbang
                          , custom_operator :: Ncustom_operator
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , simple_identifier :: Nsimple_identifier
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          , return_type ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nfunction_modifier = Nfunction_modifier { fields :: {} }
newtype Nfunction_type = Nfunction_type
    { children :: Array (Variant (async :: Nasync, throws :: Nthrows))
    , fields ::
          { name ::
                Variant
                    ( array_type :: Narray_type
                    , dictionary_type :: Ndictionary_type
                    , function_type :: Nfunction_type
                    , metatype :: Nmetatype
                    , opaque_type :: Nopaque_type
                    , optional_type :: Noptional_type
                    , protocol_composition_type :: Nprotocol_composition_type
                    , tuple_type :: Ntuple_type
                    , user_type :: Nuser_type
                    )
          , params :: Variant (tuple_type :: Ntuple_type)
          , return_type ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Ngetter_specifier = Ngetter_specifier
    { children ::
          Array
              ( Variant
                    ( async :: Nasync
                    , mutation_modifier :: Nmutation_modifier
                    , throws :: Nthrows
                    )
              )
    , fields :: {}
    }

newtype Nguard_statement = Nguard_statement
    { children :: Array (Variant (else :: Nelse, statements :: Nstatements))
    , fields ::
          { condition ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , availability_condition :: Navailability_condition
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , binding_pattern :: Nbinding_pattern
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          , type_annotation :: Ntype_annotation
                          , value_binding_pattern :: Nvalue_binding_pattern
                          )
                    )
          }
    }

newtype Nidentifier = Nidentifier
    { children :: Array (Variant (simple_identifier :: Nsimple_identifier))
    , fields :: {}
    }

newtype Nif_statement = Nif_statement
    { children ::
          Array
              ( Variant
                    ( else :: Nelse
                    , if_statement :: Nif_statement
                    , statements :: Nstatements
                    )
              )
    , fields ::
          { condition ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , availability_condition :: Navailability_condition
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , binding_pattern :: Nbinding_pattern
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          , type_annotation :: Ntype_annotation
                          , value_binding_pattern :: Nvalue_binding_pattern
                          )
                    )
          }
    }

newtype Nimport_declaration = Nimport_declaration
    { children ::
          Array (Variant (identifier :: Nidentifier, modifiers :: Nmodifiers))
    , fields :: {}
    }

newtype Ninfix_expression = Ninfix_expression
    { fields ::
          { lhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , op :: Variant (custom_operator :: Ncustom_operator)
          , rhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Ninheritance_constraint = Ninheritance_constraint
    { children :: Array (Variant (attribute :: Nattribute))
    , fields ::
          { constrained_type :: Variant (identifier :: Nidentifier)
          , inherits_from ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          , name ::
                Variant
                    ( array_type :: Narray_type
                    , dictionary_type :: Ndictionary_type
                    , function_type :: Nfunction_type
                    , metatype :: Nmetatype
                    , opaque_type :: Nopaque_type
                    , optional_type :: Noptional_type
                    , protocol_composition_type :: Nprotocol_composition_type
                    , tuple_type :: Ntuple_type
                    , user_type :: Nuser_type
                    )
          }
    }

newtype Ninheritance_modifier = Ninheritance_modifier { fields :: {} }
newtype Ninheritance_specifier = Ninheritance_specifier
    { fields ::
          { inherits_from ::
                Variant
                    (function_type :: Nfunction_type, user_type :: Nuser_type)
          }
    }

newtype Ninterpolated_expression = Ninterpolated_expression
    { child :: Maybe (Variant (type_modifiers :: Ntype_modifiers))
    , fields ::
          { name :: Maybe (Variant (simple_identifier :: Nsimple_identifier))
          , reference_specifier ::
                Array (Variant (simple_identifier :: Nsimple_identifier))
          , value ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nkey_path_expression = Nkey_path_expression
    { children ::
          Array
              ( Variant
                    ( array_type :: Narray_type
                    , bang :: Nbang
                    , dictionary_type :: Ndictionary_type
                    , simple_identifier :: Nsimple_identifier
                    , type_arguments :: Ntype_arguments
                    , type_identifier :: Ntype_identifier
                    , value_argument :: Nvalue_argument
                    )
              )
    , fields :: {}
    }

newtype Nkey_path_string_expression = Nkey_path_string_expression
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , check_expression :: Ncheck_expression
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , custom_operator :: Ncustom_operator
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , equality_expression :: Nequality_expression
                    , fully_open_range :: Nfully_open_range
                    , hex_literal :: Nhex_literal
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , postfix_expression :: Npostfix_expression
                    , prefix_expression :: Nprefix_expression
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    , super_expression :: Nsuper_expression
                    , ternary_expression :: Nternary_expression
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    )
              )
    , fields :: {}
    }

newtype Nlambda_function_type = Nlambda_function_type
    { children ::
          Array
              ( Variant
                    ( async :: Nasync
                    , lambda_function_type_parameters ::
                          Nlambda_function_type_parameters
                    , throws :: Nthrows
                    )
              )
    , fields ::
          { name ::
                Maybe
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          , return_type ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nlambda_function_type_parameters = Nlambda_function_type_parameters
    { children :: Array (Variant (lambda_parameter :: Nlambda_parameter))
    , fields :: {}
    }

newtype Nlambda_literal = Nlambda_literal
    { child :: Maybe (Variant (statements :: Nstatements))
    , fields ::
          { captures :: Maybe (Variant (capture_list :: Ncapture_list))
          , type ::
                Maybe (Variant (lambda_function_type :: Nlambda_function_type))
          }
    }

newtype Nlambda_parameter = Nlambda_parameter
    { children ::
          Array
              ( Variant
                    ( attribute :: Nattribute
                    , parameter_modifiers :: Nparameter_modifiers
                    , self_expression :: Nself_expression
                    )
              )
    , fields ::
          { external_name ::
                Maybe (Variant (simple_identifier :: Nsimple_identifier))
          , name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , simple_identifier :: Nsimple_identifier
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          , type ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nline_str_text = Nline_str_text { fields :: {} }
newtype Nline_string_literal = Nline_string_literal
    { fields ::
          { interpolation ::
                Array
                    ( Variant
                          (interpolated_expression :: Ninterpolated_expression)
                    )
          , text ::
                Array
                    ( Variant
                          ( line_str_text :: Nline_str_text
                          , str_escaped_char :: Nstr_escaped_char
                          )
                    )
          }
    }

newtype Nmember_modifier = Nmember_modifier { fields :: {} }
newtype Nmetatype = Nmetatype
    { child ::
          Variant
              ( array_type :: Narray_type
              , dictionary_type :: Ndictionary_type
              , function_type :: Nfunction_type
              , metatype :: Nmetatype
              , opaque_type :: Nopaque_type
              , optional_type :: Noptional_type
              , protocol_composition_type :: Nprotocol_composition_type
              , tuple_type :: Ntuple_type
              , user_type :: Nuser_type
              )
    , fields :: {}
    }

newtype Nmodifiers = Nmodifiers
    { children ::
          Array
              ( Variant
                    ( attribute :: Nattribute
                    , function_modifier :: Nfunction_modifier
                    , inheritance_modifier :: Ninheritance_modifier
                    , member_modifier :: Nmember_modifier
                    , mutation_modifier :: Nmutation_modifier
                    , ownership_modifier :: Nownership_modifier
                    , parameter_modifier :: Nparameter_modifier
                    , property_behavior_modifier :: Nproperty_behavior_modifier
                    , property_modifier :: Nproperty_modifier
                    , visibility_modifier :: Nvisibility_modifier
                    )
              )
    , fields :: {}
    }

newtype Nmodify_specifier = Nmodify_specifier
    { child :: Maybe (Variant (mutation_modifier :: Nmutation_modifier))
    , fields :: {}
    }

newtype Nmulti_line_str_text = Nmulti_line_str_text { fields :: {} }
newtype Nmulti_line_string_literal = Nmulti_line_string_literal
    { fields ::
          { interpolation ::
                Array
                    ( Variant
                          (interpolated_expression :: Ninterpolated_expression)
                    )
          , text ::
                Array
                    ( Variant
                          ( multi_line_str_text :: Nmulti_line_str_text
                          , str_escaped_char :: Nstr_escaped_char
                          )
                    )
          }
    }

newtype Nmultiplicative_expression = Nmultiplicative_expression
    { fields ::
          { lhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , op :: Variant ()
          , rhs ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nmutation_modifier = Nmutation_modifier { fields :: {} }
newtype Nnavigation_expression = Nnavigation_expression
    { fields ::
          { suffix :: Variant (navigation_suffix :: Nnavigation_suffix)
          , target ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , array_type :: Narray_type
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , dictionary_type :: Ndictionary_type
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nnavigation_suffix = Nnavigation_suffix
    { fields ::
          { suffix ::
                Variant
                    ( integer_literal :: Ninteger_literal
                    , simple_identifier :: Nsimple_identifier
                    )
          }
    }

newtype Nnil_coalescing_expression = Nnil_coalescing_expression
    { fields ::
          { if_nil ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , value ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nnon_binding_pattern = Nnon_binding_pattern
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , check_expression :: Ncheck_expression
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , custom_operator :: Ncustom_operator
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , equality_expression :: Nequality_expression
                    , fully_open_range :: Nfully_open_range
                    , hex_literal :: Nhex_literal
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , non_binding_pattern :: Nnon_binding_pattern
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , postfix_expression :: Npostfix_expression
                    , prefix_expression :: Nprefix_expression
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    , super_expression :: Nsuper_expression
                    , ternary_expression :: Nternary_expression
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    , type_modifiers :: Ntype_modifiers
                    , user_type :: Nuser_type
                    , wildcard_pattern :: Nwildcard_pattern
                    )
              )
    , fields ::
          { name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nopaque_type = Nopaque_type
    { child :: Variant (user_type :: Nuser_type), fields :: {} }

newtype Nopen_end_range_expression = Nopen_end_range_expression
    { fields ::
          { start ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nopen_start_range_expression = Nopen_start_range_expression
    { fields ::
          { end ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Noperator_declaration = Noperator_declaration
    { children ::
          Array
              ( Variant
                    ( custom_operator :: Ncustom_operator
                    , simple_identifier :: Nsimple_identifier
                    )
              )
    , fields :: {}
    }

newtype Noptional_type = Noptional_type
    { fields ::
          { wrapped ::
                Variant
                    ( array_type :: Narray_type
                    , dictionary_type :: Ndictionary_type
                    , tuple_type :: Ntuple_type
                    , user_type :: Nuser_type
                    )
          }
    }

newtype Nownership_modifier = Nownership_modifier { fields :: {} }
newtype Nparameter = Nparameter
    { child :: Maybe (Variant (parameter_modifiers :: Nparameter_modifiers))
    , fields ::
          { external_name ::
                Maybe (Variant (simple_identifier :: Nsimple_identifier))
          , name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , simple_identifier :: Nsimple_identifier
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          , type ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nparameter_modifier = Nparameter_modifier { fields :: {} }
newtype Nparameter_modifiers = Nparameter_modifiers
    { children :: Array (Variant (parameter_modifier :: Nparameter_modifier))
    , fields :: {}
    }

newtype Npostfix_expression = Npostfix_expression
    { fields ::
          { operation :: Variant (bang :: Nbang)
          , target ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nprecedence_group_attribute = Nprecedence_group_attribute
    { children ::
          Array
              ( Variant
                    ( boolean_literal :: Nboolean_literal
                    , simple_identifier :: Nsimple_identifier
                    )
              )
    , fields :: {}
    }

newtype Nprecedence_group_attributes = Nprecedence_group_attributes
    { children ::
          Array
              ( Variant
                    (precedence_group_attribute :: Nprecedence_group_attribute)
              )
    , fields :: {}
    }

newtype Nprecedence_group_declaration = Nprecedence_group_declaration
    { children ::
          Array
              ( Variant
                    ( precedence_group_attributes ::
                          Nprecedence_group_attributes
                    , simple_identifier :: Nsimple_identifier
                    )
              )
    , fields :: {}
    }

newtype Nprefix_expression = Nprefix_expression
    { fields ::
          { operation ::
                Variant (bang :: Nbang, custom_operator :: Ncustom_operator)
          , target ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nproperty_declaration = Nproperty_declaration
    { children ::
          Array
              ( Variant
                    ( attribute :: Nattribute
                    , inheritance_modifier :: Ninheritance_modifier
                    , modifiers :: Nmodifiers
                    , ownership_modifier :: Nownership_modifier
                    , property_behavior_modifier :: Nproperty_behavior_modifier
                    , type_annotation :: Ntype_annotation
                    , type_constraints :: Ntype_constraints
                    )
              )
    , fields ::
          { computed_value ::
                Array (Variant (computed_property :: Ncomputed_property))
          , name ::
                Array
                    (Variant (value_binding_pattern :: Nvalue_binding_pattern))
          , value ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nproperty_modifier = Nproperty_modifier { fields :: {} }
newtype Nprotocol_body = Nprotocol_body
    { children ::
          Array
              ( Variant
                    ( associatedtype_declaration :: Nassociatedtype_declaration
                    , deinit_declaration :: Ndeinit_declaration
                    , protocol_function_declaration ::
                          Nprotocol_function_declaration
                    , protocol_property_declaration ::
                          Nprotocol_property_declaration
                    , subscript_declaration :: Nsubscript_declaration
                    , typealias_declaration :: Ntypealias_declaration
                    )
              )
    , fields ::
          { body ::
                Array
                    ( Variant
                          ( protocol_function_declaration ::
                                Nprotocol_function_declaration
                          )
                    )
          }
    }

newtype Nprotocol_composition_type = Nprotocol_composition_type
    { children ::
          Array
              ( Variant
                    ( array_type :: Narray_type
                    , dictionary_type :: Ndictionary_type
                    , function_type :: Nfunction_type
                    , metatype :: Nmetatype
                    , opaque_type :: Nopaque_type
                    , optional_type :: Noptional_type
                    , protocol_composition_type :: Nprotocol_composition_type
                    , tuple_type :: Ntuple_type
                    , user_type :: Nuser_type
                    )
              )
    , fields :: {}
    }

newtype Nprotocol_declaration = Nprotocol_declaration
    { children ::
          Array
              ( Variant
                    ( attribute :: Nattribute
                    , inheritance_specifier :: Ninheritance_specifier
                    , modifiers :: Nmodifiers
                    , type_constraints :: Ntype_constraints
                    , type_parameters :: Ntype_parameters
                    )
              )
    , fields ::
          { body :: Variant (protocol_body :: Nprotocol_body)
          , declaration_kind :: Variant ()
          , name :: Variant (type_identifier :: Ntype_identifier)
          }
    }

newtype Nprotocol_function_declaration = Nprotocol_function_declaration
    { children ::
          Array
              ( Variant
                    ( async :: Nasync
                    , attribute :: Nattribute
                    , bang :: Nbang
                    , modifiers :: Nmodifiers
                    , parameter :: Nparameter
                    , statements :: Nstatements
                    , throws :: Nthrows
                    , type_constraints :: Ntype_constraints
                    , type_parameters :: Ntype_parameters
                    )
              )
    , fields ::
          { default_value ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , bang :: Nbang
                          , custom_operator :: Ncustom_operator
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , simple_identifier :: Nsimple_identifier
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          , return_type ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nprotocol_property_declaration = Nprotocol_property_declaration
    { children ::
          Array
              ( Variant
                    ( modifiers :: Nmodifiers
                    , protocol_property_requirements ::
                          Nprotocol_property_requirements
                    , type_annotation :: Ntype_annotation
                    , type_constraints :: Ntype_constraints
                    )
              )
    , fields ::
          { name :: Variant (value_binding_pattern :: Nvalue_binding_pattern) }
    }

newtype Nprotocol_property_requirements = Nprotocol_property_requirements
    { children ::
          Array
              ( Variant
                    ( getter_specifier :: Ngetter_specifier
                    , setter_specifier :: Nsetter_specifier
                    )
              )
    , fields :: {}
    }

newtype Nrange_expression = Nrange_expression
    { fields ::
          { end ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , op :: Variant ()
          , start ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nraw_str_interpolation = Nraw_str_interpolation
    { child ::
          Variant (raw_str_interpolation_start :: Nraw_str_interpolation_start)
    , fields ::
          { interpolation ::
                Array
                    ( Variant
                          (interpolated_expression :: Ninterpolated_expression)
                    )
          }
    }

newtype Nraw_string_literal = Nraw_string_literal
    { children ::
          Array
              ( Variant
                    ( raw_str_continuing_indicator ::
                          Nraw_str_continuing_indicator
                    )
              )
    , fields ::
          { interpolation ::
                Array
                    (Variant (raw_str_interpolation :: Nraw_str_interpolation))
          , text ::
                Array
                    ( Variant
                          ( raw_str_end_part :: Nraw_str_end_part
                          , raw_str_part :: Nraw_str_part
                          )
                    )
          }
    }

newtype Nrepeat_while_statement = Nrepeat_while_statement
    { child :: Maybe (Variant (statements :: Nstatements))
    , fields ::
          { condition ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , availability_condition :: Navailability_condition
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , binding_pattern :: Nbinding_pattern
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          , type_annotation :: Ntype_annotation
                          , value_binding_pattern :: Nvalue_binding_pattern
                          )
                    )
          }
    }

newtype Nselector_expression = Nselector_expression
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , check_expression :: Ncheck_expression
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , custom_operator :: Ncustom_operator
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , equality_expression :: Nequality_expression
                    , fully_open_range :: Nfully_open_range
                    , hex_literal :: Nhex_literal
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , postfix_expression :: Npostfix_expression
                    , prefix_expression :: Nprefix_expression
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    , super_expression :: Nsuper_expression
                    , ternary_expression :: Nternary_expression
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    )
              )
    , fields :: {}
    }

newtype Nself_expression = Nself_expression { fields :: {} }
newtype Nsetter_specifier = Nsetter_specifier
    { child :: Maybe (Variant (mutation_modifier :: Nmutation_modifier))
    , fields :: {}
    }

newtype Nshebang_line = Nshebang_line { fields :: {} }
newtype Nsimple_identifier = Nsimple_identifier { fields :: {} }
newtype Nsource_file = Nsource_file
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , associatedtype_declaration :: Nassociatedtype_declaration
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , check_expression :: Ncheck_expression
                    , class_declaration :: Nclass_declaration
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , custom_operator :: Ncustom_operator
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , do_statement :: Ndo_statement
                    , equality_expression :: Nequality_expression
                    , for_statement :: Nfor_statement
                    , fully_open_range :: Nfully_open_range
                    , function_declaration :: Nfunction_declaration
                    , guard_statement :: Nguard_statement
                    , hex_literal :: Nhex_literal
                    , if_statement :: Nif_statement
                    , import_declaration :: Nimport_declaration
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , operator_declaration :: Noperator_declaration
                    , postfix_expression :: Npostfix_expression
                    , precedence_group_declaration ::
                          Nprecedence_group_declaration
                    , prefix_expression :: Nprefix_expression
                    , property_declaration :: Nproperty_declaration
                    , protocol_declaration :: Nprotocol_declaration
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , repeat_while_statement :: Nrepeat_while_statement
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , shebang_line :: Nshebang_line
                    , simple_identifier :: Nsimple_identifier
                    , statement_label :: Nstatement_label
                    , super_expression :: Nsuper_expression
                    , switch_statement :: Nswitch_statement
                    , ternary_expression :: Nternary_expression
                    , throw_keyword :: Nthrow_keyword
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    , typealias_declaration :: Ntypealias_declaration
                    , while_statement :: Nwhile_statement
                    )
              )
    , fields :: {}
    }

newtype Nstatements = Nstatements
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , check_expression :: Ncheck_expression
                    , class_declaration :: Nclass_declaration
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , control_transfer_statement :: Ncontrol_transfer_statement
                    , custom_operator :: Ncustom_operator
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , do_statement :: Ndo_statement
                    , equality_expression :: Nequality_expression
                    , for_statement :: Nfor_statement
                    , fully_open_range :: Nfully_open_range
                    , function_declaration :: Nfunction_declaration
                    , guard_statement :: Nguard_statement
                    , hex_literal :: Nhex_literal
                    , if_statement :: Nif_statement
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , postfix_expression :: Npostfix_expression
                    , prefix_expression :: Nprefix_expression
                    , property_declaration :: Nproperty_declaration
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , repeat_while_statement :: Nrepeat_while_statement
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    , statement_label :: Nstatement_label
                    , super_expression :: Nsuper_expression
                    , switch_statement :: Nswitch_statement
                    , ternary_expression :: Nternary_expression
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    , typealias_declaration :: Ntypealias_declaration
                    , while_statement :: Nwhile_statement
                    )
              )
    , fields :: {}
    }

newtype Nstr_escaped_char = Nstr_escaped_char { fields :: {} }
newtype Nsubscript_declaration = Nsubscript_declaration
    { children ::
          Array
              ( Variant
                    ( attribute :: Nattribute
                    , computed_getter :: Ncomputed_getter
                    , computed_modify :: Ncomputed_modify
                    , computed_setter :: Ncomputed_setter
                    , modifiers :: Nmodifiers
                    , parameter :: Nparameter
                    , statements :: Nstatements
                    , type_constraints :: Ntype_constraints
                    , type_parameters :: Ntype_parameters
                    )
              )
    , fields ::
          { default_value ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , name ::
                Maybe
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          , return_type ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nsuper_expression = Nsuper_expression { fields :: {} }
newtype Nswitch_entry = Nswitch_entry
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , check_expression :: Ncheck_expression
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , custom_operator :: Ncustom_operator
                    , default_keyword :: Ndefault_keyword
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , equality_expression :: Nequality_expression
                    , fully_open_range :: Nfully_open_range
                    , hex_literal :: Nhex_literal
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , modifiers :: Nmodifiers
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , postfix_expression :: Npostfix_expression
                    , prefix_expression :: Nprefix_expression
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    , statements :: Nstatements
                    , super_expression :: Nsuper_expression
                    , switch_pattern :: Nswitch_pattern
                    , ternary_expression :: Nternary_expression
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    , where_keyword :: Nwhere_keyword
                    )
              )
    , fields :: {}
    }

newtype Nswitch_pattern = Nswitch_pattern
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , binding_pattern :: Nbinding_pattern
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , check_expression :: Ncheck_expression
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , custom_operator :: Ncustom_operator
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , equality_expression :: Nequality_expression
                    , fully_open_range :: Nfully_open_range
                    , hex_literal :: Nhex_literal
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , non_binding_pattern :: Nnon_binding_pattern
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , postfix_expression :: Npostfix_expression
                    , prefix_expression :: Nprefix_expression
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    , super_expression :: Nsuper_expression
                    , ternary_expression :: Nternary_expression
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    , type_modifiers :: Ntype_modifiers
                    , user_type :: Nuser_type
                    , wildcard_pattern :: Nwildcard_pattern
                    )
              )
    , fields ::
          { name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nswitch_statement = Nswitch_statement
    { children :: Array (Variant (switch_entry :: Nswitch_entry))
    , fields ::
          { expr ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nternary_expression = Nternary_expression
    { fields ::
          { condition ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , if_false ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          , if_true ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nthrows = Nthrows { fields :: {} }
newtype Ntry_expression = Ntry_expression
    { fields ::
          { expr ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Ntuple_expression = Ntuple_expression
    { fields ::
          { name :: Array (Variant (simple_identifier :: Nsimple_identifier))
          , value ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Ntuple_type = Ntuple_type
    { fields ::
          { element :: Array (Variant (tuple_type_item :: Ntuple_type_item)) }
    }

newtype Ntuple_type_item = Ntuple_type_item
    { children ::
          Array
              ( Variant
                    ( parameter_modifiers :: Nparameter_modifiers
                    , wildcard_pattern :: Nwildcard_pattern
                    )
              )
    , fields ::
          { name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , simple_identifier :: Nsimple_identifier
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          , type ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Ntype_annotation = Ntype_annotation
    { fields ::
          { name ::
                Variant
                    ( array_type :: Narray_type
                    , dictionary_type :: Ndictionary_type
                    , function_type :: Nfunction_type
                    , metatype :: Nmetatype
                    , opaque_type :: Nopaque_type
                    , optional_type :: Noptional_type
                    , protocol_composition_type :: Nprotocol_composition_type
                    , tuple_type :: Ntuple_type
                    , user_type :: Nuser_type
                    )
          , type ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Ntype_arguments = Ntype_arguments
    { children :: Array (Variant (type_modifiers :: Ntype_modifiers))
    , fields ::
          { name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Ntype_constraint = Ntype_constraint
    { child ::
          Variant
              ( equality_constraint :: Nequality_constraint
              , inheritance_constraint :: Ninheritance_constraint
              )
    , fields :: {}
    }

newtype Ntype_constraints = Ntype_constraints
    { children ::
          Array
              ( Variant
                    ( type_constraint :: Ntype_constraint
                    , where_keyword :: Nwhere_keyword
                    )
              )
    , fields :: {}
    }

newtype Ntype_identifier = Ntype_identifier { fields :: {} }
newtype Ntype_modifiers = Ntype_modifiers
    { children :: Array (Variant (attribute :: Nattribute)), fields :: {} }

newtype Ntype_parameter = Ntype_parameter
    { children ::
          Array
              ( Variant
                    ( type_identifier :: Ntype_identifier
                    , type_modifiers :: Ntype_modifiers
                    , type_parameter_modifiers :: Ntype_parameter_modifiers
                    )
              )
    , fields ::
          { name ::
                Maybe
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Ntype_parameter_modifiers = Ntype_parameter_modifiers
    { children :: Array (Variant (attribute :: Nattribute)), fields :: {} }

newtype Ntype_parameters = Ntype_parameters
    { children :: Array (Variant (type_parameter :: Ntype_parameter))
    , fields :: {}
    }

newtype Ntypealias_declaration = Ntypealias_declaration
    { children ::
          Array
              ( Variant
                    ( attribute :: Nattribute
                    , inheritance_modifier :: Ninheritance_modifier
                    , modifiers :: Nmodifiers
                    , ownership_modifier :: Nownership_modifier
                    , property_behavior_modifier :: Nproperty_behavior_modifier
                    , type_parameters :: Ntype_parameters
                    )
              )
    , fields ::
          { name ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_identifier :: Ntype_identifier
                          , user_type :: Nuser_type
                          )
                    )
          , value ::
                Array
                    ( Variant
                          ( array_type :: Narray_type
                          , dictionary_type :: Ndictionary_type
                          , function_type :: Nfunction_type
                          , metatype :: Nmetatype
                          , opaque_type :: Nopaque_type
                          , optional_type :: Noptional_type
                          , protocol_composition_type ::
                                Nprotocol_composition_type
                          , tuple_type :: Ntuple_type
                          , type_modifiers :: Ntype_modifiers
                          , user_type :: Nuser_type
                          )
                    )
          }
    }

newtype Nuser_type = Nuser_type
    { children ::
          Array
              ( Variant
                    ( type_arguments :: Ntype_arguments
                    , type_identifier :: Ntype_identifier
                    )
              )
    , fields :: {}
    }

newtype Nvalue_argument = Nvalue_argument
    { child :: Maybe (Variant (type_modifiers :: Ntype_modifiers))
    , fields ::
          { name :: Maybe (Variant (simple_identifier :: Nsimple_identifier))
          , reference_specifier ::
                Array (Variant (simple_identifier :: Nsimple_identifier))
          , value ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          )
                    )
          }
    }

newtype Nvalue_arguments = Nvalue_arguments
    { children :: Array (Variant (value_argument :: Nvalue_argument))
    , fields :: {}
    }

newtype Nvalue_binding_pattern = Nvalue_binding_pattern
    { child :: Variant (non_binding_pattern :: Nnon_binding_pattern)
    , fields :: {}
    }

newtype Nvisibility_modifier = Nvisibility_modifier { fields :: {} }
newtype Nwhere_clause = Nwhere_clause
    { children ::
          Array
              ( Variant
                    ( additive_expression :: Nadditive_expression
                    , array_literal :: Narray_literal
                    , as_expression :: Nas_expression
                    , assignment :: Nassignment
                    , await_expression :: Nawait_expression
                    , bang :: Nbang
                    , bin_literal :: Nbin_literal
                    , bitwise_operation :: Nbitwise_operation
                    , boolean_literal :: Nboolean_literal
                    , call_expression :: Ncall_expression
                    , check_expression :: Ncheck_expression
                    , comparison_expression :: Ncomparison_expression
                    , conjunction_expression :: Nconjunction_expression
                    , constructor_expression :: Nconstructor_expression
                    , custom_operator :: Ncustom_operator
                    , dictionary_literal :: Ndictionary_literal
                    , disjunction_expression :: Ndisjunction_expression
                    , equality_expression :: Nequality_expression
                    , fully_open_range :: Nfully_open_range
                    , hex_literal :: Nhex_literal
                    , infix_expression :: Ninfix_expression
                    , integer_literal :: Ninteger_literal
                    , key_path_expression :: Nkey_path_expression
                    , key_path_string_expression :: Nkey_path_string_expression
                    , lambda_literal :: Nlambda_literal
                    , line_string_literal :: Nline_string_literal
                    , multi_line_string_literal :: Nmulti_line_string_literal
                    , multiplicative_expression :: Nmultiplicative_expression
                    , navigation_expression :: Nnavigation_expression
                    , nil_coalescing_expression :: Nnil_coalescing_expression
                    , oct_literal :: Noct_literal
                    , open_end_range_expression :: Nopen_end_range_expression
                    , open_start_range_expression ::
                          Nopen_start_range_expression
                    , postfix_expression :: Npostfix_expression
                    , prefix_expression :: Nprefix_expression
                    , range_expression :: Nrange_expression
                    , raw_string_literal :: Nraw_string_literal
                    , real_literal :: Nreal_literal
                    , selector_expression :: Nselector_expression
                    , self_expression :: Nself_expression
                    , simple_identifier :: Nsimple_identifier
                    , super_expression :: Nsuper_expression
                    , ternary_expression :: Nternary_expression
                    , try_expression :: Ntry_expression
                    , tuple_expression :: Ntuple_expression
                    , where_keyword :: Nwhere_keyword
                    )
              )
    , fields :: {}
    }

newtype Nwhile_statement = Nwhile_statement
    { child :: Maybe (Variant (statements :: Nstatements))
    , fields ::
          { condition ::
                Array
                    ( Variant
                          ( additive_expression :: Nadditive_expression
                          , array_literal :: Narray_literal
                          , as_expression :: Nas_expression
                          , assignment :: Nassignment
                          , availability_condition :: Navailability_condition
                          , await_expression :: Nawait_expression
                          , bang :: Nbang
                          , bin_literal :: Nbin_literal
                          , binding_pattern :: Nbinding_pattern
                          , bitwise_operation :: Nbitwise_operation
                          , boolean_literal :: Nboolean_literal
                          , call_expression :: Ncall_expression
                          , check_expression :: Ncheck_expression
                          , comparison_expression :: Ncomparison_expression
                          , conjunction_expression :: Nconjunction_expression
                          , constructor_expression :: Nconstructor_expression
                          , custom_operator :: Ncustom_operator
                          , dictionary_literal :: Ndictionary_literal
                          , disjunction_expression :: Ndisjunction_expression
                          , equality_expression :: Nequality_expression
                          , fully_open_range :: Nfully_open_range
                          , hex_literal :: Nhex_literal
                          , infix_expression :: Ninfix_expression
                          , integer_literal :: Ninteger_literal
                          , key_path_expression :: Nkey_path_expression
                          , key_path_string_expression ::
                                Nkey_path_string_expression
                          , lambda_literal :: Nlambda_literal
                          , line_string_literal :: Nline_string_literal
                          , multi_line_string_literal ::
                                Nmulti_line_string_literal
                          , multiplicative_expression ::
                                Nmultiplicative_expression
                          , navigation_expression :: Nnavigation_expression
                          , nil_coalescing_expression ::
                                Nnil_coalescing_expression
                          , oct_literal :: Noct_literal
                          , open_end_range_expression ::
                                Nopen_end_range_expression
                          , open_start_range_expression ::
                                Nopen_start_range_expression
                          , postfix_expression :: Npostfix_expression
                          , prefix_expression :: Nprefix_expression
                          , range_expression :: Nrange_expression
                          , raw_string_literal :: Nraw_string_literal
                          , real_literal :: Nreal_literal
                          , selector_expression :: Nselector_expression
                          , self_expression :: Nself_expression
                          , simple_identifier :: Nsimple_identifier
                          , super_expression :: Nsuper_expression
                          , ternary_expression :: Nternary_expression
                          , try_expression :: Ntry_expression
                          , tuple_expression :: Ntuple_expression
                          , type_annotation :: Ntype_annotation
                          , value_binding_pattern :: Nvalue_binding_pattern
                          )
                    )
          }
    }

newtype Nbang = Nbang {}
newtype Nbin_literal = Nbin_literal {}
newtype Ncatch_keyword = Ncatch_keyword {}
newtype Ncomment = Ncomment {}
newtype Ndefault_keyword = Ndefault_keyword {}
newtype Ndiagnostic = Ndiagnostic {}
newtype Ndirective = Ndirective {}
newtype Nelse = Nelse {}
newtype Nhex_literal = Nhex_literal {}
newtype Ninteger_literal = Ninteger_literal {}
newtype Nmultiline_comment = Nmultiline_comment {}
newtype Noct_literal = Noct_literal {}
newtype Nproperty_behavior_modifier = Nproperty_behavior_modifier {}
newtype Nraw_str_continuing_indicator = Nraw_str_continuing_indicator {}
newtype Nraw_str_end_part = Nraw_str_end_part {}
newtype Nraw_str_interpolation_start = Nraw_str_interpolation_start {}
newtype Nraw_str_part = Nraw_str_part {}
newtype Nreal_literal = Nreal_literal {}
newtype Nstatement_label = Nstatement_label {}
newtype Nthrow_keyword = Nthrow_keyword {}
newtype Nwhere_keyword = Nwhere_keyword {}
newtype Nwildcard_pattern = Nwildcard_pattern {}
