open Base
open Import

let rec generate_type_directed_value (type_ : core_type) ~context =
  Expand_type_directed_value.generate_type_directed_value_helper
    type_
    ~context
    ~generate_impl_tuple
    ~generate_impl_variant

and generate_impl_tuple (types : core_type list) ~context =
  Expand_tuple.generate_impl_tuple_helper types ~context ~generate_type_directed_value

and generate_impl_variant variant_decls ~context =
  Expand_variant.generate_impl_variant_helper
    variant_decls
    ~context
    ~generate_type_directed_value
    ~generate_impl_record_structure

and generate_impl_record_structure lbl_decls ~context =
  Expand_record.generate_impl_record_structure_helper
    lbl_decls
    ~context
    ~generate_type_directed_value

and generate_impl_record lbl_decls ~context =
  Expand_record.generate_impl_record_helper
    lbl_decls
    ~context
    ~generate_impl_record_structure
;;
