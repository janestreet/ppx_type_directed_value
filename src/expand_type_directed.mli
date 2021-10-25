open Import

(** Wrappers around helpers in [Expand_tuple], [Expand_type_directed_value],
    [Expand_record] and [Expand_variant] as they are mutually recursive *)

val generate_type_directed_value : core_type      -> context:Utils.context -> expression
val generate_impl_tuple          : core_type list -> context:Utils.context -> expression

val generate_impl_variant
  :  Expand_variant.variant_declaration list
  -> context:Utils.context
  -> expression

val generate_impl_record_structure
  :  label_declaration list
  -> context:Utils.context
  -> expression

val generate_impl_record : label_declaration list -> context:Utils.context -> expression
