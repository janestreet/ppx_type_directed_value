open Base
open Import

(** Generates implementation to construct a type-directed value for a [core_type] *)
val generate_type_directed_value_helper
  :  core_type
  -> context:Utils.context
  -> generate_impl_tuple:(core_type list -> context:Utils.context -> expression)
  -> generate_impl_variant:
       (Expand_variant.variant_declaration list -> context:Utils.context -> expression)
  -> expression
