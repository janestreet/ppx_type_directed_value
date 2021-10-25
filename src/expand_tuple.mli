open Import

(** Generates implementation to construct a type-directed value for a tuple type *)
val generate_impl_tuple_helper
  :  core_type list
  -> context:Utils.context
  -> generate_type_directed_value:(core_type -> context:Utils.context -> expression)
  -> expression
