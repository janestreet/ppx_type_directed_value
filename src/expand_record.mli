open Import

(** Generates implementation to construct a [('a, _) Record(T).t] data structure *)
val generate_impl_record_structure_helper
  :  label_declaration list
  -> context:Utils.context
  -> generate_type_directed_value:(core_type -> context:Utils.context -> expression)
  -> expression

(** Generates implementation to construct a type-directed value for a record type *)
val generate_impl_record_helper
  :  label_declaration list
  -> context:Utils.context
  -> generate_impl_record_structure:
       (label_declaration list -> context:Utils.context -> expression)
  -> expression
