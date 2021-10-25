open Import

type variant_args =
  | Tuple  of core_type         list * constructor_declaration
  | Record of label_declaration list * constructor_declaration
  | Poly_variant of core_type list * row_field

(** Canonical representation for a variant type (includes polymorphic variants) *)
type variant_declaration =
  { name : string loc
  ; args : variant_args
  }

(** Convert normal variant declaration to canonical form *)
val constr_decl_to_variant_decl : constructor_declaration -> variant_declaration

(** Convert polymorphic variant declaration to canonical form *)
val row_field_to_variant_decl : row_field -> loc:location -> variant_declaration

(** Generates implementation to construct a type-directed value for a variant type *)
val generate_impl_variant_helper
  :  variant_declaration list
  -> context:Utils.context
  -> generate_type_directed_value:(core_type -> context:Utils.context -> expression)
  -> generate_impl_record_structure:
       (label_declaration list -> context:Utils.context -> expression)
  -> expression
