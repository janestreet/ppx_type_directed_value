open Import

type context =
  { loc         : location
  ; module_     : longident
  ; attrs_decls : (module Attrs_intf.Attrs_declarations)
  ; td          : type_declaration
  }

(** Returns a canonical function name for a type directed value generated with this PPX *)
val get_type_directed_name  : type_:string -> module_:longident -> string

(** Returns a canonical parameter name for a type variable *)
val get_type_var_param_name : string -> string

(** Returns an expression of a value of a variant type given [constr_name] and the
    parameters.  Simple wrapper around [pexp_construct] that adds a location to the
    [constr_name] string.
*)
val get_variant_expr
  :  loc:location
  -> constr_name:label
  -> expression option
  -> expression
