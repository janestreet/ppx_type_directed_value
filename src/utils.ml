open Base
open Import

type context =
  { loc         : location
  ; module_     : longident
  ; attrs_decls : (module Attrs_intf.Attrs_declarations)
  ; td          : type_declaration
  }

let get_type_directed_name ~(type_ : string) ~(module_ : longident) =
  let module_ = Longident.last_exn module_ in
  match type_ with
  | "t"   -> String.lowercase module_
  | type_ -> String.lowercase module_ ^ "_" ^ type_
;;

let get_type_var_param_name (typevar : string) = "_param_type_directed__" ^ typevar

let get_variant_expr ~loc ~constr_name payload : expression =
  pexp_construct ~loc (Located.mk ~loc (Lident constr_name)) payload
;;
