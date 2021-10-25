open Base
open Import

(* Generates a 'a M.t Key.t for a record field *)
let generate_impl_record_item
      (lbl_decl : label_declaration)
      ~context
      ~generate_type_directed_value
  : expression
  =
  let ({ loc; attrs_decls; _ } : Utils.context) = context in
  let field_name_node = estring ~loc lbl_decl.pld_name.txt in
  let type_directed_value_node =
    generate_type_directed_value lbl_decl.pld_type ~context
  in
  let (module Attrs_decls : Attrs_intf.Attrs_declarations) = attrs_decls in
  let attribute_expr =
    Attrs.generate Attrs_decls.For_label_declaration.attr lbl_decl ~loc
  in
  let custom_type_directed_value =
    Attribute.get Attrs_decls.For_label_declaration.custom lbl_decl
  in
  let final_type_directed_value =
    Option.value custom_type_directed_value ~default:type_directed_value_node
  in
  [%expr
    { name      = [%e field_name_node]
    ; value     = [%e final_type_directed_value]
    ; attribute = [%e attribute_expr]
    }]
;;

(* Generates a ('a, _) Record(T).t given a label_declaration list *)
let generate_impl_record_structure_helper
      (lbl_decls : label_declaration list)
      ~(context : Utils.context)
      ~generate_type_directed_value
  : expression
  =
  elist
    ~loc:context.loc
    (List.map
       lbl_decls
       ~f:(generate_impl_record_item ~context ~generate_type_directed_value))
;;

let generate_impl_of_record
      (lbl_decls : label_declaration list)
      ~context
      ~generate_impl_record_structure
  : expression
  =
  let ({ loc; module_; _ } : Utils.context) = context in
  let record_expr    = generate_impl_record_structure lbl_decls ~context               in
  let of_record_expr = pexp_ident ~loc (Located.mk ~loc (Ldot (module_, "of_record"))) in
  [%expr [%e of_record_expr] [%e record_expr]]
;;

(* Generates a function that converts tuples used by the PPX to records and vice versa *)
(* {[
     fun (tuple_0, (tuple_1, (tuple_2, ()))) ->
       { f = tuple_0; g = tuple_1 ; h = tuple_2}
   ]} *)
(* {[
     fun {f = tuple_0; g = tuple_1; h = tuple_2} ->
       (tuple_0, (tuple_1, (tuple_2, ()))))
   ]} *)

let generate_conversion_funs (lbl_decls : label_declaration list) ~loc
  : expression * expression
  =
  let len              = List.length lbl_decls                                       in
  let prefix           = "tuple_"                                                    in
  let nested_tuple     = Pattern_or_expression.create_nested_tuple len ~prefix       in
  let record           = Pattern_or_expression.create_record_value lbl_decls ~prefix in
  let nested_tuple_pat = Pattern_or_expression.to_pattern nested_tuple ~loc          in
  let record_exp       = Pattern_or_expression.to_expression record ~loc             in
  let record_pat       = Pattern_or_expression.to_pattern record ~loc                in
  let nested_tuple_exp = Pattern_or_expression.to_expression nested_tuple ~loc       in
  ( [%expr fun [%p nested_tuple_pat] -> [%e record_exp]]
  , [%expr fun [%p record_pat] -> [%e nested_tuple_exp]] )
;;

(* Generates implementation for calling apply_iso given a 'a Record(T).t *)
let generate_impl_record_apply_iso
      ~context
      (lbl_decls : label_declaration list)
      (of_record_expr : expression)
  : expression
  =
  let ({ loc; module_; _ } : Utils.context) = context in
  let apply_iso_expr = pexp_ident ~loc (Located.mk ~loc (Ldot (module_, "apply_iso"))) in
  let tuple_to_record_expr, record_to_tuple_expr =
    generate_conversion_funs lbl_decls ~loc
  in
  [%expr
    [%e apply_iso_expr]
      [%e of_record_expr]
      [%e tuple_to_record_expr]
      [%e record_to_tuple_expr]]
;;

let generate_impl_record_helper
      (lbl_decls : label_declaration list)
      ~context
      ~generate_impl_record_structure
  : expression
  =
  generate_impl_record_apply_iso
    ~context
    lbl_decls
    (generate_impl_of_record lbl_decls ~context ~generate_impl_record_structure)
;;
