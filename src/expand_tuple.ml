open Base
open Import

(* Generates a ('a, _) Tuple(T).t given a core_type list *)
let generate_impl_tuple_structure
      (types : core_type list)
      ~(context : Utils.context)
      ~generate_type_directed_value
  : expression
  =
  elist
    ~loc:context.loc
    (List.map types ~f:(fun t -> generate_type_directed_value t ~context))
;;

let generate_impl_of_tuple (types : core_type list) ~context ~generate_type_directed_value
  =
  let ({ loc; module_; _ } : Utils.context) = context in
  let tuple_expr =
    generate_impl_tuple_structure types ~context ~generate_type_directed_value
  in
  let of_tuple_expr = pexp_ident ~loc (Located.mk ~loc (Ldot (module_, "of_tuple"))) in
  [%expr [%e of_tuple_expr] [%e tuple_expr]]
;;

(* {[ fun (t4, (t3, (t2, (t1, ())))) -> (t4, t3, t2, t1) ]} *)
(* {[ fun (t4, t3, t2, t1) -> (t4, (t3, (t2, (t1, ())))) ]} *)
let generate_conversion_funs (len : int) ~loc : expression * expression =
  let prefix           = "tuple_"                                              in
  let nested_tuple     = Pattern_or_expression.create_nested_tuple len ~prefix in
  let tuple            = Pattern_or_expression.create_tuple len ~prefix        in
  let nested_tuple_pat = Pattern_or_expression.to_pattern nested_tuple ~loc    in
  let tuple_exp        = Pattern_or_expression.to_expression tuple ~loc        in
  let tuple_pat        = Pattern_or_expression.to_pattern tuple ~loc           in
  let nested_tuple_exp = Pattern_or_expression.to_expression nested_tuple ~loc in
  ( [%expr fun [%p nested_tuple_pat] -> [%e tuple_exp]]
  , [%expr fun [%p tuple_pat] -> [%e nested_tuple_exp]] )
;;

let generate_impl_tuple_apply_iso
      ~context
      (types         : core_type list)
      (of_tuple_expr : expression    )
  : expression
  =
  let ({ loc; module_; _ } : Utils.context) = context in
  let apply_iso_expr = pexp_ident ~loc (Located.mk ~loc (Ldot (module_, "apply_iso"))) in
  let nested_pairs_to_tuple_expr, tuple_to_nested_pairs_expr =
    generate_conversion_funs (List.length types) ~loc
  in
  [%expr
    [%e apply_iso_expr]
      [%e of_tuple_expr]
      [%e nested_pairs_to_tuple_expr]
      [%e tuple_to_nested_pairs_expr]]
;;

let generate_impl_tuple_helper
      (types : core_type list)
      ~context
      ~generate_type_directed_value
  : expression
  =
  generate_impl_tuple_apply_iso
    ~context
    types
    (generate_impl_of_tuple types ~context ~generate_type_directed_value)
;;
