open Base
open Import

type variant_args =
  | Tuple  of core_type         list * constructor_declaration
  | Record of label_declaration list * constructor_declaration
  | Poly_variant of core_type list * row_field

type variant_declaration =
  { name : string loc
  ; args : variant_args
  }

let constr_decl_to_variant_decl (constr_decl : constructor_declaration)
  : variant_declaration
  =
  { name = constr_decl.pcd_name
  ; args =
      (match constr_decl.pcd_args with
       | Pcstr_tuple  l -> Tuple  (l, constr_decl )
       | Pcstr_record r -> Record (r, constr_decl))
  }
;;

let row_field_to_variant_decl (row_field : row_field) ~loc : variant_declaration =
  match row_field.prf_desc with
  | Rtag (label, _, []) -> { name = label; args = Poly_variant ([], row_field) }
  | Rtag (label, _, [ constr_type ]) ->
    (match constr_type.ptyp_desc with
     | Ptyp_tuple l -> { name = label; args = Poly_variant (l, row_field) }
     | _            -> { name = label; args = Poly_variant ([ constr_type ], row_field) })
  | _ -> Location.raise_errorf ~loc "does not support Rinherit and & types"
;;

(* Generates a 'a Variant(T).t for a variant constructor declaration *)
let generate_impl_variant_item
      (variant_decl : variant_declaration)
      ~context
      ~generate_type_directed_value
      ~generate_impl_record_structure
  : expression
  =
  let ({ loc; attrs_decls; _ } : Utils.context) = context in
  let field_name_node   = estring ~loc variant_decl.name.txt                    in
  let unlabelled_constr = Utils.get_variant_expr ~loc ~constr_name:"Unlabelled" in
  let labelled_constr   = Utils.get_variant_expr ~loc ~constr_name:"Labelled"   in
  let (module Attrs_decls : Attrs.Attrs_declarations) = attrs_decls             in
  let type_directed_value_node =
    match variant_decl.args with
    | Tuple (types, _) | Poly_variant (types, _) ->
      unlabelled_constr
        (Some
           (elist
              ~loc:context.loc
              (List.map types ~f:(fun t -> generate_type_directed_value t ~context))))
    | Record (lbl_decls, _) ->
      let record_expr = generate_impl_record_structure lbl_decls ~context in
      labelled_constr (Some record_expr)
  in
  let attribute_expr, custom_type_directed_value =
    match variant_decl.args with
    | Tuple        (_, constr_decl) | Record (_, constr_decl) ->
      ( Attrs.generate Attrs_decls.For_constructor_declaration.attr constr_decl ~loc
      , Attribute.get Attrs_decls.For_constructor_declaration.custom constr_decl )
    | Poly_variant (_, row_field                            ) ->
      ( Attrs.generate Attrs_decls.For_row_field.attr row_field ~loc
      , Attribute.get Attrs_decls.For_row_field.custom row_field )
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

(* Generates the 'a Variant(M).t data structure to pass to of_variant *)
let generate_impl_variant_structure
      (variant_decls : variant_declaration list)
      ~(context : Utils.context)
      ~generate_type_directed_value
      ~generate_impl_record_structure
  : expression
  =
  elist
    ~loc:context.loc
    (List.map
       variant_decls
       ~f:
         (generate_impl_variant_item
            ~context
            ~generate_type_directed_value
            ~generate_impl_record_structure))
;;

(* Generates implementation that calls of_variant on the variant data structure
   obtained by calling [generate_impl_variant_item] on [constr_decls] *)
let generate_impl_of_variant
      (variant_decls : variant_declaration list)
      ~context
      ~generate_type_directed_value
      ~generate_impl_record_structure
  : expression
  =
  let ({ loc; module_; _ } : Utils.context) = context in
  let variant_expr =
    generate_impl_variant_structure
      variant_decls
      ~context
      ~generate_type_directed_value
      ~generate_impl_record_structure
  in
  let of_variant_expr =
    pexp_ident ~loc (Located.mk ~loc (Ldot (module_, "of_variant")))
  in
  [%expr [%e of_variant_expr] [%e variant_expr]]
;;

(* Generates a variant in Pattern_or_expression.t from a constr_decl,
   e.g., [A (c_1, c_2)] or [A { f1 = c1 }] or [`A (c_1, c_2)] *)
let gen_variant (variant_decl : variant_declaration) ~prefix : Pattern_or_expression.t =
  match variant_decl.args with
  | Tuple (l, _) ->
    Pattern_or_expression.create_variant_value
      (Pattern_or_expression.create_idents (List.length l) ~prefix)
      ~constr_name:variant_decl.name.txt
      ~is_poly:false
  | Record (lbl_decls, _) ->
    Pattern_or_expression.create_variant_value
      [ Pattern_or_expression.create_record_value lbl_decls ~prefix ]
      ~constr_name:variant_decl.name.txt
      ~is_poly:false
  | Poly_variant (l, _) ->
    Pattern_or_expression.create_variant_value
      (Pattern_or_expression.create_idents (List.length l) ~prefix)
      ~constr_name:variant_decl.name.txt
      ~is_poly:true
;;

(* Generates an Either.t in Pattern_or_expression.t from a constr_decl and
   index of constr_decl in list to nest with Base.Either.Second,
   e.g., Second(Second(Second(First (c_1, c_2))))
*)
let gen_either (variant_decl : variant_declaration) ~i ~prefix : Pattern_or_expression.t =
  let variant_len =
    match variant_decl.args with
    | Tuple        (l,         _) -> List.length l
    | Record       (lbl_decls, _) -> List.length lbl_decls
    | Poly_variant (l,         _) -> List.length l
  in
  let base_param = Pattern_or_expression.create_nested_tuple variant_len ~prefix in
  Pattern_or_expression.create_nested_variant_value
    i
    ~ind_constr:"Base.Either.Second"
    ~base_expr:
      (Pattern_or_expression.create_variant_value
         ~constr_name:"Base.Either.First"
         ~is_poly:false
         [ base_param ])
    ~is_poly:false
;;

(* Generates implementation for a function that matches on the input argument
   given a list of cases *)
let gen_match_fun cases ~fun_param_name ~loc : expression =
  let fun_param_string = Located.mk ~loc fun_param_name          in
  let fun_param_ident  = Located.mk ~loc (Lident fun_param_name) in
  pexp_fun
    ~loc
    Nolabel
    None
    (ppat_var   ~loc fun_param_string                       )
    (pexp_match ~loc (pexp_ident ~loc fun_param_ident) cases)
;;

(* Generate conversion functions

   Either to variant conversion
   {[
     fun input ->
       match input with
       | First (c1, (c2, (c3, ()))) -> FirstConstr (c_1, c_2, c_3)
       | Second (First ()) -> SecondConstr
       | Second (Second (First (c1, (c2, (c3, ()))))) -> ThirdConstr (c1, c2)
       | Second (Second (Second nothing)) -> Base.Nothing.unreachable_code nothing
   ]}

   variant to Either conversion
   {[
     fun input ->
       match input with
       | FirstConstr (c_1, c_2, c_3) -> First (c_1, (c_2, (c_3, ())))
       | SecondConstr -> Second (First ())
       | ThirdConstr (c_1, c_2) -> Second (Second (First (c_1, (c_2, ()))))
   ]}
*)

let generate_conversion_funs (variant_decls : variant_declaration list) ~loc =
  let prefix         = "c_"    in
  let fun_param_name = "input" in
  let variants_and_eithers =
    List.mapi variant_decls ~f:(fun i variant_decl ->
      gen_variant variant_decl ~prefix, gen_either variant_decl ~i ~prefix)
  in
  let generate_either_to_variant_cases =
    List.map variants_and_eithers ~f:(fun (variant, either) ->
      case
        ~lhs:(Pattern_or_expression.to_pattern either ~loc)
        ~guard:None
        ~rhs:(Pattern_or_expression.to_expression variant ~loc))
  in
  (* Extra case to match Nothing.t *)
  let generate_either_to_variant_unreachable =
    (* Identifier applied in RHS must match [base_expr] in LHS *)
    case
      ~lhs:
        (Pattern_or_expression.to_pattern
           ~loc
           (Pattern_or_expression.create_nested_variant_value
              (List.length variant_decls)
              ~ind_constr:"Base.Either.Second"
              ~base_expr:(Pattern_or_expression.create_base "nothing")
              ~is_poly:false))
      ~guard:None
      ~rhs:[%expr Base.Nothing.unreachable_code nothing]
  in
  let generate_either_to_variant =
    gen_match_fun
      (generate_either_to_variant_cases @ [ generate_either_to_variant_unreachable ])
      ~fun_param_name
      ~loc
  in
  let generate_variant_to_either_cases =
    List.map variants_and_eithers ~f:(fun (variant, either) ->
      case
        ~lhs:(Pattern_or_expression.to_pattern variant ~loc)
        ~guard:None
        ~rhs:(Pattern_or_expression.to_expression either ~loc))
  in
  let generate_variant_to_either =
    gen_match_fun generate_variant_to_either_cases ~fun_param_name ~loc
  in
  generate_either_to_variant, generate_variant_to_either
;;

(* Generates implementation for calling apply_iso given a 'a M.t *)
let generate_impl_variant_apply_iso
      ~context
      (variant_decls : variant_declaration list)
      (of_variant_expr : expression)
  : expression
  =
  let ({ loc; module_; _ } : Utils.context) = context in
  let apply_iso_expr = pexp_ident ~loc (Located.mk ~loc (Ldot (module_, "apply_iso"))) in
  let either_to_variant_expr, variant_to_either_expr =
    generate_conversion_funs variant_decls ~loc
  in
  [%expr
    [%e apply_iso_expr]
      [%e of_variant_expr]
      [%e either_to_variant_expr]
      [%e variant_to_either_expr]]
;;

let generate_impl_variant_helper
      (variant_decls : variant_declaration list)
      ~context
      ~generate_type_directed_value
      ~generate_impl_record_structure
  : expression
  =
  generate_impl_variant_apply_iso
    ~context
    variant_decls
    (generate_impl_of_variant
       variant_decls
       ~context
       ~generate_type_directed_value
       ~generate_impl_record_structure)
;;
