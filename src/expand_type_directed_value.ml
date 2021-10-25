open Base
open Import

let rec generate_type_directed_value_helper
          (type_ : core_type)
          ~context
          ~generate_impl_tuple
          ~generate_impl_variant
  : expression
  =
  let ({ module_; loc; _ } : Utils.context) = context in
  match type_.ptyp_desc with
  | Ptyp_constr (lident, constrs) ->
    let field_type = Longident.last_exn lident.txt in
    let type_directed_impl_name =
      Utils.get_type_directed_name ~type_:field_type ~module_
    in
    let type_directed_ident =
      match lident.txt with
      | Ldot (qualifier, _) -> Ldot (qualifier, type_directed_impl_name)
      | Lident _            -> Lident type_directed_impl_name
      | Lapply _            ->
        Location.raise_errorf
          ~loc
          "[longident]s should not end in [Lapply] in type positions"
    in
    (* Get type-directed values for type parameters to apply to the original
       type-directed value *)
    let type_directed_constr_values =
      List.map constrs ~f:(fun (typ : core_type) ->
        ( Nolabel
        , generate_type_directed_value_helper
            typ
            ~context
            ~generate_impl_tuple
            ~generate_impl_variant ))
    in
    (match type_directed_constr_values with
     | []   -> pexp_ident ~loc (Located.mk ~loc type_directed_ident                        )
     | vals -> pexp_apply ~loc (pexp_ident ~loc (Located.mk ~loc type_directed_ident)) vals)
  | Ptyp_var var ->
    pexp_ident ~loc (Located.mk ~loc (Lident (Utils.get_type_var_param_name var)))
  | Ptyp_tuple types -> generate_impl_tuple types ~context
  | Ptyp_variant (row_fields, _, _) ->
    generate_impl_variant
      (List.map row_fields ~f:(Expand_variant.row_field_to_variant_decl ~loc))
      ~context
  | _ -> Location.raise_errorf ~loc "Cannot generate implementation for this type"
;;
