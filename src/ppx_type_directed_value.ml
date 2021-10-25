open Base
open Import

(* Generate function skeleton with parameters named according to the type variables
   that appear in the type definition *)
let rec generate_impl_fun_params
          (params : (core_type * (variance * injectivity)) list)
          ~loc
          ~body_expr
          ~module_
  : expression
  =
  match params with
  | []               -> body_expr
  | (type_, _) :: tl ->
    let param_pat =
      match type_.ptyp_desc with
      | Ptyp_any   -> ppat_any ~loc
      | Ptyp_var v -> ppat_var ~loc (Located.mk ~loc (Utils.get_type_var_param_name v))
      | _          -> Location.raise_errorf ~loc "Should not appear in type declaration params"
    in
    pexp_fun
      ~loc
      Nolabel
      None
      param_pat
      (generate_impl_fun_params tl ~loc ~body_expr ~module_)
;;

let generate_impl context : structure_item =
  let ({ loc; td; module_; _ } : Utils.context) = context in
  let impl_expr =
    match td.ptype_kind with
    | Ptype_variant constr_decls ->
      Expand_type_directed.generate_impl_variant
        (List.map constr_decls ~f:Expand_variant.constr_decl_to_variant_decl)
        ~context
    | Ptype_record lbl_decls ->
      Expand_type_directed.generate_impl_record lbl_decls ~context
    | Ptype_abstract ->
      (match td.ptype_manifest with
       | Some core_type ->
         Expand_type_directed.generate_type_directed_value core_type ~context
       | None ->
         Location.raise_errorf ~loc "Cannot generate implementation for abstract types")
    | Ptype_open ->
      Location.raise_errorf ~loc "Cannot generate implementation for open types"
  in
  let stri_name =
    Located.mk ~loc (Utils.get_type_directed_name ~type_:td.ptype_name.txt ~module_)
  in
  pstr_value
    ~loc
    Nonrecursive
    [ value_binding
        ~loc
        ~pat:(ppat_var ~loc stri_name)
        ~expr:
          (generate_impl_fun_params td.ptype_params ~loc ~body_expr:impl_expr ~module_)
    ]
;;

let generate_type_directed_value_str
      ~loc
      ~path:_
      ~(module_ : longident)
      ~attrs_decls
      (_, tds)
  : structure_item list
  =
  List.map tds ~f:(fun td -> generate_impl { loc; module_; td; attrs_decls })
;;

(* Generates type of a type-directed value based on the base type *)
(* e.g. 'a ==> 'a M.t *)
let generate_type_directed_type base_type ~module_ ~loc =
  ptyp_constr ~loc (Located.mk ~loc (Ldot (module_, "t"))) [ base_type ]
;;

(* Iterates through type parameters and construct arrow types as needed *)
(* e.g. ('a, 'b) t ==> 'a M.t -> 'b M.t -> ('a, 'b) t M.t *)
let rec generate_sig_from_params
          (params : (core_type * (variance * injectivity)) list)
          ~loc
          ~final_type
          ~module_
  : core_type
  =
  match params with
  | []               -> final_type
  | (type_, _) :: tl ->
    ptyp_arrow
      ~loc
      Nolabel
      (generate_type_directed_type type_               ~module_ ~loc)
      (generate_sig_from_params    tl ~loc ~final_type ~module_     )
;;

let generate_sig ~loc ~(module_ : longident) (td : type_declaration) : signature_item =
  match td.ptype_kind with
  | Ptype_abstract | Ptype_variant _ | Ptype_record _ ->
    let sig_name =
      Located.mk ~loc (Utils.get_type_directed_name ~type_:td.ptype_name.txt ~module_)
    in
    let final_type =
      generate_type_directed_type
        (ptyp_constr
           ~loc
           (Located.mk ~loc (Longident.parse td.ptype_name.txt))
           (List.map td.ptype_params ~f:(fun (ty, _) -> ty)))
        ~module_
        ~loc
    in
    let sig_type = generate_sig_from_params td.ptype_params ~loc ~final_type ~module_ in
    psig_value ~loc (value_description ~loc ~name:sig_name ~type_:sig_type ~prim:[])
  | Ptype_open ->
    Location.raise_errorf ~loc "Cannot generate implementation for open types"
;;

let generate_type_directed_value_sig ~loc ~path:_ ~(module_ : longident) (_, tds)
  : signature_item list
  =
  List.map tds ~f:(generate_sig ~loc ~module_)
;;

let () =
  let register_deriver module_name =
    let module_name  = String.drop_prefix module_name 1                    in
    let module_ident = Longident.parse module_name                         in
    let ppx_name     = String.lowercase @@ Longident.last_exn module_ident in
    let attrs_decls  = Attrs.declare_attrs ppx_name                        in
    ignore
      (Deriving.add
         ppx_name
         ~str_type_decl:
           (Deriving.Generator.make_noarg
              (generate_type_directed_value_str ~module_:module_ident ~attrs_decls))
         ~sig_type_decl:
           (Deriving.Generator.make_noarg
              (generate_type_directed_value_sig ~module_:module_ident))
       : Deriving.t)
  in
  Driver.add_arg
    "-module"
    (String register_deriver)
    ~doc:"<module> Type directed module to derive"
;;

module Pattern_or_expression = Pattern_or_expression
