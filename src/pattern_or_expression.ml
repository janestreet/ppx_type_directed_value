open Base
open Import

type t =
  | Unit
  | Base        of string
  | Tuple       of t list
  | Field       of (Longident.t * t) list
  | Constructor of Longident.t * t option
  | Poly_constructor of string * t option

let rec to_pattern ~loc = function
  | Unit    -> [%pat? ()]
  | Base s  -> ppat_var ~loc (Located.mk ~loc s)
  | Tuple l -> ppat_tuple ~loc (List.map l ~f:(to_pattern ~loc))
  | Field r ->
    ppat_record
      ~loc
      (List.map r ~f:(fun (lident, t) -> Located.mk ~loc lident, to_pattern ~loc t))
      Closed
  | Constructor (lident, t_opt) ->
    ppat_construct ~loc (Located.mk ~loc lident) (Option.map t_opt ~f:(to_pattern ~loc))
  | Poly_constructor (s, t_opt) ->
    ppat_variant ~loc s (Option.map t_opt ~f:(to_pattern ~loc))
;;

let rec to_expression ~loc = function
  | Unit    -> [%expr ()]
  | Base s  -> pexp_ident ~loc (Located.mk ~loc (Longident.parse s))
  | Tuple l -> pexp_tuple ~loc (List.map l ~f:(to_expression ~loc))
  | Field r ->
    pexp_record
      ~loc
      (List.map r ~f:(fun (lident, t) -> Located.mk ~loc lident, to_expression ~loc t))
      None
  | Constructor (lident, t_opt) ->
    pexp_construct
      ~loc
      (Located.mk ~loc lident)
      (Option.map t_opt ~f:(to_expression ~loc))
  | Poly_constructor (s, t_opt) ->
    pexp_variant ~loc s (Option.map t_opt ~f:(to_expression ~loc))
;;

let string_of_expr_of_t t =
  Ppxlib_ast.Pprintast.string_of_expression @@ to_expression ~loc:Location.none t
;;

let create_base ident = Base ident

let create_idents len ~prefix =
  List.init len ~f:(fun i -> Base (prefix ^ Int.to_string i))
;;

let create_tuple len ~prefix =
  if len < 2
  then failwith [%string "created_tuple: len must be >= 2 but was %{len#Int}"]
  else Tuple (create_idents len ~prefix)
;;

let create_nested_tuple len ~prefix =
  List.rev (create_idents len ~prefix)
  |> List.fold ~init:Unit ~f:(fun acc ident -> Tuple [ ident; acc ])
;;

let create_record_value lbl_decls ~prefix =
  let fields = List.map lbl_decls ~f:(fun { pld_name; _ } -> Lident pld_name.txt) in
  let idents = create_idents (List.length lbl_decls) ~prefix                      in
  (* idents have same length as fields by definition *)
  Field (List.zip_exn fields idents)
;;

let create_variant_value params ~constr_name ~is_poly =
  let t_opt =
    match params with
    | []        -> None
    | [ param ] -> Some param
    | params    -> Some (Tuple params)
  in
  if is_poly
  then Poly_constructor (                constr_name, t_opt)
  else Constructor      (Longident.parse constr_name, t_opt)
;;

let rec create_nested_variant_value len ~ind_constr ~base_expr ~is_poly =
  match len with
  | 0            -> base_expr
  | i when i > 0 ->
    create_variant_value
      ~constr_name:ind_constr
      ~is_poly
      [ create_nested_variant_value (len - 1) ~ind_constr ~base_expr ~is_poly ]
  | _ ->
    failwith
      [%string
        "created_nested_variant_value: len must be non-negative but was %{len#Int}"]
;;
