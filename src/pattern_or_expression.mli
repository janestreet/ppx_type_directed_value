open Base
open Import

(** Canonical representation for a subset of patterns/expressions *)
type t

val to_pattern    : loc:Location.t -> t -> pattern
val to_expression : loc:Location.t -> t -> expression
val string_of_expr_of_t : t -> string

(** Some mdx-in-mli setup:

    {[
      open Base;;
      open Ppx_type_directed_value.Pattern_or_expression;;
      open Ppxlib;;
      open Ast_builder.Default;;
    ]}
*)

(** Creates a base identifier *)
val create_base : string -> t

(** Creates a list of identifiers prefixed by [prefix] of a given length

    {[
      # List.map (create_idents 3 ~prefix:"i_") ~f:(string_of_expr_of_t)
      - : string list = ["i_0"; "i_1"; "i_2"]
    ]}
*)
val create_idents : int -> prefix:string -> t list

(** Creates a tuple of identifiers prefixed by [prefix] of a given length
    Length of the tuple must be >= 2

    {[
      # string_of_expr_of_t @@ create_tuple 3 ~prefix:"t_";;
      - : string = "(t_0, t_1, t_2)"
    ]}
*)
val create_tuple : int -> prefix:label -> t

(** Creates a nested tuple that models a possibly empty list

    {[
      # string_of_expr_of_t @@ create_nested_tuple 0 ~prefix:"t_";;
      - : string = "()"
    ]}

    {[
      # string_of_expr_of_t @@ create_nested_tuple 1 ~prefix:"t_";;
      - : string = "(t_0, ())"
    ]}

    {[
      # string_of_expr_of_t @@ create_nested_tuple 2 ~prefix:"t_";;
      - : string = "(t_0, (t_1, ()))"
    ]}
*)
val create_nested_tuple : int -> prefix:label -> t

(** Creates a record value where each field has value/pattern obtained from
    [create_idents]

    {[
      let loc = Location.none

      let lbl_decls =
        [ label_declaration
            ~loc
            ~name:(Located.mk ~loc "f1")
            ~mutable_:Immutable
            ~type_:(ptyp_any ~loc)
        ; label_declaration
            ~loc
            ~name:(Located.mk ~loc "f2")
            ~mutable_:Immutable
            ~type_:(ptyp_any ~loc)
        ]
    ]}

    {[
      # string_of_expr_of_t @@ create_record_value lbl_decls ~prefix:"f_";;
      - : string = "{ f1 = f_0; f2 = f_1 }"
    ]}
*)
val create_record_value : label_declaration list -> prefix:label -> t

(** Creates a value of a variant type given a list of parameters

    {[
      let one_ident = create_idents 1 ~prefix:"c_"
      let three_idents = create_idents 3 ~prefix:"c_"
    ]}

    {[
      # string_of_expr_of_t @@ create_variant_value [] ~constr_name:"A" ~is_poly:false;;
      - : string = "A"
    ]}

    {[
      # string_of_expr_of_t
      @@ create_variant_value one_ident ~constr_name:"A" ~is_poly:false;;
      - : string = "A c_0"
    ]}

    {[
      # string_of_expr_of_t
      @@ create_variant_value three_idents ~constr_name:"A" ~is_poly:false;;
      - : string = "A (c_0, c_1, c_2)"
    ]}
*)
val create_variant_value : t list -> constr_name:string -> is_poly:bool -> t

(** Creates a nested value with the given nesting depth, where the base
    expression is [base_expr] and each outer nesting is constructed with
    [ind_constr]

    {[
      let base_expr = create_base "nothing"
      let ind_constr = "Second"
    ]}

    {[
      # string_of_expr_of_t
      @@ create_nested_variant_value 0 ~ind_constr ~base_expr ~is_poly:false;;
      - : string = "nothing"
    ]}

    {[
      # string_of_expr_of_t
      @@ create_nested_variant_value 2 ~ind_constr ~base_expr ~is_poly:false;;
      - : string = "Second (Second nothing)"
    ]}
*)
val create_nested_variant_value
  :  int
  -> ind_constr:string
  -> base_expr:t
  -> is_poly:bool
  -> t
