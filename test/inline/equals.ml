open Core

let type_directed_equal_int    = Int.equal
let type_directed_equal_string = String.equal

let rec type_directed_equal_list gen_cmp l1 l2 =
  match l1, l2 with
  | hd1 :: tl1, hd2 :: tl2 ->
    gen_cmp hd1 hd2 && (type_directed_equal_list gen_cmp) tl1 tl2
  | []        , []         -> true
  | _         , _          -> false
;;

module M_inner = struct
  type t = { f1 : string } [@@deriving type_directed_equal]
end

module M = struct
  type ('a, 'b) polymorphic =
    | Pa of 'a * 'b
    | Pb of 'b * 'a * int
  [@@deriving type_directed_equal]

  type inst_poly = A of int list list list [@@deriving type_directed_equal]

  type t =
    { f : int
    ; g : int
    }
  [@@deriving type_directed_equal]

  type t2 =
    { f : int
    ; g : int
    }
  [@@deriving type_directed_equal]

  type with_inner =
    | A
    | B of int
    | C of int * int
    | D of int * int
    | E of M_inner.t * M_inner.t
  [@@deriving type_directed_equal]

  type lbl_variant =
    | A
    | B of int
    | C of
        { f1 : int
        ; f2 : int
        }
    | D of int * int
  [@@deriving type_directed_equal]

  type 'a record_poly_with_pair =
    { rp1 : (int * int * int) list
    ; rp2 : 'a * int
    }
  [@@deriving type_directed_equal]

  type ('a, 'b) poly_tup = 'a * 'b * string [@@deriving type_directed_equal]

  type poly_variant =
    [ `A
    | `B of int
    | `C of int * int * [ `X | `Y of int ]
    ]
  [@@deriving type_directed_equal]

  type variant_args =
    | A of (int * string)
    | B of string * int
  [@@deriving type_directed_equal]

  type customcmp_attrib_record =
    { f1 : int [@type_directed_equal CustomCmp (fun _ _ -> false)]
    ; f2 : int
    }
  [@@deriving type_directed_equal]

  type ignore_attrib_record =
    { f1 : int
    ; f2 : int [@type_directed_equal Ignore]
    }
  [@@deriving type_directed_equal]

  type customcmp_attrib_variant =
    | A of int * int * int [@type_directed_equal CustomCmp (fun _ _ -> true)]
    | B of
        { f1 : int
        ; f2 : int
        } [@type_directed_equal CustomCmp (fun _ _ -> true)]
  [@@deriving type_directed_equal]

  type nested_attrib_lbl_variant =
    | A of
        { f1 : int [@type_directed_equal Ignore]
        ; f2 : int
        }
    | B of int
  [@@deriving type_directed_equal]

  type custom_tdv_record = { f1 : int [@custom fun _ _ -> false] }
  [@@deriving type_directed_equal]

  type qualified_custom_tdv_record =
    { f1 : int [@type_directed_equal.custom fun _ _ -> false] }
  [@@deriving type_directed_equal]

  type i =
    | M
    | N
  [@@deriving type_directed_equal]

  type 'a alias_j = 'a        [@@deriving type_directed_equal]
  type alias_k    = i alias_j [@@deriving type_directed_equal]
  type ('a, 'b, 'c) alias_l = 'b [@@deriving type_directed_equal]
end

let%expect_test "type_directed_equal_record_false" =
  print_s [%sexp (M.type_directed_equal { f = 10; g = 10 } { f = 100; g = 10 } : bool)];
  [%expect {| false |}]
;;

let%expect_test "type_directed_equal_record_conflict_fields_true" =
  print_s [%sexp (M.type_directed_equal_t2 { f = 10; g = 10 } { f = 10; g = 10 } : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_variant_true" =
  print_s [%sexp (M.type_directed_equal_with_inner (M.C (1, 2)) (M.C (1, 2)) : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_variant_true_qualified" =
  print_s
    [%sexp
      (M.type_directed_equal_with_inner
         (M.E ({ M_inner.f1 = "hello" }, { M_inner.f1 = "world" }))
         (M.E ({ M_inner.f1 = "hello" }, { M_inner.f1 = "world" }))
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_variant_false_different_tag" =
  print_s [%sexp (M.type_directed_equal_with_inner (M.C (1, 2)) (M.D (1, 2)) : bool)];
  [%expect {| false |}]
;;

let%expect_test "type_directed_equal_labelled_variant_false_different_tag" =
  print_s
    [%sexp
      (M.type_directed_equal_lbl_variant (M.C { f1 = 1; f2 = 2 }) (M.D (1, 2)) : bool)];
  [%expect {| false |}]
;;

let%expect_test "type_directed_equal_labelled_variant_false_ordering" =
  print_s
    [%sexp
      (M.type_directed_equal_lbl_variant (M.C { f2 = 1; f1 = 2 }) (M.C { f1 = 1; f2 = 2 })
       : bool)];
  [%expect {| false |}]
;;

let%expect_test "type_directed_equal_labelled_variant_true" =
  print_s
    [%sexp
      (M.type_directed_equal_lbl_variant (M.C { f1 = 1; f2 = 2 }) (M.C { f1 = 1; f2 = 2 })
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_polymorphic_true" =
  print_s
    [%sexp
      (M.type_directed_equal_polymorphic
         type_directed_equal_int
         type_directed_equal_string
         (M.Pb ("hello", 3, 2))
         (M.Pb ("hello", 3, 2))
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_polymorphic_false_different_tag" =
  print_s
    [%sexp
      (M.type_directed_equal_polymorphic
         type_directed_equal_int
         type_directed_equal_string
         (M.Pb ("hello", 3, 4      ))
         (M.Pa (         3, "hello"))
       : bool)];
  [%expect {| false |}]
;;

let%expect_test "type_directed_equal_poly_record_with_pair" =
  print_s
    [%sexp
      (M.type_directed_equal_record_poly_with_pair
         type_directed_equal_string
         { M.rp1 = [ 1, 2, 3; 4, 5, 6 ]; M.rp2 = "hello", 5 }
         { M.rp1 = [ 1, 2, 3; 4, 5, 6 ]; M.rp2 = "hello", 5 }
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_poly_tup" =
  print_s
    [%sexp
      (M.type_directed_equal_poly_tup
         type_directed_equal_string
         type_directed_equal_int
         ("hello", 4, "world")
         ("hello", 4, "world")
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_poly_variant" =
  print_s
    [%sexp
      (M.type_directed_equal_poly_variant (`C (1, 2, `Y 1)) (`C (1, 2, `Y 1)) : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_variant_args_false" =
  let a_constr_args = 1, "hello" in
  print_s
    [%sexp (M.type_directed_equal_variant_args (A a_constr_args) (B ("hello", 1)) : bool)];
  [%expect {| false |}]
;;

let%expect_test "type_directed_equal_variant_args_true" =
  let a_constr_args = 1, "hello" in
  print_s
    [%sexp (M.type_directed_equal_variant_args (A a_constr_args) (A (1, "hello")) : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_customcmp_record" =
  print_s
    [%sexp
      (M.type_directed_equal_customcmp_attrib_record
         { M.f1 = 10; f2 = 10 }
         { M.f1 = 10; f2 = 10 }
       : bool)];
  [%expect {| false |}]
;;

let%expect_test "type_directed_equal_ignore_record" =
  print_s
    [%sexp
      (M.type_directed_equal_ignore_attrib_record
         { M.f1 = 10; f2 = 12 }
         { M.f1 = 10; f2 = 10 }
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_customcmp_variant_same_constr" =
  print_s
    [%sexp
      (M.type_directed_equal_customcmp_attrib_variant (A (1, 2, 3)) (A (10, 20, 30))
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_customcmp_variant_diff_constr" =
  print_s
    [%sexp
      (M.type_directed_equal_customcmp_attrib_variant
         (A (1, 2, 3))
         (B { f1 = 10; f2 = 20 })
       : bool)];
  [%expect {| false |}]
;;

let%expect_test "type_directed_equal_customcmp_variant_diff_constr" =
  print_s
    [%sexp
      (M.type_directed_equal_nested_attrib_lbl_variant
         (A { f1 = 10; f2 = 20 })
         (A { f1 = 20; f2 = 20 })
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_custom_tdv_record" =
  print_s [%sexp (M.type_directed_equal_custom_tdv_record { f1 = 10 } { f1 = 10 } : bool)];
  [%expect {| false |}]
;;

let%expect_test "type_directed_equal_qualified_custom_tdv_record" =
  print_s
    [%sexp
      (M.type_directed_equal_qualified_custom_tdv_record { f1 = 10 } { f1 = 10 } : bool)];
  [%expect {| false |}]
;;

let%expect_test "type_directed_equal_alias_k_true" =
  print_s [%sexp (M.type_directed_equal_alias_k M M : bool)];
  [%expect {| true |}]
;;

let%expect_test "type_directed_equal_alias_k_false" =
  print_s [%sexp (M.type_directed_equal_alias_k N M : bool)];
  [%expect {| false |}]
;;

let%expect_test "type_directed_equal_alias_l" =
  print_s
    [%sexp
      (M.type_directed_equal_alias_l
         type_directed_equal_string
         type_directed_equal_int
         type_directed_equal_string
         10
         10
       : bool)];
  [%expect {| true |}]
;;
