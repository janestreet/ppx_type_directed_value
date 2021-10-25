open Core

let validate_int =
  Validate.bounded
    ~name:(fun _ -> "int")
    ~lower:(Incl 15)
    ~upper:Unbounded
    ~compare:[%compare: int]
;;

let validate_string s =
  if String.mem s ' ' then Validate.pass else Validate.fail "doesn't contain a space"
;;

type t =
  { f1 : int
  ; f2 : string
  }
[@@deriving validate]

type t_variant =
  | A of int
  | B of string * int
[@@deriving validate]

type t_different_name =
  { f1 : int [@validate Name "new-name"]
  ; f2 : string
  }
[@@deriving validate]

let%expect_test "validate_basic" =
  print_s [%sexp (Validate.result (validate { f1 = 10; f2 = "hello" }) : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("validation errors"
      ((f1 "value int < bound int") (f2 "doesn't contain a space")))) |}]
;;

let%expect_test "validate_variant" =
  print_s [%sexp (Validate.result (validate_t_variant (A 1)) : unit Or_error.t)];
  [%expect {| (Error ("validation errors" ((A "value int < bound int")))) |}]
;;

let%expect_test "validate_different_name" =
  print_s
    [%sexp
      (Validate.result (validate_t_different_name { f1 = 9; f2 = "hello" })
       : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("validation errors"
      ((new-name "value int < bound int") (f2 "doesn't contain a space")))) |}]
;;
