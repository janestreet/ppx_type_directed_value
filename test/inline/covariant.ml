open Core

let type_directed_enumerate_bool = [ true; false ]
let type_directed_enumerate_unit = [ () ]
let type_directed_enumerate_option l = None :: List.map l ~f:(fun a -> Some a)

type t =
  { f1 : bool
  ; f2 : bool
  }
[@@deriving type_directed_enumerate]

type t_var =
  | A
  | B of bool * bool
[@@deriving type_directed_enumerate]

type t_poly =
  | Foo
  | Bar of bool
  | Baz of [ `A | `B of unit option ]
[@@deriving type_directed_enumerate]

type t_empty = | [@@deriving type_directed_enumerate]

type t_record_poly =
  { foo : [ `A | `B ]
  ; bar : [ `C | `D ]
  }
[@@deriving type_directed_enumerate]

let%test "enumerate_record" =
  [%compare: _ list]
    type_directed_enumerate
    [ { f1 = true;  f2 = true  }
    ; { f1 = true;  f2 = false }
    ; { f1 = false; f2 = true  }
    ; { f1 = false; f2 = false }
    ]
  = 0
;;

let%test "enumerate_variant" =
  [%compare: _ list]
    type_directed_enumerate_t_var
    [ A; B (true, true); B (true, false); B (false, true); B (false, false) ]
  = 0
;;

let%test "enumerate_poly" =
  [%compare: _ list]
    type_directed_enumerate_t_poly
    [ Foo; Bar true; Bar false; Baz `A; Baz (`B None); Baz (`B (Some ())) ]
  = 0
;;

let%test "enumerate_record_poly" =
  [%compare: _ list]
    type_directed_enumerate_t_record_poly
    [ { foo = `A; bar = `C }
    ; { foo = `A; bar = `D }
    ; { foo = `B; bar = `C }
    ; { foo = `B; bar = `D }
    ]
  = 0
;;

let%test "enumerate_empty" = [%compare: _ list] type_directed_enumerate_t_empty [] = 0
