let _ = "-*-tuareg-*-"

(* The above dummy let binding is just to get ocaml formatting in emacs for the output
   file. *)

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

module M : sig
  type ('a, 'b) polymorphic =
    | Pa of 'a * 'b
    | Pb of 'b * 'a * int
  [@@deriving type_directed_equal]

  type inst_poly = A of int list list list [@@deriving type_directed_equal]

  type t =
    { f : int
    ; g : string
    }
  [@@deriving type_directed_equal]

  type test =
    | A
    | B of int
    | C of int * string
    | D of string * int
    | E of M_inner.t * M_inner.t
  [@@deriving type_directed_equal]

  type lbl_variant =
    | W
    | X of int
    | Y of
        { f1 : int
        ; f2 : string
        }
    | Z of int * int
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
    | `C of int * int * int
    ]
  [@@deriving type_directed_equal]
end = struct
  type ('a, 'b) polymorphic =
    | Pa of 'a * 'b
    | Pb of 'b * 'a * int
  [@@deriving type_directed_equal]

  type inst_poly = A of int list list list [@@deriving type_directed_equal]

  type t =
    { f : int
    ; g : string
    }
  [@@deriving type_directed_equal]

  type test =
    | A
    | B of int
    | C of int * string
    | D of string * int
    | E of M_inner.t * M_inner.t
  [@@deriving type_directed_equal]

  type 'a alias_j = 'a           [@@deriving type_directed_equal]
  type alias_k    = test alias_j [@@deriving type_directed_equal]
  type ('a, 'b, 'c) alias_l = 'b [@@deriving type_directed_equal]

  type lbl_variant =
    | W
    | X of int
    | Y of
        { f1 : int
        ; f2 : string
        }
    | Z of int * int
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
    | `C of int * int * int
    ]
  [@@deriving type_directed_equal]

  type variant_args =
    | A of (int * string)
    | B of string * int
  [@@deriving type_directed_equal]

  type attrib_record =
    { f1 : int
    ; f2 : int [@type_directed_equal Ignore]
    }
  [@@deriving type_directed_equal]

  type attrib_variant_simple =
    | A [@type_directed_equal CustomCmp (fun _ _ -> true)]
    | B of
        { f1 : int
        ; f2 : string
        } [@type_directed_equal CustomCmp (fun _ _ -> true)]
  [@@deriving type_directed_equal]

  module Custom_demonstration = struct
    (* $MDX part-begin=attribute-custom *)
    let always_equal : int -> int -> bool = fun _ _ -> true

    type t = { f1 : int [@type_directed_equal.custom always_equal] }
    [@@deriving type_directed_equal]

    (* $MDX part-end *)
  end
end
