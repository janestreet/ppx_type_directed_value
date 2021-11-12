[@@@ocaml.warning "-K"]

open Core
open Ppx_type_directed_value_runtime
open Ppx_type_directed_value_runtime.Type_directed.Type_nat

(* $MDX part-begin=sexp_implementation *)
module _ = struct
  type 'a t =
    { sexp_of_t : 'a -> Sexp.t
    ; t_of_sexp : Sexp.t -> 'a
    }

  (* Don't implement any attributes for now *)
  type 'a attribute = Nothing.t
end

(* $MDX part-end *)

(* $MDX part-begin=equal_implementation *)
module T = struct
  type 'a t = 'a -> 'a -> bool
  type 'a attribute = Nothing.t
end

(* $MDX part-end *)

module type S = sig
  (* $MDX part-begin=tuple *)
  module _ (T : Type_directed.Type_directed_value) : sig
    type ('a, 'length) seq =
      | [] : (unit, zero) seq
      | ( :: ) : 'a T.t * ('b, 'l) seq -> ('a * 'b, 'l succ) seq

    type ('a, 'length) t = ('a, 'length succ succ) seq
  end

  (* $MDX part-end *)

  (* $MDX part-begin=record *)
  module _ (T : Type_directed.Type_directed_value) : sig
    type ('a, 'length) seq =
      | [] : (unit, zero) seq
      | ( :: ) :
          ('a T.t, 'a T.attribute) Type_directed.Key.t * ('b, 'l) seq
          -> ('a * 'b, 'l succ) seq

    type ('a, 'length) t = ('a, 'length succ) seq
  end

  (* $MDX part-end *)

  (* $MDX part-begin=variant *)
  module _ (T : Type_directed.Type_directed_value) : sig
    type 'a variant =
      | Unlabelled : ('a, 'length) Type_directed.Variant_constructor(T).t -> 'a variant
      | Labelled : ('a, 'length) Type_directed.Record(T).t -> 'a variant

    type ('a, 'length) t =
      | [] : (Nothing.t, zero) t
      | ( :: ) :
          ('a variant, 'a T.attribute) Type_directed.Key.t * ('b, 'l) t
          -> (('a, 'b) Either.t, 'l succ) t
  end
end

(* $MDX part-end *)

let t_int = Int.equal
let t_string = String.equal

(* $MDX part-begin=record_t *)
type t1 =
  { f1 : int
  ; f2 : string
  }

(* Generated value passed in to [of_record] *)
let t_t1 : (int * (string * unit), zero succ) Type_directed.Record(T).t =
  [ { name = "f1"; value = t_int; attribute = None }
  ; { name = "f2"; value = t_string; attribute = None }
  ]
;;

(* $MDX part-end *)

(* $MDX part-begin=variant_t *)
type t2 =
  | A
  | B of int * string
  | C of { f : int }

(* Generated value passed in to [of_variant] *)
let t_t2
  : ( (unit, (int * (string * unit), (int * unit, Nothing.t) Either.t) Either.t) Either.t
    , zero succ succ succ ) Type_directed.Variant(T).t
  =
  [ { name = "A"; value = Unlabelled []; attribute = None }
  ; { name = "B"; value = Unlabelled [ t_int; t_string ]; attribute = None }
  ; { name = "C"
    ; value = Labelled [ { name = "f"; value = t_int; attribute = None } ]
    ; attribute = None
    }
  ]
;;

(* $MDX part-end *)

(* $MDX part-begin=apply_iso *)
let apply_iso instance _f f' x y = instance (f' x) (f' y)

(* $MDX part-end *)

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

(* $MDX part-begin=attribute-user *)
let f2_attrib : int Ppx_type_directed_value_examples.Validate.attribute = Name "new-name"

type attrib_record =
  { f1 : int
  ; f2 : int [@validate f2_attrib]
  }
[@@deriving validate]

let a_attrib : unit Ppx_type_directed_value_examples.Validate.attribute =
  Name "new-name-1"
;;

let b_attrib : (int * (string * unit)) Ppx_type_directed_value_examples.Validate.attribute
  =
  Name "new-name-2"
;;

type attrib_variant_simple =
  | A [@validate a_attrib]
  | B of
      { f1 : int
      ; f2 : string
      } [@validate b_attrib]
[@@deriving validate]

(* $MDX part-end *)

let command_int =
  let open Command.Param in
  flag "int" (required int) ~doc:"int"
;;

let command_string =
  let open Command.Param in
  flag "string" (required string) ~doc:"string"
;;

(* $MDX part-begin=of_applicative *)
type t =
  { f1 : int
  ; f2 : string
  }
[@@deriving command]

let command : (int * (string * unit)) Ppx_type_directed_value_examples.Command.T.t =
  let open Command.Param in
  both command_int (both command_string (return ()))
;;

(* $MDX part-end *)
