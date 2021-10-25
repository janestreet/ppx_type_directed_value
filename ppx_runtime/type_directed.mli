open Base

(* $MDX part-begin=tdv_sig *)
module type Type_directed_value = sig
  type 'a t

  (** ['a attribute] allows supporting attributes such as

      {[
        type t =
          { foo : int [@my_module attr]
          ; ...
          } [@@deriving my_module]
      ]}

      where in the above example [attr] would have type [int My_module.attribute].

      If you don't want to use this feature, you can define [type 'a attribute =
      Nothing.t]. *)
  type 'a attribute
end

(* $MDX part-end *)

(** Packages the name of the constructor (variants) / field (records)
    and the associated type-directed value.

    E.g., the corresponding [Key] of the record field [f1 : int] is
    [{ name = "f1"; value = M_int }]
    where M is the name of the module from which we are
    deriving type-directed values.
*)

(* $MDX part-begin=key_sig *)
module Key : sig
  type ('a, 'attribute) t =
    { name      : string
    ; value     : 'a
    ; attribute : 'attribute option
    }
end

(* $MDX part-end *)

(** Type-level naturals used for tracking size of a list  *)
module Type_nat : sig
  type zero      = Zero
  type 'num succ = Succ of 'num
end

(** A generic data structure that models a list of ['a M.t], where the first index
    of the GADT is the product of each type used to instantiate ['a M.t] in the list,
    and the second index tracks the length of the list using [Type_nat]
*)
module Indexed_seq : sig
  module Any_length (M : T1) : sig
    type ('a, 'length) t =
      | [] : (unit, Type_nat.zero) t
      | ( :: ) : 'a M.t * ('b, 'l) t -> ('a * 'b, 'l Type_nat.succ) t
  end

  module Length_at_least_one (M : T1) : sig
    type ('a, 'length) t = ('a, 'length Type_nat.succ) Any_length(M).t
  end

  module Length_at_least_two (M : T1) : sig
    type ('a, 'length) t = ('a, 'length Type_nat.succ Type_nat.succ) Any_length(M).t
  end
end

(** Data structure that packages the associated type-directed values
    of a specific variant constructor

    e.g. The corresponding Variant_constructor of the variant constructor
    A of int * string is

    {[
      [ M_int; M_string ]
    ]}

    with type [((int * (string * unit)), zero succ succ) t]
*)
module Variant_constructor = Indexed_seq.Any_length

(** Data structure that packages the associated type-directed values
    of a tuple

    E.g., the corresponding [Tuple] of the tuple [int * string] is

    {[
      [ M_int; M_string ]
    ]}

    with type [((int * (string * unit)), zero succ succ) t]

    This guarantees at compile time that the length of this
    [Indexed_seq] is >= 2
*)
module Tuple = Indexed_seq.Length_at_least_two

(** Data structure that packages the associated Keys of a record

    E.g., the corresponding [Record] of the following record

    {[
      type t =
        { f1 : int
        ; f2 : string
        }
    ]}

    is

    {[
      [ { name = "f1"; value = M_int }
      ; { name = "f2"; value = M_string }
      ]
    ]}

    with type ((int * (string * unit)), zero succ succ) t

    This guarantees at compile time that the length of this
    [Indexed_seq] is >= 1
*)
module Record (M : Type_directed_value) : sig
  module M_key : sig
    type 'a t = ('a M.t, 'a M.attribute) Key.t
  end

  type ('a, 'length) t = ('a, 'length) Indexed_seq.Length_at_least_one(M_key).t
end

(** Data structure that packages the associated [Key]s of a variant

    E.g., the corresponding [Variant] of the following variant

    {[
      type t =
        | A
        | B of int * string
        | C of { f:int }
    ]}

    is

    {[
      [ { name = "A"; value = Unlabelled [] }
      ; { name = "B"; value = Unlabelled [ M_int; M_string ] }
      ; { name = "C"; value = Labelled [ { name = "f"; value = M_int } ] }
      ]
    ]}

    with type

    {[
      (unit,
       (int * (string * unit), (int * unit, Nothing.t) Either.t)
         Either.t)
        Either.t
    ]}
*)
module Variant (M : Type_directed_value) : sig
  type 'a variant =
    | Unlabelled : ('a, 'length) Variant_constructor(M).t -> 'a variant
    | Labelled   : ('a, 'length) Record             (M).t -> 'a variant

  type ('a, 'length) t =
    | [] : (Nothing.t, Type_nat.zero) t
    | ( :: ) :
        ('a variant, 'a M.attribute) Key.t * ('b, 'l) t
        -> (('a, 'b) Either.t, 'l Type_nat.succ) t
end

(* $MDX part-begin=interface *)

(** Signature that a user-provided module should implement to guide the
    code generation of type-directed values *)
module type S = sig
  (** Type-directed value of interest *)
  module T : Type_directed_value

  (** Given transformations between two isomorphic types 'a, 'b,
      turns a 'a type-directed value to a 'b type-directed value
  *)
  val apply_iso  : 'a T.t -> ('a -> 'b) -> ('b -> 'a) -> 'b T.t

  val of_tuple   : ('a, 'length) Tuple(T).t -> 'a T.t
  val of_record  : ('a, 'length) Record(T).t -> 'a T.t
  val of_variant : ('a, 'length) Variant(T).t -> 'a T.t
end

(* $MDX part-end *)
