open Base

module type Type_directed_value = sig
  type 'a t
  type 'a attribute
end

module Key = struct
  type ('a, 'attribute) t =
    { name      : string
    ; value     : 'a
    ; attribute : 'attribute option
    }
end

module Type_nat = struct
  type zero      = Zero
  type 'num succ = Succ of 'num
end

module Indexed_seq = struct
  module Any_length (M : T1) = struct
    type ('a, 'length) t =
      | [] : (unit, Type_nat.zero) t
      | ( :: ) : 'a M.t * ('b, 'l) t -> ('a * 'b, 'l Type_nat.succ) t
  end

  module Length_at_least_one (M : T1) = struct
    type ('a, 'length) t = ('a, 'length Type_nat.succ) Any_length(M).t
  end

  module Length_at_least_two (M : T1) = struct
    type ('a, 'length) t = ('a, 'length Type_nat.succ Type_nat.succ) Any_length(M).t
  end
end

module Variant_constructor = Indexed_seq.Any_length
module Tuple               = Indexed_seq.Length_at_least_two

module Record (M : Type_directed_value) = struct
  module M_key = struct
    type 'a t = ('a M.t, 'a M.attribute) Key.t
  end

  type ('a, 'length) t = ('a, 'length) Indexed_seq.Length_at_least_one(M_key).t
end

module Variant (M : Type_directed_value) = struct
  type 'a variant =
    | Unlabelled : ('a, 'length) Variant_constructor(M).t -> 'a variant
    | Labelled   : ('a, 'length) Record             (M).t -> 'a variant

  type ('a, 'length) t =
    | [] : (Nothing.t, Type_nat.zero) t
    | ( :: ) :
        ('a variant, 'a M.attribute) Key.t * ('b, 'l) t
        -> (('a, 'b) Either.t, 'l Type_nat.succ) t
end

module type S = sig
  module T : Type_directed_value

  val apply_iso  : 'a T.t -> ('a -> 'b) -> ('b -> 'a) -> 'b T.t
  val of_tuple   : ('a, 'length) Tuple(T).t -> 'a T.t
  val of_record  : ('a, 'length) Record(T).t -> 'a T.t
  val of_variant : ('a, 'length) Variant(T).t -> 'a T.t
end
