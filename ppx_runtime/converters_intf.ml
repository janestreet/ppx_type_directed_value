open Base
open Type_directed

(* $MDX part-begin=simple_sig *)
module type Simple = sig
  type 'a t

  val apply_iso : 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t
  val both      : 'a t -> 'b t -> ('a * 'b) t
  val unit      : unit t
  val either    : 'a t -> 'b t -> ('a, 'b) Either.t t
  val nothing   : Nothing.t t
end

(* $MDX part-end *)


(** Note that there's a bit of asymmetry in that [Simple_with_key] has [both], [both_key],
    and [either_key] but not [either].  This is because [both] is necessary for tuples,
    but there's no analogously anonymous construct for variants. *)

(* $MDX part-begin=simple_with_key_sig *)
module type Simple_with_key = sig
  type 'a t
  type 'a attribute

  val apply_iso  : 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t
  val both       : 'a t -> 'b t -> ('a * 'b) t
  val unit       : unit t
  val nothing    : Nothing.t t
  val both_key   : ('a t, 'a attribute) Key.t -> 'b t -> ('a * 'b) t
  val either_key : ('a t, 'a attribute) Key.t -> 'b t -> ('a, 'b) Either.t t
end

(* $MDX part-end *)

module type Converters = sig
  module type Simple          = Simple
  module type Simple_with_key = Simple_with_key

  module Of_applicative (X : Applicative.S) :
    S with type 'a T.t = 'a X.t and type 'a T.attribute = Nothing.t

  module Of_simple (X : Simple) :
    S with type 'a T.t = 'a X.t and type 'a T.attribute = Nothing.t

  module Of_simple_with_key (X : Simple_with_key) :
    S with type 'a T.t = 'a X.t and type 'a T.attribute = 'a X.attribute
end
