open Ppx_type_directed_value_runtime

type 'a t = 'a -> 'a -> bool

type 'a attribute =
  | CustomCmp of ('a -> 'a -> bool)
  | Ignore

include Type_directed.S with type 'a T.t = 'a t and type 'a T.attribute = 'a attribute
