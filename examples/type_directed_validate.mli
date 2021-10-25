open Ppx_type_directed_value_runtime

type 'a t         = 'a Validate.check
type 'a attribute = Name of string

include Type_directed.S with type 'a T.t = 'a t and type 'a T.attribute = 'a attribute
