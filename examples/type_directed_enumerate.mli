open Base
open Ppx_type_directed_value_runtime
include Type_directed.S with type 'a T.t = 'a list and type 'a T.attribute = Nothing.t
