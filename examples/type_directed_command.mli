open Core
open Ppx_type_directed_value_runtime

include
  Type_directed.S
  with type 'a T.t         = 'a Command.Param.t
  with type 'a T.attribute = Nothing.t
