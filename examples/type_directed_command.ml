(* $MDX part-begin=of_applicative *)
open Ppx_type_directed_value_runtime
include Converters.Of_applicative (Core.Command.Param)

(* $MDX part-end *)
