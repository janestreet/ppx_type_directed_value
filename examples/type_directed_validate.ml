open Base
open Ppx_type_directed_value_runtime

type 'a t         = 'a Validate.check
type 'a attribute = Name of string

include Converters.Of_simple_with_key (struct
    type nonrec 'a t         = 'a t
    type nonrec 'a attribute = 'a attribute

    let apply_iso t  _  f' x   = t (f' x)
    let both      t1 t2 (x, y) = Validate.combine (t1 x) (t2 y)

    let nothing : Nothing.t t = function
      | _ -> .
    ;;

    let unit = Validate.pass_unit

    let name (key : _ Type_directed.Key.t) =
      match key.attribute with
      | Some (Name name) -> name
      | None             -> key.name
    ;;

    let both_key (t1_key : (_ t, _) Type_directed.Key.t) (t2 : _ t) (x, y) =
      Validate.combine (Validate.name (name t1_key) (t1_key.value x)) (t2 y)
    ;;

    let either_key (t1_key : (_ t, _) Type_directed.Key.t) (t2 : _ t) (x : _ Either.t) =
      match x with
      | First  x -> Validate.name (name t1_key) (t1_key.value x)
      | Second y -> t2 y
    ;;
  end)
