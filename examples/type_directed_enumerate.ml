open Base
open Ppx_type_directed_value_runtime

type 'a t = 'a list

include Converters.Of_simple (struct
    type nonrec 'a t = 'a t

    let apply_iso instance f _ = List.map ~f instance
    let both hd tl = List.concat_map hd ~f:(fun a -> List.map tl ~f:(fun b -> a, b))

    let either hd tl =
      List.map hd ~f:(fun a -> Either.First a) @ List.map tl ~f:(fun b -> Either.Second b)
    ;;

    let unit    = [ () ]
    let nothing = []
  end)
