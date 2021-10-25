open Base
open Type_directed
include Converters_intf

module type Simple_without_either = sig
  type 'a t
  type 'a attribute

  val apply_iso : 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val unit : unit t
end

module Of_simple_without_either (X : Simple_without_either) = struct
  module T = struct
    type 'a t         = 'a X.t
    type 'a attribute = 'a X.attribute
  end

  let apply_iso = X.apply_iso

  let rec of_tuple : type a len. (a, len) Tuple(T).t -> a T.t =
    fun t ->
    match t with
    | [ v1; v2 ]                -> X.both v1 (X.both v2 X.unit)
    | v1 :: (_ :: _ :: _ as tl) -> X.both v1 (of_tuple tl)
  ;;

  let rec of_record : type a len. (a, len) Record(T).t -> a T.t =
    fun r ->
      match r with
      | [ { value; _ } ]               -> X.both value X.unit
      | { value; _ } :: (_ :: _ as tl) -> X.both value (of_record tl)
  ;;

  let rec of_variant_constructor : type a len. (a, len) Variant_constructor(T).t -> a T.t =
    fun vc ->
      match vc with
      | []       -> X.unit
      | hd :: tl -> X.both hd (of_variant_constructor tl)
  ;;

  let of_variant _ = failwith "Cannot support variants without either"
end

module Of_simple (X : Simple) = struct
  include Of_simple_without_either (struct
      include X

      type 'a attribute = Nothing.t
    end)

  let rec of_variant : type a len. (a, len) Variant(T).t -> a T.t =
    fun v ->
    match v with
    | [] -> X.nothing
    | { value = variant; _ } :: tl ->
      let tdv_head =
        match variant with
        | Labelled r    -> of_record r
        | Unlabelled vc -> of_variant_constructor vc
      in
      X.either tdv_head (of_variant tl)
  ;;
end

module Of_simple_with_key (X : Simple_with_key) = struct
  include Of_simple_without_either (X)

  let rec of_record : type a len. (a, len) Record(T).t -> a T.t =
    fun r ->
    match r with
    | [ v ]               -> X.both_key v X.unit
    | v :: (_ :: _ as tl) -> X.both_key v (of_record tl)
  ;;

  let rec of_variant : type a len. (a, len) Variant(T).t -> a T.t =
    fun v ->
      match v with
      | [] -> X.nothing
      | { value = variant; attribute; name } :: tl ->
        let tdv_head =
          match variant with
          | Labelled r    -> of_record r
          | Unlabelled vc -> of_variant_constructor vc
        in
        X.either_key { value = tdv_head; attribute; name } (of_variant tl)
  ;;
end

module Of_applicative (X : Applicative.S) = struct
  module X_simple_without_either = struct
    type 'a t         = 'a X.t
    type 'a attribute = Nothing.t

    let apply_iso instance f _ = X.map ~f instance
    let both                   = X.both
    let unit                   = X.return ()
  end

  include Of_simple_without_either (X_simple_without_either)
end
