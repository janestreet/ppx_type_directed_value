open Core
open Ppx_type_directed_value_runtime

(* $MDX part-begin=equals_sig *)
type 'a t = 'a -> 'a -> bool

type 'a attribute =
  | CustomCmp of ('a -> 'a -> bool)
  | Ignore

module T = struct
  type nonrec 'a t         = 'a t
  type nonrec 'a attribute = 'a attribute
end

(* $MDX part-end *)

let apply_iso instance _forward backward x y = instance (backward x) (backward y)

(* $MDX part-begin=equals_of_tuple *)
let rec of_tuple : type a len. (a, len) Type_directed.Tuple(T).t -> a T.t =
  fun t ->
  match t with
  | ([ v1; v2 ]                : _ Type_directed.Tuple(T).t) ->
    fun (fst1, (snd1, ())) (fst2, (snd2, ())) -> v1 fst1 fst2 && v2 snd1 snd2
  | (v1 :: (_ :: _ :: _ as tl) : _ Type_directed.Tuple(T).t) ->
    fun (hd1, tl1) (hd2, tl2) -> v1 hd1 hd2 && (of_tuple tl) tl1 tl2
;;

(* $MDX part-end *)

let rec of_record : type a len. (a, len) Type_directed.Record(T).t -> a T.t =
  fun r ->
  let cmp_hd value attrib v1 v2 =
    match attrib with
    | Some (CustomCmp new_cmp) -> new_cmp v1 v2
    | Some Ignore              -> true
    | None                     -> value v1 v2
  in
  match r with
  | ([ { value; attribute; _ } ]               : _ Type_directed.Record(T).t) ->
    fun (v1, ()) (v2, ()) -> cmp_hd value attribute v1 v2
  | ({ value; attribute; _ } :: (_ :: _ as tl) : _ Type_directed.Record(T).t) ->
    let cmp_tl = of_record tl in
    (* Records are equal if first field is equal based on type_directed value
       and rest of the record is equal recursively *)
    fun (hd1, tl1) (hd2, tl2) -> cmp_hd value attribute hd1 hd2 && cmp_tl tl1 tl2
;;

(* Constructs a comparison function for a particular variant constructor *)
let rec cmp_variant_constr
  : type a len. (a, len) Type_directed.Variant_constructor(T).t -> a T.t
  =
  fun cmp ->
  match cmp with
  | ([]       : _ Type_directed.Variant_constructor(T).t) -> fun () () -> true
  | (hd :: tl : _ Type_directed.Variant_constructor(T).t) ->
    (* Specific variant is equal if first element is equal based on type_directed value
       and rest of constructor is equal recursively *)
    fun (fst1, snd1) (fst2, snd2) -> hd fst1 fst2 && (cmp_variant_constr tl) snd1 snd2
;;

let rec of_variant : type a len. (a, len) Type_directed.Variant(T).t -> a T.t =
  fun v ->
  match v with
  | ([]                                      : _ Type_directed.Variant(T).t) -> fun _ _ -> true
  | ({ value = variant; attribute; _ } :: tl : _ Type_directed.Variant(T).t) ->
    fun v1 v2 ->
      (match v1, v2 with
       | First f1, First f2 ->
         let cmp_hd =
           match variant with
           | (Labelled   r  : _ Type_directed.Variant(T).variant) -> of_record r
           | (Unlabelled vc : _ Type_directed.Variant(T).variant) -> cmp_variant_constr vc
         in
         (match attribute with
          | Some (CustomCmp new_cmp) -> new_cmp f1 f2
          | Some Ignore              -> true
          | _                        -> cmp_hd f1 f2)
       | Second s1, Second s2 -> (of_variant tl) s1 s2
       | _        , _         -> false)
;;
