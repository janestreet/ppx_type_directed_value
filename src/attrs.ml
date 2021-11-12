open Base
open Import
include Attrs_intf

let for_declaration
      (type target)
      (module_ : string)
      (context : target Attribute.Context.t)
  =
  let module M = struct
    type nonrec target = target

    let attr =
      Attribute.declare
        module_
        context
        Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
        (fun x -> x)
    ;;

    let custom =
      Attribute.declare
        (module_ ^ ".custom")
        context
        Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
        (fun x -> x)
    ;;
  end
  in
  (module M : For_declaration with type target = target)
;;

let declare_attrs (module_ : string) =
  let module M = struct
    module For_label_declaration =
      (val for_declaration module_ Attribute.Context.label_declaration)

    module For_constructor_declaration =
      (val for_declaration module_ Attribute.Context.constructor_declaration)

    module For_row_field = (val for_declaration module_ Attribute.Context.rtag)

    module _ = (val for_declaration module_ Attribute.Context.type_declaration)
  end
  in
  (module M : Attrs_declarations)
;;

let generate declared_attr item ~loc : expression =
  let attrs = Attribute.get declared_attr item in
  match attrs with
  | Some exp -> [%expr Some [%e exp]]
  | None -> [%expr None]
;;
