open Ppxlib

module type For_declaration = sig
  type target
  type t := (target, expression) Attribute.t

  val attr   : t
  val custom : t
end

module type Attrs_declarations = sig
  module For_label_declaration : For_declaration with type target = label_declaration

  module For_constructor_declaration :
    For_declaration with type target = constructor_declaration

  module For_row_field : For_declaration with type target = row_field
end

module type Attrs = sig
  module type For_declaration    = For_declaration
  module type Attrs_declarations = Attrs_declarations

  val declare_attrs : string -> (module Attrs_declarations)
  val generate : ('a, expression) Attribute.t -> 'a -> loc:location -> expression
end
