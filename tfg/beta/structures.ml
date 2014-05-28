module Ctx = Map.Make(String)
type obj =
    { o_level : int 
    ; methods : (string * meth) list }
and value =
    | Obj       of obj
    | Primitive of primitive
and meth =
    { pot   : int
    ; typ   : expression option
    ; value : expression option }
and expression =
    | New       of expression
    | Method    of expression * string
    | Id        of string
    | InhObj    of expression list * obj
    | Value     of value
and primitive =
    { compile    : (expression -> Js.expression) ->
                       Js.expression
    ; type_of    : value Ctx.t ->
                   value Ctx.t ->
                       value
    ; evaluate   : value Ctx.t ->
                       value
    ; p_level    : value Ctx.t ->
                       int
    ; subtype_of : value Ctx.t ->
                   value ->
                       bool
    }

type definition = string * expression
type program = definition list * expression

