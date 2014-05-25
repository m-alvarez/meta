type fn =
    { args  : (string * expression) list
    ; body  : expression
    ; level : int }
and obj =
    { level : int 
    ; methods : (string * meth) list }
and meth =
    { level : int
    ; typ   : expression
    ; value : expression option }
and value =
    | Obj    of obj
    | Fn     of fn
and expression =
    | New     of expression
    | Method  of expression * string
    | Call    of expression * expression list
    | Id      of string
    | Value   of value
    | InhObj  of expression list * obj

type definition = string * expression * expression
type program = definition list * expression

module Ctx = Map.Make(String)
type context = (string, expression) Hashtbl.t
