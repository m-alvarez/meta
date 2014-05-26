type obj =
    { level : int 
    ; methods : (string * meth) list }
and meth =
    { level : int
    ; typ   : expression option
    ; value : expression option }
and expression =
    | New     of expression
    | Method  of expression * string
    | Id      of string
    | Obj     of obj
    | InhObj  of expression list * obj

type definition = string * expression
type program = definition list * expression

module Ctx = Map.Make(String)
type context = (string, expression) Hashtbl.t
