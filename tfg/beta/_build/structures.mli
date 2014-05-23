type fn =
    { args : (string * expression) list
    ; body : expression }
and obj =
    < level : int 
    ; parents : obj list
    ; methods : (string * meth) list >
and meth =
    { level : int
    ; typ   : expression
    ; value : expression option }
and value =
    | Obj    of obj
    | Fn     of fn
    | FnType of value list * value
and expression =
    | New     of expression
    | Method  of expression * string
    | Call    of expression * expression list
    | Id      of string
    | Value   of value

type context = (string, expression) Hashtbl.t
