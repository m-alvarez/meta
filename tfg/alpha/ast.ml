type id = string
type literal =
    | Obj of (id * method_decl) list
    | Fn  of id list * expression
and method_decl = 
    { self : id option
    ; pot  : int
    ; body : expression }
and expression =
    | Lit of literal
    | Name of id
    | New of expression
    | Extend of expression * expression
    | Apply of expression * expression list
    | Method of expression * id


