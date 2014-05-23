exception No_such_method of string
exception Unbound_variable of string
exception Not_an_object
exception Not_a_function
exception Not_implemented
exception Does_not_typecheck

type arg_decl = (string * value) list
and field = (string * value)
and value =
	| Fun of arg_decl * expression
    | Obj of < level : int; methods : (string * expression) list >
and expression =
    | Id of string
    | Value of value
	| Method of expression * string
	| FunApp of expression * expression list

type context = (string, expression) Hashtbl.t

let rec value_of_literal l = 
	value_of_literal l

let invoke obj meth = match obj with
    | Fun _ -> raise Not_an_object
    | Obj o -> try
        List.assoc meth o#methods
    with Not_found -> raise (No_such_method meth)

let lookup context name =
    try
        Hashtbl.find context name
    with Not_found -> raise (Unbound_variable name)

let rec compatible a b = raise Not_implemented
and type_of context = function
	| Id(s) ->
            type_of context (lookup context s)
    | Value(v) ->
            raise Not_implemented
	| Method(e, s) ->
            type_of context @@ invoke (type_of context e) s
	| FunApp(e, args) -> 
            let arg_types = List.map (type_of context) args in
            match type_of context e with
                | Fun(arg_decl, fun_body) -> 
                        if List.for_all2 compatible (List.map snd arg_decl) arg_types
                        then type_of context fun_body
                        else raise Does_not_typecheck
                | Obj(_) ->
                        raise (Not_a_function)

let typecheck context e =
    try
        let _ = type_of context e in
        true
    with Does_not_typecheck ->
        false
