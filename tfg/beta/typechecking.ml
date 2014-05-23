open Structures

let nil n =
   Value(Obj(object 
        method level = n; 
        method parents = []; 
        method methods = []
    end))

exception No_such_method of string
exception No_such_name of string
exception Not_yet_implemented
exception Message of string
exception Hell

let assoc a l e =
    try
        List.assoc a l
    with Not_found -> raise e

let lookup_name c i =
    assoc i c (No_such_name(i))
let lookup_method obj m =
    assoc m obj (No_such_method(m))

let decrease_level l =
    if l < 1
    then raise Hell
    else l - 1

let instantiate_method = function
    { level
    ; typ
    ; value } -> raise Not_yet_implemented

let instantiate obj : obj =
    if obj#parents <> []
    then raise Hell
    else begin
        let methods = ref [] in
        !methods
        |> List.iter (fun (name, body) ->
            match instantiate_method body with
            | None    -> ()
            | Some(m) -> methods := (name, m) :: !methods);
        object
            method level = decrease_level obj#level
            method parents = []
            method methods = !methods
        end
    end
            
(* NOTE This doesn't typecheck the expression either *)
(* Actually maybe *)
let rec evaluate ctx e = 
    if level ctx e < 1
    then raise (Message "Trying to evaluate runtime expression")
    else match e with
        | New(tmpl) ->
                begin
                    match evaluate ctx tmpl with
                    | FnType(_)
                    | Fn(_) -> raise Hell
                    | Obj(o) -> Obj(instantiate o)
                end
        | Id(s) ->
                evaluate ctx @@ lookup_name ctx s
        | Value(v) ->
                v
        | Method(o, m) ->
                begin
                    match evaluate ctx o with
                    | Obj(o) ->
                            begin
                                match lookup_method o#methods m with
                                | { level
                                ; typ
                                (* NOTE there's more than one way to do this *)
                                ; value = Some(value) } when level = 0 ->
                                    value
                                | _ -> raise Hell
                            end
                    | _ -> raise Hell
                end
        | Call(f, args) ->
                raise Hell

(* NOTE THIS DOES NOT TYPECHECK THE EXPRESSION *)
let rec level ctx expression =
    match expression with
    | New(e) ->
            decrease_level @@ level ctx e
    | Method(e,m) ->
            begin
                match type_of ctx e with
                | Fn(_) 
                | FnType(_) -> raise Hell
                | Obj(o)    ->
                        (lookup_method o#methods m).level
            end
    | Call(f,_) ->
            begin
                match type_of ctx f with
                | Obj(_)    
                | Fn(_)       -> raise Hell
                | FnType(_,t) -> decrease_level @@ level ctx (Value(t))
            end
    | Id(s) ->
            level ctx @@ lookup_name ctx s
    | Value(v) ->
            match v with
            | Obj(o)      -> o#level
            | Fn(f)       -> level ctx f.body
            | FnType(_,v) -> level ctx (Value(v))
and type_of ctx exp =
    match exp with
    | New(e) ->
            evaluate ctx e
