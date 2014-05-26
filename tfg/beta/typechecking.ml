open Structures

exception No_such_method of string
exception No_such_name of string
exception Not_yet_implemented
exception Compiler_error of string
exception Type_error of string

let fmt = Format.sprintf

let compiler_error str =
    raise (Compiler_error str)
let type_error str =
    raise (Type_error str)

let lookup_name c i =
    try
        Ctx.find i c
    with Not_found ->
        raise (No_such_name i)

let lookup_method obj m =
    try
        List.assoc m obj.methods
    with Not_found ->
        raise (No_such_method m)

let rec type_of types values = function
    | New(exp) -> 
        let exp = evaluate values exp in
        if exp.level = 0
        then compiler_error "Evaluated runtime expression";
        if List.exists (function _,meth ->
            meth.level < 2 && meth.value = None) 
            exp.methods
        then type_error @@ fmt "Abstract method cannot be instantiated";
        { level   = exp.level - 1
        ; methods =
            List.map (function n,m -> n, {m with level = m.level - 1}) 
                (List.filter (function _,m -> m.level > 0) exp.methods)
        }
    | Method(exp,name) ->
        begin
            let t_exp = type_of types values exp in
            let meth  = lookup_method t_exp name in
            if meth.level <> 1
            then raise (Type_error "Invoking higher-level method")
            else match meth.typ with
            | None ->
                compiler_error @@ fmt "Can't infer the type of method %s" name
            | Some(t) -> 
                evaluate values t
        end
    | Id(id) ->
        begin
            try
                Ctx.find id types
            with Not_found ->
                type_of types values @@ Obj (lookup_name values id)
        end
    | Obj(obj) ->
        begin
            let level   = obj.level + 1 in
            let methods = obj.methods   in
            let methods = List.map (function name,meth ->
                match meth.typ, meth.value with
                | None, None ->
                    type_error @@ fmt "Untyped abstract value %s" name
                | Some(t), None ->
                    let t = evaluate values t in
                    if t.level < 1
                    then compiler_error @@ fmt "Runtime object used as type for method %s" name;
                    name, { level = meth.level + 1; typ = Some (Obj t); value = None }
                | None, Some(e) ->
                    let t = type_of types values e in
                    if t.level < 1
                    then compiler_error @@ fmt "Runtime object used as type for method %s" name;
                    name, { level = meth.level + 1; typ = Some (Obj t); value = None }
                | Some(t), Some(e) ->
                    let t' = type_of types values e in
                    let t  = evaluate values t      in
                    if t.level < 1 || t.level <> t'.level
                    then type_error @@ fmt "Level mismatch on method %s" name;
                    if not (subtype_of values t' t)
                    then type_error @@ fmt "Type mismatch on method %s" name;
                    name, { level = meth.level + 1; typ = Some (Obj t); value = None }
            ) methods
            in
            { level; methods } 
        end
    | InhObj(parents, obj) ->
        raise Not_yet_implemented
and evaluate values exp : obj =
    match exp with
    | New(exp) ->
        let exp = evaluate values exp in
        if exp.level < 1
        then compiler_error @@ "Trying to instantiate a runtime object";
        let level = exp.level - 1 in
        let methods = List.map (function name,meth ->
            name, { meth with level = meth.level - 1 }
        ) (List.filter (function name,meth ->
            if meth.value = None
            then type_error @@ fmt "Trying to instantiate object with abstract method %s" name;
            meth.level > 0
        ) exp.methods)
        in
        { level; methods }
    | Method(exp, name) ->
        begin
            let exp = evaluate values exp in
            if exp.level < 1
            then compiler_error @@ "Trying to evaluate a runtime method invocation";
            let meth = lookup_method exp name in
            if meth.level <> 0
            then type_error @@ fmt "Trying to invoke uninstantiated method %s" name;
            match meth.value with
            | None    ->
                type_error @@ fmt "Trying to invoke abstract method %s" name;
            | Some(b) ->
                let b = evaluate values b in
                if b.level < 1
                then compiler_error @@ "Evaluated a runtime expression";
                b
        end
    | Id(id) ->
        lookup_name values id
    | Obj(obj) ->
        obj
    | InhObj(parents, obj) ->
        raise Not_yet_implemented
and subtype_of values t1 t2 =
    t1.level = t2.level
    && List.for_all (function name, meth ->
        try
            begin
                let meth' = lookup_method t1 name in
                if meth'.level <> meth.level
                then type_error @@ fmt "Potency mismatch for method %s" name;
                match meth'.typ, meth.typ with
                | Some(t'), Some(t) ->
                    subtype_of values (evaluate values t') (evaluate values t)
                | _ ->
                    compiler_error @@ fmt "Failed inference for method %s" name
            end
        with Not_found -> 
            type_error @@ fmt "Subclass fails to provide method %s" name
    ) t2.methods

