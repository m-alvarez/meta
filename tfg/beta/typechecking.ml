open Structures
open Reporting

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

let as_obj e =
    match e with
    | Obj o -> o
    | Primitive _ -> compiler_error "Expected object, got primitive instead"

let value_level values v =
    match v with
    | Obj o -> o.o_level
    | Primitive p -> p.p_level values

let rec type_of_value types values exp =
    match exp with
    | Obj obj ->
        begin
            let o_level = obj.o_level + 1 in
            let methods = obj.methods     in
            let methods = List.map (function name,meth ->
                if meth.pot < obj.o_level
                then type_error @@ fmt "Method %s will never be instantiated" name;
                match meth.typ, meth.value with
                | None, None ->
                    type_error @@ fmt "Untyped abstract value %s" name
                | Some(t), None ->
                    begin
                        let t = evaluate values t in
                        if value_level values t < 1
                        then compiler_error @@ fmt "Runtime object used as type for method %s" name;
                        if obj.o_level = 0
                        then type_error @@ fmt "Runtime object with abstract method %s" name;
                        name, { pot = meth.pot + 1; typ = Some (Value t); value = None }
                    end
                | None, Some(e) ->
                    let t = as_obj @@ type_of types values e in
                    if t.o_level < 1
                    then compiler_error @@ fmt "Runtime object used as type for method %s" name;
                    name, { pot = meth.pot + 1; typ = Some (Value (Obj t)); value = None }
                | Some(t), Some(e) ->
                    let t' = type_of types values e  in
                    let t  = evaluate values t in
                    let t'_level = value_level values t' in
                    let t_level  = value_level values t  in
                    if t_level < 1 || t_level <> t'_level
                    then type_error @@ fmt "Level mismatch on method %s" name;
                    if not (subtype_of values t' t)
                    then type_error @@ fmt "Type mismatch on method %s" name;
                    name, { pot = meth.pot + 1; typ = Some (Value t); value = None }
            ) methods
            in
            Obj { o_level; methods } 
        end
    | Primitive p ->
        p.type_of types values
and type_of (types : value Ctx.t) (values : value Ctx.t) (exp : expression) : value =
    match exp with
    | New(exp) -> 
        let exp = as_obj @@ evaluate values exp in
        if exp.o_level = 0
        then compiler_error "Evaluated runtime expression";
        if List.exists (function _,meth ->
            meth.pot < 2 && meth.value = None) 
            exp.methods
        then type_error @@ fmt "Abstract method cannot be instantiated";
        Obj { o_level = exp.o_level - 1
            ; methods =
                List.map (function n,m -> n, {m with pot = m.pot - 1}) 
                    (List.filter (function _,m -> m.pot > 0) exp.methods)
            }
    | Method(exp,name) ->
        begin
            let t_exp = as_obj @@ type_of types values exp in
            let meth  = lookup_method t_exp name in
            if meth.pot <> 1
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
                type_of types values @@ Value (lookup_name values id)
        end
    | Value(v) ->
        type_of_value types values v
    | InhObj(parents, obj) ->
        raise Not_yet_implemented
and evaluate values exp : value =
    match exp with
    | New(exp) ->
        let exp = as_obj @@ evaluate values exp in
        if exp.o_level < 1
        then compiler_error @@ "Trying to instantiate a runtime object";
        let o_level = exp.o_level - 1 in
        let methods = List.map (function name,meth ->
            name, { meth with pot = meth.pot - 1 }
        ) (List.filter (function name,meth ->
            if meth.value = None
            then type_error @@ fmt "Trying to instantiate object with abstract method %s" name;
            meth.pot > 0
        ) exp.methods)
        in
        Obj { o_level; methods }
    | Method(exp, name) ->
        begin
            let exp = as_obj @@ evaluate values exp in
            if exp.o_level < 1
            then compiler_error @@ "Trying to evaluate a runtime method invocation";
            let meth = lookup_method exp name in
            if meth.pot <> 0
            then type_error @@ fmt "Trying to invoke uninstantiated method %s" name;
            match meth.value with
            | None    ->
                type_error @@ fmt "Trying to invoke abstract method %s" name;
            | Some(b) ->
                let b = evaluate values b in
                let b_level = value_level values b in
                if b_level < 1
                then compiler_error @@ "Evaluated a runtime expression";
                b
        end
    | Id(id) ->
        lookup_name values id
    | Value (Obj o) ->
        Obj o
    | Value (Primitive p) ->
        p.evaluate values
    | InhObj(parents, obj) ->
        raise Not_yet_implemented
and subtype_of values t1 t2 =
    let t1_level = value_level values t1 in
    let t2_level = value_level values t2 in
    t1_level = t2_level
    && match t1, t2 with
    | Obj t1, Obj t2 ->
        List.for_all (function name, meth ->
            try
                begin
                    let meth' = lookup_method t1 name in
                    if meth'.pot <> meth.pot
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
    | Primitive p1, _ ->
        p1.subtype_of values t2
    | _ ->
        false

