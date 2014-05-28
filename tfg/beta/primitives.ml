open Structures
open Reporting

module Tc = Typechecking

let rec bool_type =
    { compile = (fun _ ->
        Js.Lit (Js.Prim "Boolean"))

    ; type_of = (fun types values ->
        Obj({ o_level = 2; methods = [] }))

    ; evaluate = (fun values ->
        Primitive bool_type)

    ; subtype_of = (fun values other ->
        match other with
        | Primitive t when t = bool_type -> true
        | _ -> false)

    ; p_level = (fun values ->
        0)
    }

let bool_lit i =
    { compile = (fun _ ->
        Js.Lit (Js.Bool i))

    ; type_of = (fun types values ->
        Primitive bool_type)

    ; evaluate = (fun values ->
        compiler_error @@ fmt "Trying to evaluate boolean literal")

    ; subtype_of = (fun values other ->
        compiler_error @@ fmt "Using boolean literal as type")

    ; p_level = (fun values ->
        0)
    }

let rec int_type =
    { compile = (fun _ ->
        Js.Lit (Js.Prim "Number"))

    ; type_of = (fun types values ->
        Obj({ o_level = 2; methods = [] }))

    ; evaluate = (fun values ->
        Primitive int_type)

    ; subtype_of = (fun values other ->
        match other with
        | Primitive t when t = int_type -> true
        | _ -> false)

    ; p_level = (fun values ->
        0)
    }

let int_lit i =
    { compile = (fun _ ->
        Js.Lit (Js.Int i))

    ; type_of = (fun types values ->
        Primitive int_type)

    ; evaluate = (fun values ->
        compiler_error @@ fmt "Trying to evaluate integer literal")

    ; subtype_of = (fun values other ->
        compiler_error @@ fmt "Using integer literal as type")

    ; p_level = (fun values ->
        0)
    }

let bin_op b l r =
    { compile = (fun compile ->
        match b with
        | `Add -> Js.Add (compile l, compile r)
        | `Sub -> Js.Sub (compile l, compile r))

    ; type_of = (fun types values ->
        if Tc.type_of types values l = Primitive int_type
           && Tc.type_of types values r = Primitive int_type
        then Primitive int_type
        else type_error "Binary operation between non-integers")

    ; evaluate = (fun _values ->
        compiler_error @@ fmt "Trying to evaluate integer expression")

    ; subtype_of = (fun _values _other ->
        compiler_error @@ fmt "Using integer expression as type")

    ; p_level = (fun _values ->
        0)
    }

let eq l r =
    { compile = (fun compile ->
        Js.Eq (compile l, compile r))

    ; type_of = (fun types values ->
        if Tc.type_of types values l = Primitive int_type
           && Tc.type_of types values r = Primitive int_type
        then Primitive bool_type
        else type_error "Comparing non-integer arguments")

    ; evaluate = (fun _values ->
        compiler_error @@ fmt "Trying to evaluate boolean expression")

    ; subtype_of = (fun _values _other ->
        compiler_error @@ fmt "Using boolean expression as type")

    ; p_level = (fun _values ->
        0)
    }

let if_exp cond thn els =
    { compile = (fun compile ->
        Js.If (compile cond, compile thn, compile els))

    ; type_of = (fun types values ->
        if Tc.type_of types values cond <> Primitive bool_type
        then type_error @@ fmt "Using non-boolean expression as condition for if"
        else begin
            let t_thn = Tc.type_of types values thn in
            let t_els = Tc.type_of types values els in
            if t_thn = t_els
            then (if Tc.value_level values t_thn = 1
                  then t_thn
                  else type_error @@ "Using if to discriminate between non-runtime exprs")
            else type_error @@ "Type mismatch between if branches"
        end)

    ; evaluate = (fun _values ->
        compiler_error @@ fmt "Trying to evaluate if expression")

    ; subtype_of = (fun _values _other ->
        compiler_error @@ fmt "Using if expression as type")

    ; p_level = (fun _values ->
        0)
    }
