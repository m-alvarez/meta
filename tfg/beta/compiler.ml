open Reporting
open Structures
open Typechecking
open Parser

let rec join s a =
    match a with
    | [] -> ""
    | [a] -> a
    | a::b::r -> a ^ s ^ join s (b::r) 

let compile (defs, expr) =
    let values = List.fold_left (fun ctx (name, expr) ->
        let _t = type_of Ctx.empty ctx expr in
        Ctx.add name (evaluate Ctx.empty ctx expr) ctx)
        Ctx.empty
        defs
    in
    let _t = type_of Ctx.empty values expr in
    let rec compile = function
        | New(e)             -> Js.New(compile e)
        | Id(i)              -> Js.Name(i)
        | Method(e, i)       -> Js.Method(compile e, i)
        | InhObj (i,s,l)     -> compile_inherit i s l
        | Value(Obj o)       -> Js.Lit (compile_obj o)
        | Value(Primitive p) -> p.compile compile
    and compile_obj o = 
        let methods = List.map (function name, meth -> 
            match meth.value with
            | None    -> 
                raise (Failure "The impossible happened")
            | Some(v) ->
                name, {Js. self = Some o.self; pot = meth.pot; body = compile v })
            (List.filter (function _,meth -> meth.value <> None) o.methods)
        in Js.Obj(methods)
    and compile_inherit level self = function
        | [] ->
            Js.Lit (Js.Obj [])
        | (`Inherit e) :: r -> 
            let e = compile e in
            let r = compile_inherit level self r in
            Js.Extend (e, r)
        | (`Base methods) :: r ->
            let b = compile_obj { self; o_type = ref None; o_level = level; methods } in
            let r = compile_inherit level self r in
            Js.Extend (Js.Lit b, r)
    and add_defs defs expr =
        match defs with
        | [] -> expr
        | (name, body)::rest ->
            add_defs rest
                (Js.Apply(
                    Js.Lit(Js.Fn([name], expr))
                    , [compile body]
                    )
                )
    in add_defs (List.rev defs) (compile expr)

let compile_program ~input ~output =
    let lexbuf = Lexing.from_channel input in
    let report message =
        let open Lexing in
        Format.printf "%s at line %d column %d\n"
            message
            lexbuf.lex_curr_p.pos_lnum
            (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
    in
    try
        let prog = Parser.pgm Lexer.token lexbuf in
        let compiled = Js.compile_program @@ compile prog in  
        Printf.fprintf output "%s" compiled;
    with Parsing.Parse_error ->
        report "Parse error"
        
let () =
    let input = ref stdin in
    let output = ref stdout in
    let open Arg in
    parse
        [ "-in",String (fun s -> input := open_in s),"el nombre del fichero a compilar"
        ; "-out",String (fun s -> output := open_out s),"el nombre del fichero de salida" ]
        (fun arg -> raise (Bad("No se permiten argumentos anonimos")))
        "Compilador experimental de calculo multinivel tipado a JavaScript";
    try
        Printexc.record_backtrace true;
        compile_program ~input:!input ~output:!output;
        exit 0
    with e ->
        print_string (Printexc.to_string e);
        print_newline ();
        Printexc.print_backtrace stdout;
        exit 1

