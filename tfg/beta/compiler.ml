open Structures
open Typechecking
open Parser

let rec join s a =
    match a with
    | [] -> ""
    | [a] -> a
    | a::b::r -> a ^ s ^ join s (b::r) 

let compile expr =
    let _t = type_of Ctx.empty Ctx.empty expr in
    let rec compile = function
        | New(e)             -> Js.New(compile e)
        | Id(i)              -> Js.Name(i)
        | Call(f, args)      -> Js.Apply(compile f, List.map compile args)
        | Method(e, i)       -> Js.Method(compile e, i)
        | Value(Fn(f))       ->
                let arg_names = List.map fst f.args in
                Js.Lit(Js.Fn(arg_names, compile f.body))
        | Value(Obj(o))      -> Js.Lit(compile_obj o)
        | InhObj([], obj)    -> compile (Value(Obj(obj)))
        | InhObj(p::ps, obj) -> Js.Extend(compile (InhObj(ps, obj)), compile p)
    and compile_obj o = 
        let methods = List.map (function name, meth -> 
            match meth.value with
            | None    -> raise (Failure "The impossible happened")
            | Some(v) ->
                name, {Js. self = Some "self"; pot = meth.level; body = compile v })
            o.methods
        in Js.Obj(methods)
    in compile expr

let compile_toplevel = function
    | (defs, pgm) ->
        let defs = List.rev defs in
        let rec loop defs pgm = match defs with
        | [] -> pgm
        | (name, typ, body) :: defs ->
            loop defs (Call(
                Value(Fn(
                    {args = [(name, typ)]; body = pgm; level = 1}
                ))
                , [body]))
        in
        compile @@ loop defs pgm

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
        let compiled = Js.compile_program @@ compile_toplevel prog in  
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

