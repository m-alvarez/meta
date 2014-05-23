open Ast
open Parser

module Context = Set.Make(String)

exception No_such_identifier of string

let rec join s a =
    match a with
    | [] -> ""
    | [a] -> a
    | a::b::r -> a ^ s ^ join s (b::r) 

let rec compile context = function
    | Lit(l)  -> compile_literal context l
    | Name(n) -> 
            if Context.mem n context
            then n
            else raise (No_such_identifier n)
    | New(e) ->
            "instantiate(" ^ compile context e ^ ")"
    | Extend(e1, e2) ->
            "extend(" ^ compile context e1 ^ "," ^ compile context e2 ^ ")"
    | Apply(e1, exprs) ->
            let args = List.map (compile context) exprs in
            let arg_str = join "," args in
            compile context e1 ^ "(" ^ arg_str ^ ")"
    | Method(e, i) ->
            let e = compile context e in
            "invoke(" ^ e ^ ", \"" ^ i ^ "\")"
and compile_literal context = function
    | Obj(methods) -> 
            let methods = List.map (compile_method context) methods in
            "{" ^ join "," methods ^ "}"
    | Fn(args, body) ->
            let new_context = List.fold_right Context.add args context in
            Format.sprintf
            "function( %s ) {
                result = %s;
                return result;
             }" (join "," args) (compile new_context body)
and compile_method context (name, decl) =
    Format.sprintf "%s : %s" name (compile_method_body context decl)
and compile_method_body context decl =
    Format.sprintf "[%d, function() {
        %s
        return %s;
    }]" decl.pot 
        (match decl.self with
        | None -> ""
        | Some(self) -> self ^ "=this;")
        (compile (match decl.self with
                 | None -> context
                 | Some(self) -> Context.add self context) decl.body)
 
let add_prelude s =
    Format.sprintf
    "(function() {
        function invoke(obj, method) {
            var m = obj[method];
            var result;
            if (m[0] == 0) {
                return m[1].apply(obj);
            } else {
                throw Error(\"Method \" + method +
                    \" of object \" + obj + \" has potency \" + m[0]);
            }
        }
        function extend(obj1, obj2) {
            var object = {};
            for(method in obj1) {
                object[method] = obj1[method];
            }
            for(method in obj2) {
                object[method] = obj2[method];
            }

            return object;
        }
        function instantiate(obj) {
            var object = {};
            for(method in obj) {
                var potency = obj[method][0];
                var fn = obj[method][1];
                if(potency > 0) {
                    object[method] = [potency - 1, fn];
                }
            }
            return object;
        }
        return %s
     })()" s

let compile_program expr =
    add_prelude (compile Context.empty expr)

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
        let compiled = compile_program prog in
        Printf.fprintf output "%s" compiled;
    with Parsing.Parse_error ->
        report "Parse error"
        | Failure(s) ->
        report s
        | e ->
        print_string (Printexc.to_string e);
        print_newline ();
        exit 1
        
let () =
    let input = ref stdin in
    let output = ref stdout in
    let open Arg in
    parse
        [ "-in",String (fun s -> input := open_in s),"el nombre del fichero a compilar"
        ; "-out",String (fun s -> output := open_out s),"el nombre del fichero de salida" ]
        (fun arg -> raise (Bad("No se permiten argumentos anonimos")))
        "Compilador experimental de calculo multinivel no tipado a JavaScript";
    try
        compile_program ~input:!input ~output:!output;
        exit 0
    with e ->
        print_string (Printexc.to_string e);
        print_newline ();
        exit 1

