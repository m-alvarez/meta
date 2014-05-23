type token =
  | Id of (string)
  | Int of (int)
  | EXTEND
  | WITH
  | NEW
  | FUNCTION
  | DELAY
  | IS
  | COLON
  | DEF
  | BEGIN
  | END
  | LBRACE
  | RBRACE
  | LPAR
  | RPAR
  | COMMA
  | SEMICOLON
  | EOF
  | DOT
  | ARROW

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 29 "parser.ml"
let yytransl_const = [|
  259 (* EXTEND *);
  260 (* WITH *);
  261 (* NEW *);
  262 (* FUNCTION *);
  263 (* DELAY *);
  264 (* IS *);
  265 (* COLON *);
  266 (* DEF *);
  267 (* BEGIN *);
  268 (* END *);
  269 (* LBRACE *);
  270 (* RBRACE *);
  271 (* LPAR *);
  272 (* RPAR *);
  273 (* COMMA *);
  274 (* SEMICOLON *);
    0 (* EOF *);
  275 (* DOT *);
  276 (* ARROW *);
    0|]

let yytransl_block = [|
  257 (* Id *);
  258 (* Int *);
    0|]

let yylhs = "\255\255\
\001\000\004\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\006\000\006\000\005\000\005\000\008\000\
\008\000\009\000\009\000\010\000\010\000\011\000\011\000\007\000\
\007\000\000\000"

let yylen = "\002\000\
\005\000\004\000\002\000\000\000\001\000\001\000\002\000\003\000\
\004\000\004\000\003\000\003\000\000\000\003\000\007\000\000\000\
\003\000\004\000\003\000\001\000\005\000\000\000\001\000\000\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\000\000\000\000\
\003\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\000\000\014\000\025\000\008\000\000\000\
\000\000\011\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\000\000\010\000\017\000\000\000\000\000\018\000\
\012\000\000\000\000\000\015\000\000\000\000\000"

let yydgoto = "\002\000\
\004\000\005\000\026\000\006\000\017\000\041\000\024\000\033\000\
\025\000\049\000\036\000"

let yysindex = "\014\000\
\017\255\000\000\013\255\000\000\023\255\017\255\006\255\006\255\
\000\000\000\000\006\255\006\255\010\255\007\255\006\255\014\255\
\000\000\247\254\254\254\056\255\034\255\028\255\061\255\029\255\
\007\255\054\255\006\255\000\000\055\255\061\000\006\255\060\255\
\063\255\025\255\000\000\007\255\000\000\000\000\000\000\031\255\
\064\255\000\000\000\000\056\255\034\255\068\255\036\255\056\255\
\058\255\000\000\006\255\000\000\000\000\006\255\066\255\000\000\
\000\000\053\255\065\255\000\000\006\255\056\255"

let yyrindex = "\000\000\
\072\255\000\000\000\000\000\000\000\000\072\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\070\255\000\000\000\000\
\000\000\000\000\000\000\041\255\071\255\000\000\015\255\000\000\
\070\255\000\000\073\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\048\255\071\255\000\000\000\000\074\255\
\000\000\000\000\073\255\000\000\000\000\000\000\059\255\000\000\
\000\000\000\000\000\000\000\000\000\000\075\255"

let yygindex = "\000\000\
\000\000\080\000\249\255\000\000\000\000\037\000\065\000\046\000\
\058\000\000\000\000\000"

let yytablesize = 94
let yytable = "\016\000\
\018\000\031\000\030\000\019\000\020\000\027\000\010\000\022\000\
\011\000\029\000\012\000\013\000\027\000\023\000\001\000\022\000\
\029\000\003\000\014\000\040\000\015\000\022\000\007\000\044\000\
\021\000\010\000\048\000\011\000\027\000\012\000\013\000\028\000\
\029\000\008\000\032\000\034\000\055\000\014\000\011\000\047\000\
\012\000\013\000\037\000\040\000\007\000\027\000\058\000\051\000\
\014\000\029\000\015\000\009\000\007\000\062\000\007\000\042\000\
\007\000\007\000\007\000\009\000\043\000\009\000\035\000\009\000\
\009\000\009\000\060\000\027\000\027\000\039\000\027\000\029\000\
\029\000\006\000\029\000\056\000\045\000\006\000\046\000\052\000\
\054\000\059\000\004\000\024\000\061\000\009\000\016\000\057\000\
\013\000\038\000\053\000\020\000\021\000\050\000"

let yycheck = "\007\000\
\008\000\004\001\012\001\011\000\012\000\015\001\001\001\001\001\
\003\001\019\001\005\001\006\001\015\001\007\001\001\000\001\001\
\019\001\001\001\013\001\027\000\015\001\007\001\010\001\031\000\
\015\001\001\001\034\000\003\001\015\001\005\001\006\001\018\001\
\019\001\011\001\001\001\008\001\001\001\013\001\003\001\015\001\
\005\001\006\001\014\001\051\000\004\001\015\001\054\000\017\001\
\013\001\019\001\015\001\004\001\012\001\061\000\014\001\001\001\
\016\001\017\001\018\001\012\001\000\000\014\001\002\001\016\001\
\017\001\018\001\014\001\015\001\015\001\016\001\015\001\019\001\
\019\001\015\001\019\001\018\001\017\001\019\001\016\001\016\001\
\013\001\016\001\011\001\014\001\020\001\006\000\016\001\051\000\
\016\001\025\000\045\000\018\001\018\001\036\000"

let yynames_const = "\
  EXTEND\000\
  WITH\000\
  NEW\000\
  FUNCTION\000\
  DELAY\000\
  IS\000\
  COLON\000\
  DEF\000\
  BEGIN\000\
  END\000\
  LBRACE\000\
  RBRACE\000\
  LPAR\000\
  RPAR\000\
  COMMA\000\
  SEMICOLON\000\
  EOF\000\
  DOT\000\
  ARROW\000\
  "

let yynames_block = "\
  Id\000\
  Int\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'def_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
                                ( Program(_1, _3) )
# 171 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 29 "parser.mly"
                          ( Def(_1, _3) )
# 179 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'def) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'def_list) in
    Obj.repr(
# 33 "parser.mly"
                   ( _1 :: _2 )
# 187 "parser.ml"
               : 'def_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
      ( [] )
# 193 "parser.ml"
               : 'def_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 38 "parser.mly"
                               ( Lit(_1) )
# 200 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
                               ( Name(_1) )
# 207 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
                               ( New(_2) )
# 214 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                               ( _2 )
# 221 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                               ( Extend(_2, _4) )
# 229 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 43 "parser.mly"
                               ( Apply(_1, _3) )
# 237 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
                               ( Method(_1, _3) )
# 245 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 48 "parser.mly"
                         ( _1 :: _3 )
# 253 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                         ( [] )
# 259 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fields) in
    Obj.repr(
# 53 "parser.mly"
                                                     ( Obj(_2) )
# 266 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'arg_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                                                     ( Fn(_3, _6) )
# 274 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
      ( [] )
# 280 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arg_list) in
    Obj.repr(
# 59 "parser.mly"
                        ( _1 :: _3 )
# 288 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    Obj.repr(
# 63 "parser.mly"
                           ( (_1, _3) )
# 296 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'prec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 64 "parser.mly"
                                  ( (fst _3, { (snd _3) with pot = (snd _3).pot + _2 }) )
# 304 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                              ( {self=None; pot=0; body=_1} )
# 311 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                              ( {self=Some(_2); pot=0; body=_5} )
# 319 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
          ( 1 )
# 325 "parser.ml"
               : 'prec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parser.mly"
          ( _1 )
# 332 "parser.ml"
               : 'prec))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
                   ( [] )
# 338 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'field) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 79 "parser.mly"
                   ( _1 :: _2 )
# 346 "parser.ml"
               : 'fields))
(* Entry pgm *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let pgm (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
