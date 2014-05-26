{
    open Parser
    exception Unexpected_token of char
}

let ws = [' ' '\t']

rule token = parse
    | ws+ {token lexbuf}
    | '\n'        {Lexing.new_line lexbuf;
                   token lexbuf}
    | "begin"    { BEGIN }
    | "end"      { END }
    | "new"      { NEW }
    | "is"       { IS }
    | "object"   { OBJECT }
    | "method"   { METHOD }
    | "abstract" { ABSTRACT }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "(" { LPAR }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
    | ")" { RPAR }
    | ";" { SEMICOLON }
    | ":" { COLON }
    | ":=" { DEF }
    | "," { COMMA }
    | "."  { DOT }
    | eof  { EOF } 
    | ['a'-'z''A'-'Z''_']+ as lxm { Id(lxm) }
    | ['0'-'9']+ as lxm { Int(int_of_string lxm) }
    | _ as lxm { raise (Unexpected_token lxm) }
