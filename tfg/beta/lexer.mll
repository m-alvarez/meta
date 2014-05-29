{
    open Parser
    exception Unexpected_token of char
}

let ws = [' ' '\t']

rule token = parse
    | ws+ {token lexbuf}
    | '\n'        {Lexing.new_line lexbuf;
                   token lexbuf}
    
    | "int"      { INT_TYPE }
    | "bool"     { BOOL_TYPE }

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

    | "+" { ADD }
    | "-" { SUB }
    | "=" { EQ }
    
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }

    | ";" { SEMICOLON }
    | ":" { COLON }
    | ":=" { DEF }
    | "," { COMMA }
    | "."  { DOT }

    | eof  { EOF } 

    | "true" { Bool true }
    | "false" { Bool false }
    | ['0'-'9']+ as lxm { Int(int_of_string lxm) }
    | '-'['0'-'9']+ as lxm { Int(int_of_string lxm) }

    | ['a'-'z''A'-'Z''_']+ as lxm { Id(lxm) }

    | _ as lxm { raise (Unexpected_token lxm) }
