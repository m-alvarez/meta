{
    open Parser
}

let ws = [' ' '\t']

rule token = parse
    | ws+ {token lexbuf}
    | '\n'        {Lexing.new_line lexbuf;
                   token lexbuf}
    | "extend"   { EXTEND }
    | "end"      { END }
    | "with"     { WITH }
    | "new"      { NEW }
    | "function" { FUNCTION }
    | "delay"    { DELAY }
    | "is"       { IS }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "(" { LPAR }
    | ")" { RPAR }
    | ";" { SEMICOLON }
    | "," { COMMA }
    | "->" { ARROW }
    | "."  { DOT }
    | eof  { EOF } 
    | ['a'-'z''A'-'Z''_']+ as lxm { Id(lxm) }
    | ['0'-'9']+ as lxm { Int(int_of_string lxm) }
