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

val pgm :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
