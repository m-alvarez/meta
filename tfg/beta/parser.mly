%{
    open Ast
%}

%token <string> Id
%token <int> Int
%token EXTEND WITH NEW FUNCTION DELAY IS COLON DEF
%token BEGIN END
%token LBRACE RBRACE LPAR RPAR
%token COMMA SEMICOLON
%token EOF
%nonassoc WITH
%nonassoc NEW
%nonassoc DOT
%nonassoc Id
%token DOT ARROW 
%nonassoc LPAR RPAR
%start pgm
%type <Ast.program> pgm


%%

pgm:
    def_list BEGIN expr END EOF { Program($1, $3) }
;

def:
    Id DEF expr SEMICOLON { Def($1, $3) }
;

def_list: 
    | def def_list { $1 :: $2 }
    | { [] }
;

expr:
    literal                    { Lit($1) }
    | Id                       { Name($1) }
    | NEW expr                 { New($2) }
    | LPAR expr RPAR           { $2 }
    | EXTEND expr WITH expr    { Extend($2, $4) }
    | expr LPAR expr_list RPAR { Apply($1, $3) }
    | expr DOT Id              { Method($1, $3) }
;

expr_list:
    expr COMMA expr_list { $1 :: $3 }
    |                    { [] }
;

literal:
    LBRACE fields RBRACE                             { Obj($2) }
    | FUNCTION LPAR arg_list RPAR LBRACE expr RBRACE { Fn($3, $6) }
    | 
;

arg_list:
    | { [] }
    | Id COMMA arg_list { $1 :: $3 } 
;

field:
    | Id IS decl SEMICOLON { ($1, $3) }
    | DELAY prec field            { (fst $3, { (snd $3) with pot = (snd $3).pot + $2 }) } 
;

decl:
    | expr                    { {self=None; pot=0; body=$1} }
    | LPAR Id RPAR ARROW expr { {self=Some($2); pot=0; body=$5} }
;

prec:
    |     { 1 }
    | Int { $1 }
;

fields:
    |              { [] }
    | field fields { $1 :: $2 }
;


