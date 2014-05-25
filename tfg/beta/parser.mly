%{
    open Structures
%}

%token <string> Id
%token <int> Int
%token FUNCTION OBJECT
%token NEW IS DEF INHERIT METHOD ABSTRACT
%token BEGIN END
%token LBRACE RBRACE LPAR RPAR LBRACKET RBRACKET
%token COMMA SEMICOLON COLON
%token EOF
%token DOT ARROW 

%left COMMA
%nonassoc COLON
%nonassoc ARROW
%nonassoc DOT
%nonassoc LPAR

%start pgm
%type <Structures.program> pgm


%%

pgm:
    def_list BEGIN expr END EOF { ($1, $3) }
;

def:
    Id COLON expr DEF expr SEMICOLON { ($1, $3, $5) }
;

def_list: 
    | def def_list { $1 :: $2 }
    | { [] }
;

expr:
    | fun_lit                  { Value($1) }
    | obj_lit                  { $1 }
    | Id                       { Id($1) }
    | NEW LPAR expr RPAR       { New($3) }
    | LPAR expr RPAR           { $2 }
    | expr LPAR expr_list RPAR { Call($1, $3) }
    | expr DOT Id              { Method($1, $3) }
;

expr_list:
    expr COMMA expr_list { $1 :: $3 }
    |                    { [] }
;

fun_lit:
    FUNCTION level_spec LPAR arg_list RPAR ARROW expr { 
        Fn({args = $4; body = $7; level = match $2 with None -> 0 | Some(i) -> i}) 
    }
;

num:
    Int { if $1 < 0 then raise (Failure "Number must be positive") else $1 }
;

obj_lit:
    OBJECT level_spec LBRACE
        inherit_section
        fields
    RBRACE {
        let obj =
            { level = (match $2 with | None -> 0 | Some(i) -> i)
            ; methods = List.rev $5 }
        in if $4 = []
        then begin
            Value(Obj(obj))
        end
        else begin
            InhObj(List.rev $4, obj)
        end
    }
;

level_spec:
    | LBRACKET num RBRACKET { Some $2 }
    | { None }
;

inherit_section:
    | {[]} 
    | INHERIT expr SEMICOLON inherit_section { $2 :: $4 }
;

fields:
    | { [] }
    | field SEMICOLON fields { $1 :: $3 }
;

field:
    | METHOD Id LBRACKET num RBRACKET COLON expr IS field_body {
        ($2,
        { level = $4
        ; typ   = $7
        ; value = $9 })
    }
    | METHOD Id COLON expr IS field_body {
        ($2,
        { level = 0
        ; typ   = $4
        ; value = $6 })
    }
;

field_body:
    | ABSTRACT { None }
    | expr     { Some($1) }
;

arg:
    | Id COLON expr { ($1, $3) }
;

arg_list:
    | { [] }
    | arg COMMA arg_list { $1 :: $3 }
;
