%{
    open Structures
%}

%token <string> Id
%token <int> Int
%token <bool> Bool

%token OBJECT

%token INT_TYPE BOOL_TYPE

%token NEW IS IS INHERIT METHOD 
%token BEGIN END
%token LBRACE RBRACE LPAR RPAR LBRACKET RBRACKET
%token COMMA SEMICOLON COLON

%token ADD SUB EQ
%token IF THEN ELSE

%token EOF
%token DOT 

%left COMMA
%left ADD SUB
%nonassoc EQ

%nonassoc COLON
%nonassoc DOT
%nonassoc LPAR

%nonassoc IF THEN ELSE

%start pgm
%type <Structures.program> pgm


%%

pgm:
    def_list BEGIN expr END EOF { ($1, $3) }
;

def:
    Id IS expr SEMICOLON { ($1, $3) }
;

def_list: 
    | def def_list { $1 :: $2 }
    | { [] }
;

expr:
    | obj_lit                  { $1 }
    | Id                       { Id($1) }
    | NEW LPAR expr RPAR       { New($3) }
    | LPAR expr RPAR           { $2 }
    | expr DOT Id              { Method($1, $3) }
    | primitive                { Value (Primitive $1) }
;

num:
    Int { if $1 < 0 then raise (Failure "Number must be positive") else $1 }
;

primitive:
    | int_val   { $1 }
    | int_type  { $1 }
    | bool_val  { $1 }
    | bool_type { $1 }
    | if_expr   { $1 }
    | eq        { $1 }
    | bin_op    { $1 }
;

if_expr:
    | IF expr THEN expr ELSE expr { Primitives.if_exp $2 $4 $6 }
;

int_val:
    | Int { Primitives.int_lit $1 }
;

int_type:
    | INT_TYPE { Primitives.int_type }
;

bool_val:
    | Bool { Primitives.bool_lit $1 }
;

bool_type:
    | BOOL_TYPE { Primitives.bool_type }
;

eq:
    | expr EQ expr { Primitives.eq $1 $3 }
;

bin_op:
    | expr ADD expr { Primitives.bin_op `Add $1 $3 }
    | expr SUB expr { Primitives.bin_op `Sub $1 $3 }
;

obj_lit:
    OBJECT self_spec level_spec LBRACE
        fields
    RBRACE {
        let level = match $3 with None -> 0 | Some i -> i in
        match $5 with
        | [`Base methods] ->
            Value (Obj 
                { o_type  = ref None
                ; o_level = level
                ; self    = $2
                ; methods })
        | [] ->
            Value (Obj
                { o_type  = ref None
                ; o_level = level
                ; self    = $2
                ; methods = [] })
        | _ ->
            InhObj (level, $2, $5)
    }
;

self_spec:
    | { "self" }
; 

level_spec:
    | LBRACKET num RBRACKET { Some $2 }
    | { None }
;

fields:
    | { [] }
    | field SEMICOLON fields {
        match $3 with
        | (`Inherit _) :: _
        | [] ->
            `Base [$1] :: $3 
        | (`Base l) :: r ->
            `Base ($1::l) :: r
    }
    | INHERIT expr SEMICOLON fields { (`Inherit $2) :: $4 } 
;

field:
    | METHOD Id LBRACKET num RBRACKET opt_type_decl field_body {
        ($2,
        { pot   = $4
        ; typ   = $6
        ; value = $7 })
    }
    | METHOD Id opt_type_decl field_body {
        ($2,
        { pot   = 0
        ; typ   = $3
        ; value = $4 })
    }
;

opt_type_decl:
    |            { None }
    | COLON expr { Some $2 }
;

field_body:
    |         { None }
    | IS expr { Some($2) }
;
