
%{ open Ast 
let parse_error s =
      begin
        try
          let start_pos = Parsing.symbol_start_pos ()
          and end_pos = Parsing.symbol_end_pos () in
          Printf.printf "File \"%s\", line %d, characters %d-%d: \n"
            start_pos.pos_fname
            start_pos.pos_lnum
            (start_pos.pos_cnum - start_pos.pos_bol)
            (end_pos.pos_cnum - start_pos.pos_bol)
        with Invalid_argument(_) -> ()
      end;
      Printf.printf "Syntax error: %s\n" s;
      raise Parsing.Parse_error
%}

%token SEMICOLON COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA DOT TAG_BEGIN TAG_END
%token PLUS MINUS MULTIPLY DIVIDE MODULO EXPONENT INCR DECR LSHIFT RSHIFT UNION INTERSECT SETDIFF  
%token ASSIGN MULT_ASSIGN DIV_ASSIGN PLUS_ASSIGN MINUS_ASSIGN EXP_ASSIGN MOD_ASSIGN EQUAL NOT_EQUAL GT GTE LT LTE 
%token IF ELSE LOOP RETURN LINK BREAK CONTINUE CASE DEAULT CONST STATIC RENAME
%token INT FLOAT STRING BOOL VOID MATRIX GRAPH NUMSET STRSET STRUCT FUNC 
%token AND OR NOT TRUE FALSE NULL
%token <string> ID 
%token <string> STRLIT
%token <int> INTLIT
%token <float> FLOATLIT
%token EOF

%nonassoc IF
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQUAL NOT_EQUAL  
%left GT GTE LT LTE
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO
%right NOT
%nonassoc LPAREN RPAREN

%start start
%type <Ast.program> start

%%

start:		/* nothing */							{	[] }
	| start decls  				{($2 :: $1)}

decls:
  | fdecl       { $1 }
  | var_decl      { $1 }

fdecl: FUNC typ ID LPAREN args RPAREN LBRACE var_decl_list stmt_list RBRACE 
  {{
    ret_typ = $2;
    fname = $3;
    args = List.rev $5;
    local_vars = List.rev $8;
    body = List.rev $9;
  }}

args: /* nothing */     { [] } 
      | arg       { $1}
    | args COMMA arg    { $3 :: $1}

arg: sc_specifier typ ID        {{ sc = $1 ;typ = $2; vname = $3 }}
    
var_decl: sc_specifier typ id_list SEMICOLON {{ sc = $1 ;typ = $2; vnames = List.rev $3 }}
  | set_decl      { $1}
  | matrix_decl   { $1}
  | graph_decl    { $1}

var_decl_list:  /* nothing */   { [] }
    | var_decl_list var_decl { $2 :: $1 }


matrix_decl: MATRIX ID ASSIGN LBRACK set_list RBRACK SEMICOLON  
      {{
        typ = Matrix;
        mname = $2;
        sets = $5;
      }}

set_decl:
    NUMSET ID ASSIGN LBRACK set RBRACK SEMICOLON
  {{
    typ = Numset;
    sname = $2;
    set = $5;
  }}

  | STRSET ID ASSIGN LBRACK set RBRACK SEMICOLON
  {{
    typ = Strset;
    sname = $2;
    set = $5;
  }}

graph_decl:
  GRAPH ID ASSIGN LBRACK edge_list RBRACK
  {{
    typ = Graph;
    gname = $2;
    edges = $5;
  }}

set_list:
   set              { [$1] }
  |set_list COMMA set       { $3 :: $1 }

set:
  LBRACK element_list RBRACK    { List.rev $2 }

edge_list:
  edge              { [$1] }
  | edge_list SEMICOLON edge      { $3 :: $1 }

edge:
  element COLON element_list
  {{
    vertex = $1;
    nb = List.rev $3;
  }}

element_list : 
  element           { [$1] }
  | element_list COMMA element    { $3 :: $1 }

element:
  | INTLIT          { Intlit}
  | FLOATLIT        { Floatlit}
  | STRLIT          { Strlit}
    
id_list: ID           { [$1]}
    | ID ASSIGN expr    { [($1,$3)] }
    | id_list COMMA ID      { $3 :: $1}

sc_specifier: /* nothing */  { []}
  | CONST           { Const}
  | STATIC          { Static}
  | RENAME          { Rename}

typ:    INT        { Int           }
    | BOOL       { Bool          }
    | FLOAT      { Float         }
    | VOID       { Void          }
    | STRING     { String        }
    | STRUCT     { Struct        }
    

stmt: expr SEMICOLON        { Expr $1 }
  | BREAK SEMICOLON     { Break   }
  | CONTINUE SEMICOLON      { Continue }
  | RETURN expr SEMICOLON     { Return $2 }
  | RETURN SEMICOLON      { Return }
  | LBRACE stmt_list RBRACE   { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt ELSE stmt      { If($3,$5,$7) }
  | IF LPAREN expr RPAREN stmt        { If($3,$5,Block([]))}
  | LOOP LPAREN expr SEMICOLON expr RPAREN stmt     { Loop($3,$5,$7)}
  | LOOP LPAREN expr RPAREN stmt        { Loop($3,NULL,$5)}

stmt_list: /* nothing */        { [] }
  | stmt_list stmt     { $2::$1 }  

expr:  INTLIT               { Int($1) }
    | FLOATLIT              { Floatlit($1) }
    | TRUE                  { True }
    | FALSE                 { False }
    | ID                    { Strlit($1) }
    | STRLIT                { Strlit($1) }
    | LPAREN expr RPAREN    { $2 }
    | expr EQUAL expr         { Binop($1, Equal, $3) }
    | expr NOT_EQUAL expr       { Binop($1, Not_equal, $3) }
    | expr LT expr          { Binop($1, Lt, $3)}
    | expr LTE expr         { Binop($1, Lte, $3)}
    | expr GT expr          { Binop($1, Gt, $3) }
    | expr GTE expr         { Binop($1, Gte, $3) }
    | expr AND expr         { Binop($1, And, $3) }
    | expr OR expr          { Binop($1, Or, $3) }
    | expr PLUS expr        { Binop($1, Add, $3) }
    | expr MINUS expr       { Binop($1, Sub, $3) }
    | expr MULTIPLY expr      { Binop($1, Mul, $3) }
    | expr DIVIDE expr      { Binop($1, Div, $3) }
    | expr MODULO expr        { Binop($1, Mod, $3) }
    | NOT expr               { Unop(Not,$2)}
    | ID ASSIGN expr        { Asgn($1,$3)}
