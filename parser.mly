
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

%token SEMICOLON COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA DOT 
%token PLUS MINUS MULTIPLY DIVIDE MODULO EXPONENT INCR DECR LSHIFT RSHIFT UNION INTERSECT SETDIFF  
%token ASSIGN EQUAL NOT_EQUAL GT GTE LT LTE 
%token IF ELSE LOOP RETURN LINK BREAK CONTINUE CASE DEFAULT CONST STATIC RENAME
%token INT FLOAT STRING BOOL VOID MATRIX GRAPH NUMSET STRSET STRUCT FUNC 
%token AND OR NOT TRUE FALSE NULL
%token <string> ID TAG_BEGIN TAG_END
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

start:		  
  decls EOF                     { $1 }

decls:  /* nothing */               
    {{  globals = []; 
        funcs = []; 
        structs = []; 
    }}
  | decls vdecl    
    {{
      globals = $2 :: $1.globals;
      funcs = $1.funcs;
      structs = $1.structs;
    }}
  | decls fdecl    
    {{
      globals = $1.globals;
      funcs = $2 :: $1.funcs;
      structs = $1.structs;
    }}
  | decls structdecl 
    {{
      globals = $1.globals;
      funcs = $1.funcs;
      structs = List.rev ($2 :: (List.rev ($1.structs)));
    }}

fdecl: FUNC cmpd_typ ID LPAREN args RPAREN LBRACE var_decl_list stmt_list RBRACE 
  {{
    ret_type = $2;  
    fname = $3;
    args = List.rev $5;
    local_vars = List.rev $8;
    body = List.rev $9;
  }}

structdecl:
  STRUCT ID LBRACE var_decl_list RBRACE
  {{
    name = $2;
    members = List.rev $4;
  }}

args: /* nothing */          { []       } 
  | arg                      { [$1]     }
  | args COMMA arg           { $3 :: $1 }

arg:
    sc_specifier cmpd_typ ID             { check_primitive $1 $2 $3 Noexpr        }
  | sc_specifier cmpd_typ ID typ_size    { check_non_primitive $1 $2 $3 $4 Noexpr }

/* ----------------- Parsing Datatypes  ----------------- */
cmpd_typ:
    typ                      { Datatype($1) }
  | STRUCT ID                { Struct($2)   } 

/* keep typ uniform for both primitive and non-primitive */
typ:
    INT                      { Int      }
  | FLOAT                    { Float    }
  | STRING                   { String   }
  | BOOL                     { Bool     }
  | VOID                     { Void     }
  | MATRIX                   { Matrix   }

var_decl_list: /* nothing */ { []       }   
  | var_decl_list vdecl      { $2 :: $1 }

vdecl:
  /* declaration without definition */
    sc_specifier typ ID SEMICOLON                { check_primitive $1 (Datatype($2)) $3 Noexpr    }
  | sc_specifier typ ID typ_size SEMICOLON       { check_non_primitive $1 (Datatype($2)) $3 $4 Noexpr }
  | STRUCT ID ID SEMICOLON                       { check_primitive Normal (Struct($2)) $3 Noexpr  }
  /* declaration with definition */
  | sc_specifier typ ID ASSIGN expr SEMICOLON    { check_primitive $1 (Datatype($2)) $3 $5        }
  | sc_specifier typ ID typ_size ASSIGN expr SEMICOLON 
                                                 { check_non_primitive $1 (Datatype($2)) $3 $4 $6 }

/* TODO: extend for set size, num nodes in graph... */
typ_size:
  LT INTLIT COMMA INTLIT GT                      { ($2, $4) }

sc_specifier: /* nothing */                      { Normal }      
  | CONST                                        { Const  }
  | STATIC                                       { Static }
  | RENAME                                       { Rename }

/* ------------- Statements and Expressions ------------- */
stmt: expr SEMICOLON                             { Expr $1         }
  | BREAK SEMICOLON                              { Break Noexpr    }
  | CONTINUE SEMICOLON                           { Continue Noexpr }
  | RETURN expr SEMICOLON                        { Return $2 }
  | RETURN SEMICOLON                             { Return Noexpr }
  | LBRACE stmt_list RBRACE                      { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt ELSE stmt         { If($3,$5,$7) }
  | IF LPAREN expr RPAREN stmt                   { If($3,$5,Block([]))}
  | LOOP LPAREN expr SEMICOLON expr_opt RPAREN stmt  { Loop($3,$5,$7)}
  | LOOP LPAREN expr RPAREN stmt                 { Loop($3,Null,$5)}

stmt_list: /* nothing */                         { [] }
  | stmt_list stmt                               { $2::$1 }  

expr_opt:
        /* nothing */   { Noexpr }
  | expr                { $1 }

/* TODO: update other operators and unary minus, etc. */
expr:   INTLIT                                   { Intlit($1) }
  | FLOATLIT                                     { Floatlit($1) }
  | TRUE                                         { True  }
  | FALSE                                        { False }
  | NULL                                         { Null  }
  | STRLIT                                       { Strlit($1) }
  | ID                                           { Strlit($1) }
  | LPAREN expr RPAREN                           { $2 }
  | expr EQUAL expr                              { Binop($1, Equal, $3) }
  | expr NOT_EQUAL expr                          { Binop($1, Not_equal, $3) }
  | expr LT expr                                 { Binop($1, Lt, $3)  }
  | expr LTE expr                                { Binop($1, Lte, $3) }
  | expr GT expr                                 { Binop($1, Gt, $3)  }
  | expr GTE expr                                { Binop($1, Gte, $3) }
  | expr AND expr                                { Binop($1, And, $3) }
  | expr OR expr                                 { Binop($1, Or, $3)  }
  | expr PLUS expr                               { Binop($1, Add, $3) }
  | expr MINUS expr                              { Binop($1, Sub, $3) }
  | expr MULTIPLY expr                           { Binop($1, Mul, $3) }
  | expr DIVIDE expr                             { Binop($1, Div, $3) }
  | expr MODULO expr                             { Binop($1, Mod, $3) }
  | NOT expr                                     { Unop(Not,$2) }
  | LBRACK matrix_lit RBRACK                     { MatrixLit($2) }
  | ID LBRACK expr RBRACK                        { MatrixRow($1, $3) }
  | ID LBRACK expr COMMA expr RBRACK             { MatrixElem($1, $3, $5) }  
  | ID LBRACK expr COMMA expr RBRACK ASSIGN expr { MatrixModify($1, ($3,$5), $8) }  
  | ID DOT ID                                    { StructAccess($1, $3) }
  | ID DOT ID ASSIGN expr                        { StructAsgn($1, $3, $5) }
  | ID DOT ID LBRACK expr COMMA expr RBRACK      { StructMatrixAccess($1, $3, ($5, $7)) }
  | ID DOT ID LBRACK expr COMMA expr RBRACK ASSIGN expr { StructMatrixModify($1, $3, ($5, $7), $10) }
  | ID LPAREN args_rev RPAREN                    { FunCall($1, $3) }   
  | ID ASSIGN expr                               { Asgn($1,$3)}


/* function call arguments */
args_rev:
    /* nothing */         { []       }  /* when function has no args */
  | args_list             { List.rev $1 }

args_list:
    expr                  { [$1]     }
  | args_list COMMA expr  { $3 :: $1 }

/* rules for syntax to define a matrix */
matrix_lit:
    matrix_lit_row        { [|$1|]   }
  | matrix_lit_part       { $1       }

matrix_primitive:
    FLOATLIT              { $1       }
  | INTLIT                { float_of_int $1 }
  | MINUS FLOATLIT        { -. $2    }
  | MINUS INTLIT          { float_of_int (-$2) }  

matrix_lit_row:
    matrix_primitive      { [|$1|]   }
  | matrix_lit_row COMMA matrix_primitive { Array.append $1 [|$3|] }

matrix_lit_part:
  | LBRACK matrix_lit_row RBRACK COMMA LBRACK matrix_lit_row RBRACK { [|$2; $6|] }
  | matrix_lit_part COMMA LBRACK matrix_lit_row RBRACK { Array.append $1 [|$4|] }


