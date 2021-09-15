Editing
%{ open Ast 
	 let parse_error x = (* Called by parser on error *)
		print_endline x;
		flush stdout
%}

%token SEMICOLON COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA DOT TAG_BEGIN TAG_END
%token PLUS MINUS MULTIPLY DIVIDE MODULO EXPONENT UNEG INCR DECR LSHIFT RSHIFT UNION INTERSECT SETDIFF  
%token ASSIGN MULT_ASSIGN DIV_ASSIGN PLUS_ASSIGN MINUS_ASSIGN EXP_ASSIGN MOD_ASSIGN EQUAL NOT_EQUAL GT GTE LT LTE 
%token IF ELSE LOOP RETURN LINK BREAK CONTINUE CASE DEAULT CONST
%token INT FLOAT STRING BOOL VOID MATRIX GRAPH NUMSET STRSET STRUCT 
%token AND OR NOT TRUE FALSE NULL
// %token <bool> BOOLEAN_LITERAL
%token <string> STRING_L
%token <int> INTEGER_L
%token <float> FLOAT_L
%token EOF



%nonassoc IF
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQUAL NOT_EQUAL  
%left GT GTE LT LTE
%left PLUS MINUS
%left MUL DIV MOD
%nonassoc LPAREN RPAREN

%start start
%type <Ast.program> start

    // grammar rules

%%

start:
	(*nothing*)							{	[] }
	|start fdecl   				{($2 :: $1)}
	
stmt: expr SEMICOLON			{ Expr $1 }
	| BREAK SEMICOLON			{ Break   }
	| CONTINUE SEMICOLON		{ Continue }
	| RETURN expr SEMICOLON			{ Return $2 }
	| RETURN SEMICOLON				{ Return }
	| LBRACE stmt_list RBRACE		{ Block(List.rev $2) }
    | IF LPAREN expr RPAREN stmt ELSE stmt 		{ If($3,$5,$7) }
	| IF LPAREN expr RPAREN stmt 				{ If($3,$5,Block([]))}
	| LOOP LPAREN expr SEMICOLON expr RPAREN stmt 	{ Loop($3,$5,$7)	}
	| LOOP LPAREN expr RPAREN stmt   				{ Loop()}

stmt_list: 						{ [] }
	| stmt stmt_list 			{ $2::$1 } 	

assgn_op: 
		ASSIGN 			{ Assign }
	|	PLUS_ASSIGN		{ Plus_Assign }
	|	MULT_ASSIGN		{ Mult_Assign }
	|	MINUS_ASSIGN    { Minus_Assign }
	|   DIV_ASSIGN		{ Div_Assign  }
	| 	MOD_ASSIGN		{ Mod_Assign }
	|   EXP_ASSIGN 		{ Exp_Assign }

expr: VOIDLIT 					{ }
	| INTLIT                { Int($1) }
    | BOOLLIT             { Boollit($1) }
    | LONGLIT				{ LongLit($1) }
    | FLOATLIT            { Floatlit($1) }
    | STRLIT              { Strlit($1) }
    | MATRIXLIT				{ MatrixLit($1)}
    | GRAPHLIT                  { GraphLit($1) }
    | NUMSETLIT				{ NumsetLit($1) }
    | STRSETLIT 			{ StrsetLit($1) }
    | STRUCTLIT				{ StructLit($1) }
    | LPAREN expr RPAREN  	{ $2 }
    | expr EQUAL expr        { Binop($1, Equal, $3) }
    | expr NOT_EQUAL expr       { Binop($1, Not_equal, $3) }
    | expr LT expr        { Binop($1, Lt, $3)}
    | expr LTE expr       { Binop($1, Lte, $3)}
    | expr GT expr        { Binop($1, Gt, $3) }
    | expr GTE expr       { Binop($1, Gte, $3) }
    | expr AND expr       { Binop($1, And, $3) }
    | expr OR expr        { Binop($1, Or, $3) }
    | expr PLUS expr      { Binop($1, Add, $3) }
    | expr MINUS expr     { Binop($1, Sub, $3) }
    | expr MULTIPLY expr     { Binop($1, Mul, $3) }
    | expr DIVIDE expr    { Binop($1, Div, $3) }
    | expr MODULO expr       { Binop($1, Mod, $3) }
    | UNEG expr 			{ Unop(Uneg, $2) }
    | expr assgn_op expr %prec ASSIGN    
    								{
                                        let f = match $1 with
                                           Rid(rid) -> Binassop(rid, $2, $3)
                                           | _        -> raise
                                           (Parsing.Parse_error)
                                        in f
                                      }
