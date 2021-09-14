%{ open Ast 
	 let parse_error x = (* Called by parser on error *)
		print_endline x;
		flush stdout
%}

%token SEMICOLON COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA DOT TAG_BEGIN TAG_END
%token PLUS MINUS MULTIPLY DIVIDE MODULO EXPONENT UNEG INCR DECR LSHIFT RSHIFT UNION INTERSECT SETDIFF  
%token ASSIGN MULT_ASSIGN DIV_ASSIGN PLUS_ASSIGN MINUS_ASSIGN EXP_ASSIGN MOD_ASSIGN EQUAL NOT_EQUAL GT GTE LT LTE 
%token IF ELSE LOOP RETURN LINK BREAK CONTINUE CASE DEAULT CONST
%token INT FLOAT STRING BOOL VOID LONG MATRIX GRAPH NUMSET STRSET STRUCT 
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
