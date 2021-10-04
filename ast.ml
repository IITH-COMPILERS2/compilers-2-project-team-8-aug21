
type operator = Add | Sub | Mul | Div | Mod
type rop = Eq | Neq | Lt | Ltq | Gt | Gtq 
type re = And | Or 
type datatypes = IntType | BoolType | StringType | FloatType


type expr =
    Binop of expr * operator * expr
  | Brela of expr * re * expr
  | Int of int
  | String of string 
  | Bool of bool
  | Float of float
  | Asn of string * expr
  | Equation of string * element list * element list
  | Balance of molecule list * molecule list
  | Concat of expr * expr
  | Print of expr
  | List of expr list 
  | Call of string * expr list
  | Access of expr * string
  | Bracket of expr
  | Null 
  | Noexpr

type statement = 
    Block of statement list
  | Expr of expr
  | Return of expr
  | If of expr * statement * statement
  | Loop of expr * expr * expr * statement
  

type vdecl = {
  vname : string;
  vtype : datatypes;
}
  | set_decl  | matrix_decl | graph_decl


type set_decl = {
  sname : string;
  sets  : set list;
} 

type matrix_decl = {
  mname : string;
  sets  : set list;
}

type graph_decl = {
  gname : string;
  edges: edge list;
}

type par_decl = {
  pname : string;
  ptype : datatypes; 
}

type fdecl = {
  fname : string;
  arguments : par_decl list;
  locals: variable_decl list;
  body : statement list;
}

type program = decls list

type decls = fdecl | vdecl
