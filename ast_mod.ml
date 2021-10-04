
type operator = Add | Sub | Mul | Div | Mod
type rop = Eq | Neq | Lt | Ltq | Gt | Gtq 
type re = And | Or 
type datatypes = IntType | BoolType | StringType | FloatType
type storage_class = Const | Static | Rename

type element = IntType | StringType

type set = element list
type edge =   {
        vertex : int;
        nb : element list
  }

type expr =
    Binop of expr * operator * expr
  | Brela of expr * re * expr
  | Int of int
  | String of string 
  | Bool of bool
  | Float of float
  | Asn of string * expr
  | Null 

type statement = 
    Block of statement list
  | Expr of expr
  | Return of expr
  | If of expr * statement * statement
  | Loop of expr * expr * expr * statement
  

type variable_decl = {
    sc : storage_class;
    typ : datatypes ; 
    vnames : string list; 
}


type set_decl = {
  typ   : string;
  sname : string;
  sets  : set list;
} 

type matrix_decl = {
  typ   : string;
  mname : variable_decl;
  sets  : set list;
}

type graph_decl = {
  typ   : string;
  gname : string;
  edges: edge list;
}

type par_decl = {
  sc : storage_class;
  typ : datatypes ; 
  vname : string; 
}

type fdecl = {
  ret_typ : datatypes;
  fname : string;
  args : par_decl list;
  local_vars: variable_decl list;
  body : statement list;
}

type decls = fdecl

type program = decls list
