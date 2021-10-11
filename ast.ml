type operator = Add | Sub | Mul | Div | Mod | Equal | Not_equal | Lt | Lte | Gt | Gte | And | Or
type datatypes = Int | Bool | String | Float | Void | Struct
type storage_class = Const | Static | Rename
type neg = Not

type element = Int | String

type set = element list

type edge =   { 
        vertex : int;
        nb : element list
  }

type id = ID of string

type expr =
    Binop of expr * operator * expr
  | Int of int
  | Strlit of string 
  | Floatlit of float
  | Unop of neg * expr
  | True
  | False
  | Asgn of id * expr
  | Null 

type statement = 
    Block of statement list
  | Expr of expr
  | Break
  | Continue
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
  mname : string;
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