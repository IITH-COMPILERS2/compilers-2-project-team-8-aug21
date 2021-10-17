type operator = Add | Sub | Mul | Div | Mod | Equal | Not_equal | Lt | Lte | Gt | Gte | And | Or
type datatype = Int | Bool | String | Float | Void | Matrix
type storage_class = Const | Static | Rename | Noexpr | Normal
type unary_operator = Not  

type expr =
    Binop of expr * operator * expr
  | Unop of unary_operator * expr
  | Int of int
  | Strlit of string 
  | Floatlit of float
  | True
  | False  
  | Id of string
  | Asgn of string * expr
  | FunCall of string * expr list
  | MatrixLit of float array array
  | MatrixRow of string * expr
  | MatrixPart of string * expr * expr
  | MatrixModify of string * (expr * expr) * expr
  | Null 
  | Noexpr

(* storage class, var type, var name, dimensions, defn *)
type bind = storage_class * datatype * string * (int * int) * expr

type statement = 
    Block of statement list
  | Expr of expr
  | Break
  | Continue
  | Return of expr
  | If of expr * statement * statement
  | Loop of expr * expr * statement 
  
type fdecl = {
  ret_type : datatype;
  fname : string;
  args : bind list;
  local_vars: bind list;
  body : statement list;
}

type program = bind list * fdecl list

(* functions for datatype uniformity in parser *)

let check_non_primitive sc_specifier data_type variable_name typ_size expr =
  match data_type with
    Matrix -> (sc_specifier, data_type, variable_name, typ_size, expr)
    | _ -> failwith("Primitive type, only non-primitive types can have a size")

let check_primitive sc_specifier data_type variable_name expr =
  match data_type with
    Matrix -> failwith("Non-primitive type: Must assign size.")
    | _ -> (sc_specifier, data_type, variable_name, (-1, -1), expr)
