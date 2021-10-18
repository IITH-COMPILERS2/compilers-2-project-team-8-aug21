type operator = Add | Sub | Mul | Div | Mod | Equal | Not_equal | Lt | Lte | Gt | Gte | And | Or
type datatype = Int | Bool | String | Float | Void | Matrix
type cmpd_typ = (* uniform for structs and other datatypes *)
    Datatype of datatype
  | Struct of string
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
  | MatrixElem of string * expr * expr
  | MatrixModify of string * (expr * expr) * expr
  | StructAccess of (string * string)
  | StructAsgn of (string * string * expr)
  | StructMatrixAccess of (string * string * (expr * expr))
  | StructMatrixModify of (string * string * (expr * expr) * expr)
  | Null 
  | Noexpr

(* storage class, var type, var name, dimensions, defn *)
type bind = storage_class * cmpd_typ * string * (int * int) * expr

type statement = 
    Block of statement list
  | Expr of expr
  | Break
  | Continue
  | Return of expr
  | If of expr * statement * statement
  | Loop of expr * expr * statement 
  
type fdecl = {
  ret_type : cmpd_typ;
  fname : string;
  args : bind list;
  local_vars: bind list;
  body : statement list;
}

type struct_decl = {
  name : string;
  members : bind list;
}

type program = {
  globals : bind list;
  funcs : fdecl list;
  structs : struct_decl list;
}

(* functions for datatype uniformity in parser *)

let check_non_primitive sc_specifier cmpd_typ variable_name typ_size expr =
  match cmpd_typ with
    Datatype a -> 
    ( 
      match a with
          Matrix -> (sc_specifier, cmpd_typ, variable_name, typ_size, expr)
        | _ -> failwith("Primitive type, only non-primitive types can have a size") 
    )
    | Struct a -> failwith("Cannot have a size")

let check_primitive sc_specifier cmpd_typ variable_name expr =
  match cmpd_typ with
    Datatype a -> 
    ( 
      match a with
        Matrix -> failwith("Non-primitive type: Must have size.")
      | _ -> (sc_specifier, cmpd_typ, variable_name, (-1, -1), expr) 
    )
    | _ -> (sc_specifier, cmpd_typ, variable_name, (-1, -1), expr)
