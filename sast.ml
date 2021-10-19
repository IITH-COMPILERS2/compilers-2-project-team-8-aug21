open Ast

(* s_expr is formed after type and dimensions of sub expressions are found compatible *)
type s_expr = (datatype * (int * int)) * chkd_expr
type chkd_expr = 
    S_Binop of s_expr * operator * s_expr
  | S_Unop of unary_operator * s_expr
  | S_Int of int
  | S_Strlit of string 
  | S_Floatlit of float
  | True
  | False  
  | S_Id of string
  | S_Asgn of string * s_expr
  | S_FunCall of string * s_expr list
  | S_MatrixLit of float array array
  | S_MatrixRow of string * s_expr
  | S_MatrixElem of string * s_expr * s_expr
  | S_MatrixModify of string * (s_expr * s_expr) * s_expr
  | S_StructAccess of (string * string)
  | S_StructAsgn of (string * string * s_expr)
  | S_StructMatrixAccess of (string * string * (s_expr * s_expr))
  | S_StructMatrixModify of (string * string * (s_expr * s_expr) * s_expr)
  | S_Null 
  | S_Noexpr

(* storage class, var type, var name, dimensions, defn *)
type s_bind = storage_class * cmpd_typ * string * (int * int) * s_expr

type s_statement = 
    S_Block of s_statement list
  | S_Expr of s_expr
  | S_Break
  | S_Continue
  | S_Return of s_expr
  | S_If of s_expr * s_statement * s_statement
  | S_Loop of s_expr * s_expr * s_statement 
  
type s_fdecl = {
  s_ret_type : cmpd_typ;
  s_ret_dim : (int * int)    (* dimensions of return value *)
  s_fname : string;
  s_args : s_bind list;
  s_local_vars: s_bind list;
  s_body : s_statement list;
}

type s_struct_decl = {
  s_name : string;
  s_members : s_bind list;
}

type s_program = {
  s_globals : s_bind list;
  s_funcs : s_fdecl list;
  s_structs : s_struct_decl list;
}



