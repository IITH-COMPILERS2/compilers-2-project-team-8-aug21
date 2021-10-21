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





(*priting functions*)

let print_oper = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Not_equal -> "!="
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  | And -> "AND"
  | Or -> "OR" 
  | Mod -> "%"


let print_uoper = function
  (* Neg -> "-" *)
  | Not -> "!"

let print_storage_class = function
    | Const -> "const"
    | Static -> "static"
    | Rename -> "rename"
    | Noexpr -> " "
    | Normal -> ""  

let rec print_expr_string = function
  Int(l) -> string_of_int l
  | Floatlit(l) -> string_of_float l
  | True -> "true"
  | False -> "false"
  | Strlit(l) -> "\"" ^ (String.escaped l) ^ "\""
  | MatrixLit(_) -> "matLit"
  | Id(s) -> s
  | Binop(e1, o, e2) ->print_expr_string e1 ^ " " ^ print_oper o ^ " " ^ print_expr_string e2
  | Unop(o, e) -> print_uoper o ^ print_expr_string e
  | Asgn(v, e) -> v ^ " = " ^ print_expr_string e
  | FunCall(f, el) -> f ^ "(" ^ String.concat ", " (List.map print_expr_string el) ^ ")"
  | MatrixRow(s,e) -> s ^ ".row"
  | MatrixElem(s, r, c) -> s ^ "[" ^ print_expr_string r ^ "]" ^ "[" ^ print_expr_string c ^ "]"
  | MatrixModify(s, (e1,e2), e3) -> s ^ "[" ^ print_expr_string(e1) ^ ", " ^ print_expr_string(e2) ^ "]" ^ " = " ^ print_expr_string(e3)
  | StructAccess((s1,s2)) -> s1 ^ s2
  | StructAsgn((s1,s2,e)) -> s1 ^ "." ^ s2 ^ " = " ^ print_expr_string(e) 
  | StructMatrixAccess(s1,s2,(e1,e2)) -> s1 ^ s2 ^ "[" ^ print_expr_string(e1) ^ ", " ^ print_expr_string(e2) ^ "]"
  | StructMatrixModify((s1,s2,(e1,e2),e3)) -> s1 ^ s2 ^ "[" ^ print_expr_string(e1) ^ ", " ^ print_expr_string(e2) ^ "]" ^ " = " ^ print_expr_string(e3)
  | Noexpr -> ""
  | Null -> ""

let print_typ_str = function
  Void -> "void"
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Matrix -> "matrix"


let print_cmpdtyp_info = function
    Datatype(d) -> print_typ_str d
  | Struct(s) -> "struct" ^ s


let rec print_stmt_string = function
  Block(stmts) -> "{\n" ^ String.concat "" (List.map print_stmt_string stmts) ^ "}\n"
  | Expr(expr) -> print_expr_string expr ^ ";\n";
  | Return(expr) -> "return " ^ print_expr_string expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ print_expr_string e ^ ")\n" ^ print_stmt_string s
  | If(e, s1, s2) ->  "if (" ^ print_expr_string e ^ ")\n" ^  print_stmt_string s1 ^ "else\n" ^ print_stmt_string s2
  | Loop(e1, e2, s) -> "loop (" ^ print_expr_string e1  ^ " ; " ^ print_expr_string e2 ^ " ; " ^ ") " ^ print_stmt_string s
  | Break -> "break"
  | Continue -> "continue"
  

let string_of_vdecl (t, id) = print_cmpdtyp_info t ^ " " ^ id ^ ";\n"
let string_of_vdecl (s,t, id,_,expr) = print_storage_class s ^ print_cmpdtyp_info t ^ " " ^ id ^ " " ^ print_expr_string expr ^ ";\n"

  let string_of_fdecl fdecl =
    let thirdoffive = fun (_,_,y,_, _) -> y in
    print_cmpdtyp_info fdecl.ret_type ^ " " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map thirdoffive fdecl.args) ^
    ")\n{\n" ^
    String.concat "" (List.map string_of_vdecl fdecl.local_vars) ^
    String.concat "" (List.map print_stmt_string fdecl.body) ^
    "}\n"
  
  let string_of_struct_decl s =
    let vdecls = String.concat "" (List.map string_of_vdecl s.members) in
    "struct " ^ s.name ^ "{\n" ^
      vdecls ^
    "}\n"
  
    let print_program_string program =
      let vars = program.globals
      and funcs = program.funcs
      and structs = program.structs in
      String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
      String.concat "" (List.map string_of_struct_decl structs) ^ "\n" ^
      String.concat "\n" (List.map string_of_fdecl funcs)  
