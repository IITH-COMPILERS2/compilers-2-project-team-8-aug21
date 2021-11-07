(* Semantic checker for Tureasy *)

open Ast
(* open Sast *)

module StringMap = Map.Make(String)   (* Symbol table *)

(* First we declared some helper functions which will be used in semantic checking*)

let check_for_void errormsg = function
    (_, Datatype(typ), name, _, _) when typ = Void -> raise (Failure (errormsg name))
  | _ -> ()

let check_duplicate errormsg lst =
  let rec help_check = function
      elem1 :: elem2 :: _ when elem1 = elem2 -> raise (Failure (errormsg elem2))
    | _ :: rem -> help_check rem
    | [] -> ()
  in help_check (List.sort compare lst)  

(* redundant, merge later *)
let thirdoffive = function 
  (_,_,y,_, _) -> y
  
let secondoffive = function 
  (_,y,_,_, _) -> y

let rec contains z = function
    [] -> false
  | head :: tail -> if z = head then true else contains z tail

let type_match types = function Datatype(t) -> contains t (Array.to_list types)
      | _ -> false

let get_struct_name = function
struct_typ -> struct_typ.name

let check_for_empty_struct errormsg = function
  { name = n; members = []; } -> raise (Failure (errormsg n))
  | _ -> ()

let check_duplicate_fields errormsg = function
   { name = n; members = memberlst; } 
   -> check_duplicate (fun field -> "Found duplicate field '" ^ field ^ "' in struct declaration") (List.map thirdoffive memberlst)

let rec check_assign_stmt lval rval =
  match (lval, rval) with
      (Datatype(p1),Datatype(p2)) -> if (p1 = p2 || (p1 = Int && p2 = Float) || (p1 = Float && p2 = Int)) then true else false
    | (Struct(s1), Struct(s2)) -> if s1 = s2 then true else
        (print_endline (s1 ^ s2); false)
    | _ -> false

let check_assign lval rval ex = 
    if check_assign_stmt lval rval then lval
    else raise (Failure ("Illegal assignment of " ^ print_cmpdtyp_info lval ^
                  " = " ^ print_cmpdtyp_info rval ^ " in " ^
                  print_expr_string ex))
                  
 let get_struct_member_type struct_decl member errormsg =
  try
    let member_bind = List.find (fun (_,_,n,_, _) -> n = member) struct_decl.members
    in secondoffive member_bind  (* return the typ *)
  with Not_found -> raise (Failure errormsg)



let semantic_check program = 
  
  let global_vars = program.globals
  and funcs = program.funcs
  and structs = program.structs in
  
  (* CHECK FOR GLOBAL VARIABLES *)
  (* check for globals defined as void type *)
  List.iter (check_for_void (fun name -> "Found illegal void global '" ^ name ^ "'")) global_vars;
  (* check for duplicate variable names *)
  check_duplicate (fun name -> "Found duplicate global '" ^ name ^ "'") (List.map thirdoffive global_vars);


  (* CHECK STRUCTS *)
  (* check for struct without any fields *)
  List.iter (check_for_empty_struct (fun name -> "Found struct without fields '" ^ name ^ "'")) structs;
  (* check for duplicate struct names *)
  check_duplicate (fun name -> "Found duplicate struct '" ^ name ^ "'") (List.map get_struct_name structs);
  (* check for duplicate field names *)
  List.iter (check_duplicate_fields (fun fieldmessage -> fieldmessage)) structs;

 (* above has been tested, and is working *)

  (* CHECK FOR FUNCTIONS *)
  let keywords_builtin = Array.to_list
    [|
      "print";      
      (* ***************more keywords to add*************** *)
    |]
  in
  (* if a builtin function's name has been used by user for some other definition*)
  List.iter (fun fname ->
    if List.mem fname (List.map (fun fd -> fd.fname) funcs)
    then raise (Failure ("Function " ^ fname ^ " cannot be defined, it is built-in"))
  ) keywords_builtin;
  (*check for duplicate function names as we don't support function overloading*)
  check_duplicate (fun name -> "Duplicate function " ^ name )
    (List.map (fun fd -> fd.fname) funcs);


  let built_in_decls = StringMap.empty in
  (* Add all function names to symbol table *)
  let func_decls = List.fold_left (fun map fd -> StringMap.add fd.fname fd map)
                        built_in_decls funcs 
  in
  (* check whether a function exists in symbol table *)
  let func_decl f = try StringMap.find f func_decls
      with Not_found -> raise (Failure ("Unknown function " ^ f ^ " call"))
  in
    
  (* main has to be declared in every program *)
  let _ = func_decl "main" in

  (* semantic check  inside each function *)
  let semantic_check_func func = 

    List.iter (check_for_void (fun arg -> "Illegal void arguments " ^ arg ^ 
        " in " ^ func.fname)) func.args;

    check_duplicate (fun arg -> "Duplicate arguments " ^ arg ^ " in " ^ func.fname)
      (List.map thirdoffive func.args);

    List.iter (check_for_void (fun local -> "Illegal void locals " ^ local ^
        " in" ^ func.fname)) func.local_vars;

    check_duplicate (fun local -> "Duplicate locals " ^ local ^ " in " ^ func.fname)
      (List.map thirdoffive func.local_vars);

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun map (sc,t,n,(r,c),_) -> StringMap.add n t map)
      StringMap.empty ( global_vars @ func.args @ func.local_vars )
    in

    (* Return a variable from our local symbol table if exists or throw error*)
    let identifier_type id = 
      try StringMap.find id symbols
      with Not_found -> raise (Failure ("Identifier " ^ id ^ " is undeclared"))
    in

    let struct_decl = List.fold_left (fun map sd -> StringMap.add sd.name sd map)
                        StringMap.empty structs 
    in

    let get_struct_decl id =
      match identifier_type id with
      Struct name -> (
          try StringMap.find name struct_decl
        with Not_found -> raise (Failure ("Identifier " ^ id ^ " is undeclared"))
      )
      | _ -> raise (Failure (id ^ "is not a struct " ))
    in

    (* semantic check of expressions inside functions*)
    let rec expr: expr-> cmpd_typ = function
        Intlit _ -> Datatype(Int)
      | Floatlit _ -> Datatype(Float)
      | True -> Datatype(Bool)
      | False -> Datatype(Bool)
      | Id id -> identifier_type id 
      | Strlit _ -> Datatype(String)
      | Noexpr -> Datatype(Void)
      | Null -> Datatype(Void)
      | Asgn (var,e) as cmpd -> 
        let left = identifier_type var
        and right = expr e in
        check_assign left right cmpd
      | MatrixLit m as m_expr -> let row_size = Array.length(m.(0)) in
                          let check_length l = 
                            if Array.length(l) != row_size then 
                              raise (Failure ("All rows must have same number of elements in matrix literal: " ^ print_expr_string m_expr)) 
                          in
                          Array.iter check_length m; 
                          Datatype(Matrix)
      | MatrixElem(m_id,ridx,cidx) as m_elem -> if (expr ridx) <> Datatype(Int) then 
                                                  raise (Failure ("Index of matrix expected to be an integer, but found '" ^ print_expr_string ridx 
                                                                   ^ "' which has type  " ^ print_cmpdtyp_info (expr ridx)))
                                                else if (expr cidx) <> Datatype(Int) then 
                                                  raise (Failure ("Index of matrix expected to be an integer, but found '" ^ print_expr_string ridx 
                                                  ^ "' which has type  " ^ print_cmpdtyp_info (expr ridx)))
                                                else if (identifier_type m_id) <> Datatype(Matrix) then
                                                  raise (Failure ("Expected matrix type, but found '" ^ m_id ^ "' which is declared as type " 
                                                                  ^ print_cmpdtyp_info (identifier_type m_id)));
                                                Datatype(Float)
      | MatrixModify(m_id,(ridx,cidx),a_ex) as m_ex ->  if (expr ridx) <> Datatype(Int) then 
                                                          raise (Failure ("Index of matrix expected to be an integer, but found '" ^ print_expr_string ridx 
                                                                          ^ "' which has type  " ^ print_cmpdtyp_info (expr ridx)))
                                                        else if (expr cidx) <> Datatype(Int) then 
                                                          raise (Failure ("Index of matrix expected to be an integer, but found '" ^ print_expr_string ridx 
                                                          ^ "' which has type  " ^ print_cmpdtyp_info (expr ridx)))
                                                        else if (identifier_type m_id) <> Datatype(Matrix) then
                                                          raise (Failure ("Expected matrix type, but found '" ^ m_id ^ "' which is declared as type " 
                                                                          ^ print_cmpdtyp_info (identifier_type m_id)));
                                                        check_assign (Datatype(Float)) (expr a_ex) m_ex
                                                        
      | StructAccess (name, member) -> ignore(identifier_type name); (*check it's declared *)
          let s_decl = get_struct_decl name in (* get the ast struct_decl type *)
          get_struct_member_type s_decl member
          ("Illegal struct member access: " ^ name  ^ "." ^ member)

      | StructAsgn (name, member, e) as ex ->  (* TODO: add illegal assign test *)
          let t = expr e and struct_decl = get_struct_decl name in
          let member_t = get_struct_member_type struct_decl member
              ("Illegal struct member access: " ^ name  ^ "." ^ member) in
          check_assign member_t t ex
         
      | Binop (e1,op,e2) as ex -> 
        let typ1 = expr e1 and
        typ2 = expr e2 in 
        let res = match (typ1,typ2) with (Datatype(t1), Datatype(t2)) -> (
        let operand_type = 
                      match op with 
                          Add when (t1 = Int && t2 = Float) || (t1 = Float && t2 = Int) || 
                                   ((t1 = t2) && (t1 = Int || t1 = Float || t1 = String )) -> t1
                        | Mul when (t1 = Int && t2 = Float) || (t1 = Float && t2 = Int) || 
                                   ((t1 = t2) && (t1 = Int || t1 = Float)) -> t1
                        | Sub | Div when (t1 = Int && t2 = Float) || (t1 = Float && t2 = Int) || 
                                         ((t1 = t2) && (t1 = Int || t1 = Float)) -> t1
                        | Mod when (t1 = t2) && (t1 = Int) -> t1
                        | Equal | Not_equal when (t1 = Int && t2 = Float) || (t1 = Float && t2 = Int) || 
                                                 ((t1 = t2) && (t1 = Int || t1 = Float || t1 = String)) -> Bool
                        | Lt | Lte | Gt | Gte when (t1 = Int && t2 = Float) || (t1 = Float && t2 = Int) || 
                                                   ((t1 = t2) && (t1 = Int || t1 = Float)) -> Bool
                        | And | Or when (t1 = t2) && (t1 = Bool) -> Bool
                        | _ -> raise (Failure ("Illegal binary operator " ^ print_cmpdtyp_info typ1 ^ " " ^ 
                                      print_oper op ^ " " ^ print_cmpdtyp_info typ2 ^
                                      " usage in " ^ print_expr_string ex))

                          (* Matrix operations also need to be added here *)
                      in Datatype(operand_type)
            )
            | _ -> raise (Failure("Operator not found"))
        in res
      | Unop (op,e) as ex -> 
        let typ = expr e in ( match typ with Datatype(dt) -> (

            let operand_type = 
              match op with 
              Neg when dt = Int || dt = Float -> dt
            | Not when dt = Bool -> Bool
            | _ -> raise (Failure ("Illegal unary operator " ^ print_uoper op ^ " usage in " ^ print_expr_string ex))

        in Datatype(operand_type)
        ) 
          | _ -> raise (Failure ("This operator has not been implemented")))
    
      (* Currently only print is the built in function,
      any other built in functions we would like to include must
      be first added to the function keywords_builtin and then
      corresponding Funcall must be added here  *)

      (* Example Funcall for print *)

      | FunCall ("print",_) -> Datatype(Int) 
      | FunCall ("print",_) -> Datatype(String)
      | FunCall (fn,arg_list) as call ->
            let fd = func_decl fn in
            if List.length arg_list != List.length fd.args then
              raise (Failure ("Expecting " ^  string_of_int (List.length fd.args) ^  " number of arguments in " ^ print_expr_string call)) 
            else 
              List.iter2 (fun (sc,arg,name,_ ,_) e -> let typ = expr e in
                  ignore (if check_assign_stmt arg typ then arg 
                          else raise (Failure ("Illegal arguments found! Expected " ^ print_cmpdtyp_info arg ^ " in " ^
                                          print_expr_string e ^ " but got " ^ print_cmpdtyp_info typ))))
              fd.args arg_list;
              fd.ret_type
 
    in


    let check_bool_expr b = if not (type_match [|Bool|] (expr b))
        then raise (Failure ("Expected boolen expression in " ^ print_expr_string b))
        else () in
    
    (* semantically-checking statement *)
    let rec semantic_check_stmt = function 
        Block blk -> let rec check_block = function 
          [Return _ as s] -> semantic_check_stmt s
        | Block blk :: blks -> check_block (blk @ blks)
        | s :: blks -> semantic_check_stmt s; check_block blks
        | [] -> ()
      in check_block blk
      | Expr e -> ignore (expr e)
      | Break e -> 
            if e != Noexpr then raise (Failure ("Break statement should include Noexpr, " ^
                                                    print_expr_string e ^ " found"))
      | Continue e ->
            if e != Noexpr then raise (Failure ("Continue statement should include Noexpr, " ^
                                                    print_expr_string e ^ " found"))
      | Return r -> let rt_expr = (expr r) in
          if (check_assign_stmt rt_expr func.ret_type) then () else
          raise (Failure ("Return gives " ^ print_cmpdtyp_info rt_expr ^ " but expected " ^
                        print_cmpdtyp_info func.ret_type ^ " in " ^ print_expr_string r ))
      | If (e,b1,b2) -> check_bool_expr e; semantic_check_stmt b1; semantic_check_stmt b2;
      | Loop (e1,e2,st) -> check_bool_expr e1; ignore (expr e2); semantic_check_stmt st; 

    in 
    semantic_check_stmt (Block func.body)

  in
  List.iter semantic_check_func funcs
