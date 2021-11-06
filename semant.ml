(* Semantic checker for Tureasy *)

open Ast
(* open Sast *)

module StringMap = Map.Make(String)   (* Symbol table *)

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
      (Datatype(p1),Datatype(p2)) -> if p1 = p2 then true else false
    | (Struct(s1), Struct(s2)) -> if s1 = s2 then true else
        (print_endline (s1 ^ s2); false)
    | _ -> false


let semantic_check program = 
  
  let global_vars = program.globals
  and funcs = program.funcs
  and structs = program.structs in
  
  (* Check global variables *)

  (* check for globals defined as void type *)
  List.iter (check_for_void (fun name -> "Found illegal void global '" ^ name ^ "'")) global_vars;
  (* check for duplicate variable names *)
  check_duplicate (fun name -> "Found duplicate global '" ^ name ^ "'") (List.map thirdoffive global_vars);

  (* Check structs *)

  (* check for struct without any fields *)
  List.iter (check_for_empty_struct (fun name -> "Found struct without fields '" ^ name ^ "'")) structs;
  (* check for duplicate struct names *)
  check_duplicate (fun name -> "Found duplicate struct '" ^ name ^ "'") (List.map get_struct_name structs);
  (* check for duplicate field names *)
  List.iter (check_duplicate_fields (fun fieldmessage -> fieldmessage)) structs;

 (* above has been tested, and is working *)




(* check for functions *)
let keywords_builtin = Array.to_list
  [|
    "print";      
    (* more keywords to add *)
  |]
in

List.iter (fun fname ->
  if List.mem fname (List.map (fun fd -> fd.fname) funcs)
  then raise (Failure ("Function " ^ fname ^ " cannot be defined, it is built-in"))
) keywords_builtin;

check_duplicate (fun name -> "Duplicate function " ^ name )
  (List.map (fun fd -> fd.fname) funcs);
        (*  duplicate functions are checked *)

let built_in_decls = StringMap.empty in

let func_decls = List.fold_left (fun map fd -> StringMap.add fd.fname fd map)
                      built_in_decls funcs 
in

let func_decl f = try StringMap.find f func_decls
    with Not_found -> raise (Failure ("Unknown function " ^ f ^ " call"))
in
  
  (* main has to be there in every program *)
let _ = func_decl "main" in

let semantic_check_func func = 

  List.iter (check_for_void (fun arg -> "Illegal void arguments " ^ arg ^ 
      " in " ^ func.fname)) func.args;

  check_duplicate (fun arg -> "Duplicate arguments " ^ arg ^ " in " ^ func.fname)
     (List.map thirdoffive func.args);

  List.iter (check_for_void (fun local -> "Illegal void locals " ^ local ^
      " in" ^ func.fname)) func.local_vars;

  check_duplicate (fun local -> "Duplicate locals " ^ local ^ " in " ^ func.fname)
     (List.map thirdoffive func.local_vars);



     (* expression's semantic checking *)
  let rec expr: expr-> cmpd_typ = function
      Intlit _ -> Datatype(Int)
    | Floatlit _ -> Datatype(Float)
    | True -> Datatype(Bool)
    | False -> Datatype(Bool)           (* the basic datatypes checking *)
    | Strlit _ -> Datatype(String)
    | Noexpr -> Datatype(Void)
    | Null -> Datatype(Void)
  in
 

          (* to check if an expression is boolean or not *)
  let check_bool_expr b = if not (type_match [|Bool|] (expr b))
      then raise (Failure ("Expected boolen expression in " ^ print_expr_string b))
      else () in

              (* semantic check for statements *)
  let rec semantic_check_stmt = function 
      Block blk -> let rec check_block = function 
        [Return _ as s] -> semantic_check_stmt s
      | Block blk :: blks -> check_block (blk @ blks)
      | s :: blks -> semantic_check_stmt s; check_block blks
      | [] -> ()                      (* making sure it is a block *)
     in check_block blk
    | Expr e -> ignore (expr e)
    | Break e -> 
          if e != Noexpr then raise (Failure ("Break statement should include Noexpr, " ^
                                                  print_expr_string e ^ " found"))
    | Continue e ->               (*  break and continue statements should not have any follow up expression *)
          if e != Noexpr then raise (Failure ("Continue statement should include Noexpr, " ^
                                                  print_expr_string e ^ " found"))
    | Return r -> let rt_expr = (expr r) in
        if (check_assign_stmt rt_expr func.ret_type) then () else     (* the return type of function must match what it is returning*)
        raise (Failure ("Return gives " ^ print_cmpdtyp_info rt_expr ^ " but expected " ^
                      print_cmpdtyp_info func.ret_type ^ " in " ^ print_expr_string r ))
    | If (e,b1,b2) -> check_bool_expr e; semantic_check_stmt b1; semantic_check_stmt b2
    | Loop (e1,e2,st) -> check_bool_expr e1; ignore (expr e2); semantic_check_stmt st

  in 
  semantic_check_stmt (Block func.body)

in
List.iter semantic_check_func funcs



  
