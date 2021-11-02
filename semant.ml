(* Semantic checker for Tureasy *)

open Ast
open Sast

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

let get_struct_name = function
struct_typ -> struct_typ.name

let check_for_empty_struct errormsg = function
  { name = n; members = []; } -> raise (Failure (errormsg n))
  | _ -> ()

let check_duplicate_fields errormsg = function
   { name = n; members = memberlst; } 
   -> check_duplicate (fun field -> "Found duplicate field '" ^ field ^ "' in struct declaration") (List.map thirdoffive memberlst)

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

  (* add struct definitions to symbol table *)

  (* Tested and working: 
      1. checking globals - no void 
      2.                  - no duplicates 
      3. checking struct decl - no fields not allowed 
      4.                      - no duplicate struct names 
      5.                      - no duplicate field names 
  *)
  
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

in
List.iter semantic_check_func funcs
  
    (* above is also working code with more deatiled checking yet to be written*)

    (* above does keyword checking, duplicate function/ variables checking *)