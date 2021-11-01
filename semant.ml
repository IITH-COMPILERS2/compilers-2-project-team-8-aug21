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
  


  (* Check functions *)
  
    (* Check expresssions *)

