module L = Llvm
module A = Ast
module P = Printf
module StringMap = Map.Make(String)

let translate program = 
	let globals = program.A.globals in 
	let functions = program.A.funcs in 
	let structs = program.A.structs in 
	let context = L.global_context() in 
	let the_module = L.create_module context "Tureasy" in


	let i32_t  = L.i32_type context in
	let i1_t   = L.i1_type context  in
	let i8_t   = L.i8_type context  in
	let float_t = L.double_type context in
	let string_t = L.pointer_type i8_t in 
	let void_t = L.void_type context in  
	let matrix_t = L.pointer_type i32_t in

	let datatype_infer = function
		  A.Bool        -> i1_t
		| A.Void        -> void_t
		| A.Int         -> i32_t
		| A.Float       -> float_t
		| A.Matrix      -> matrix_t
		| A.String      -> string_t

	in

	let lcmpd_type_infer struct_decl_map = function
		  A.Datatype (typ) -> datatype_infer (typ)
		| A.Struct (s)        -> L.pointer_type (fst (StringMap.find s struct_decl_map))
	in

  let struct_decl_map =
    let add_struct m structdecl =
      let name = struct_decl.A.name
      and members = Array.of_list
        (List.map (fun (_,t, _,_,_) -> lcmpd_type_infer m t) struct_decl.A.members) in
      let structname = L.named_struct_type context ("struct." ^ name) in
        L.struct_set_body structname members false;
            StringMap.add name (structname, structdecl) m in
    List.fold_left add_struct StringMap.empty structs in

  let struct_lltype_list =
    let bindings = StringMap.bindings struct_decl_map in
    List.map (fun (_, (typ_l, _)) -> L.pointer_type typ_l) bindings
  in

  let get_struct_pointer_lltype llval =
    let typ_l = L.type_of llval in
    A.get_try typ_l struct_lltype_list
  in

	let str_check_empty = L.define_global "__empty string" (L.const_string context "") the_module in 
	let initialize = function 
		A.Datatype(typ) -> (
			match typ with 
					A.Float -> L.const_float float_t 0.0
				|	A.String -> L.const_bitcast str_check_empty string_t
				|	A.Matrix -> L.const_null matrix_t
				|	A.Bool  -> L.const_int i1_t 0 
				| _ -> L.const_int i32_t 0 
			)
		| A.Struct(_) as typ -> L.const_null (lcmpd_type_infer struct_decl_map typ)
	in
	
	let print_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in 
	let print_func = L.declare_function "print" print_t the_module in 
	
	let global_vars = 
	    let global_var m (typ,name) = 
		let init  = initialize typ in 
		let global_val_ll = L.define_global name init the_module in 
		StringMap.add name (global_val_ll, typ) m in 
		let globs = List.map (fun (_,t,n,_,_) -> (t,n)) globals in 
		List.fold_left global_var StringMap.empty globs in 

	let func_decls = 
		let func_decl m fdecl = 
		let name = fdecl.A.fname and 
			args_type= Array.of_list
				(List.map(fun (_,t,_,_,_) -> lcmpd_type_infer struct_decl_map t) fdecl.A.args) in
				let ftyp = L.function_type (lcmpd_type_infer struct_decl_map fdecl.A.ret_type) args_type in
				StringMap.add name( L.declare_function name ftyp the_module,fdecl) m in
				List.fold_left func_decl StringMap.empty functions in

	let find_func fname = StringMap.find fname func_decls in

	let build_predefined fname actuals the_builder = 
		let (fdef, fdecl) = (try StringMap.find fname func_decls with 
				Not_found -> raise (Failure("Not defined: " ^ fname))) in 
		let result = (match fdecl.A.ret_type with 
				A.Datatype(t) when t = A.Void -> ""
				| _ -> fname ^ "_res")
	in 
	L.build_call fdef actuals result the_builder in 

	let build_func fdecl = 
		let (func, _) = try StringMap.find fdecl.A.fname func_decls with Not_found -> raise (Failure("Error 127")) in 
		let builder = L.builder_at_end context (L.entry_block func) in 

		let local_vars = 
			let add_arg m (s, name) p = L.set_value_name name p;
			let locals = L.build_alloca (lcmpd_type_infer struct_decl_map s) name builder in 
			ignore (L.build_store p locals builder);
			StringMap.add name (locals, s) m 
		in 

			let add_local m (t, n) = 
				let local_var = L.build_alloca (lcmpd_type_infer struct_decl_map t) n builder in 
				ignore (L.build_store (initialize t) local_var builder);
				StringMap.add n (local_var, t) m 
				in

	let args = List.fold_left2 add_arg StringMap.empty (List.map (fun (_,t,n,_,_) -> (t,n)) fdecl.A.args) (Array.to_list (L.params func)) in 
	List.fold_left add_local args (List.map (fun (_,t,n,_,_) -> (t,n)) fdecl.A.local_vars) in 

	let lookup_llval x = try fst (StringMap.find x local_vars)
									with Not_found -> fst (StringMap.find x global_vars)
	in

	let lookup_type n = try snd (StringMap.find n local_vars)
									with Not_found -> snd (StringMap.find n global_vars)
	in 

	let get_struct_decl sname = 
		try 
			let typ = lookup_type sname in 
			match typ with 
				A.Struct(s) -> snd (StringMap.find s struct_decl_map)
			| _ -> raise Not_found
		with Not_found -> raise (Failure(sname ^ " not declared"))
	in 

	let int_ops = function 
			A.Add     -> L.build_add
    | A.Sub     -> L.build_sub
    | A.Mul    -> L.build_mul
    | A.Div     -> L.build_sdiv
    | A.Mod     -> L.build_srem
    | A.Or      -> L.build_or
    | A.And     -> L.build_and
    | A.Equal   -> L.build_icmp L.Icmp.Eq
    | A.Not_equal     -> L.build_icmp L.Icmp.Ne
    | A.Gt -> L.build_icmp L.Icmp.Sgt
    | A.Gte     -> L.build_icmp L.Icmp.Sge
    | A.Lt    -> L.build_icmp L.Icmp.Slt
    | A.Lte     -> L.build_icmp L.Icmp.Sle
    in	

   let float_ops = function 
   	  A.Add     -> L.build_fadd
    | A.Sub     -> L.build_fsub
    | A.Mul    -> L.build_fmul
    | A.Div     -> L.build_fdiv
    | A.Equal   -> L.build_fcmp L.Fcmp.Ueq
    | A.Not_equal     -> L.build_fcmp L.Fcmp.Une
    | A.Lt    -> L.build_fcmp L.Fcmp.Ult
    | A.Lte     -> L.build_fcmp L.Fcmp.Ule
    | A.Gt -> L.build_fcmp L.Fcmp.Ugt
    | A.Gte     -> L.build_fcmp L.Fcmp.Uge
    | _         -> raise Not_found
    in

   let bool_ops = function 
   		A.And 			-> L.build_and
   	| A.Or 				-> L.build_or
   	| _           -> raise Not_found
   in 

	let rec expr builder = function
		  A.Intlit i  		-> L.const_int i32_t i
		| A.Floatlit f    -> L.const_float float_t f
		| A.True			    -> L.const_int i1_t (1)
		| A.False					-> L.const_int i1_t (0)
		| A.Strlit s     	-> L.build_global_stringptr (Scanf.unescaped s) "strlit" builder
		| A.Noexpr        -> L.const_int i32_t 0
		| A.Null          -> L.const_pointer_null void_t
		| A.Id id         -> let var = L.build_load (lookup_llval id) id builder 
												 in var 
		| A.Asgn (var,e)  -> let e1 = expr builder e 
										in ignore (L.build_store e1 (lookup_llval var) builder); e1
		| A.Unop (op,e)     ->
				let e1 = expr builder e in 
				let typ = L.type_of e1 in 
				(match op with 
						A.Neg ->
							if  typ = float_t then L.build_fneg e1 "f_neg" builder
							else L.build_neg e1 "neg" builder 
					| A.Not -> L.build_not e1 "not" builder
				)
		| A.Binop (e1,op,e2)->
			let e11 = expr builder e1 and e22 = expr builder e2 in
			let typ1 = L.type_of e11 and typ2 = L.type_of e22 in 
			let typs = (typ1, typ2) in
			(
				if typs = (i32_t, i32_t) && op = A.Mod then (build_predefined "mod" [|e11; e22|] builder)
				else if typs = (i32_t, i32_t) then (int_ops op e11 e22 "int_ops" builder)
				else if typs = (float_t, float_t) then (float_ops op e11 e22 "float_ops" builder)
				else if typs = (i1_t, i1_t) then (bool_ops op e11 e22 "bool_ops" builder)
	      else raise (Failure ((A.print_oper op) ^ " not defined for " 
										^ (L.string_of_lltype typ1) ^ " and " ^	(L.string_of_lltype typ2) ^ " in " 
										^ (A.print_expr_string e2))) 
			
			)
		| A.FunCall ("print", arg) ->
			let args = List.map (expr builder) arg 
		  in L.build_call print_func (Array.of_list args) "print" builder
		| A.FunCall (fn,arg_list) ->
			let args = Array.of_list (List.rev (List.map(expr builder) (List.rev arg_list))) in
			let (func_def, func_decl) = find_func fn 
		  in L.build_call func_def args fn builder
	in

	let add_terminal builder instr =
	      match L.block_terminator (L.insertion_block builder) with
	        Some _ -> ()
	      | None -> ignore (instr builder)
	in

	let rec stmt builder = function
		  A.Block blk        -> List.fold_left stmt builder blk
		| A.Expr e           -> ignore (expr builder e); builder 	
		(* | A.Break e          -> ignore (L.build_br builder);
		| A.Continue e       -> ignore (L.build_br builder); *)
		| A.If (e,b1,b2)     ->
			let condition = expr builder e in
			let then_blk = L.append_block context "then" func in 
			let merge_blk = L.append_block context "merge" func in 
			add_terminal (stmt (L.builder_at_end context then_blk) b1)
			(L.build_br merge_blk);

			let else_blk = L.append_block context "else" func in 
			add_terminal (stmt (L.builder_at_end context else_blk) b2)
			(L.build_br merge_blk);

			ignore (L.build_cond_br condition then_blk else_blk builder);
			L.builder_at_end context merge_blk 
		
		| A.Return r         -> 
			ignore (match fdecl.A.ret_type with 
				A.Datatype (typ) when typ = A.Void -> L.build_ret_void builder
				| _ -> L.build_ret (expr builder r) builder ); builder

		| A.Loop (e1,e2,st)  ->

			let body = A.Block [st;A.Expr e2] in   (*thoda doubt*)
			let body_blk = L.append_block context "loop_body" func in
			let pred_blk = L.append_block context "loop" func in 
						   ignore (L.build_br pred_blk builder);
			add_terminal (stmt (L.builder_at_end context body_blk) body)
			(L.build_br pred_blk);
			let merge_blk = L.append_block context "merge" func in
			let pred_builder = L.builder_at_end context pred_blk in 
			let condition = expr pred_builder e1 in  
			ignore (L.build_cond_br condition body_blk merge_blk pred_builder);
			L.builder_at_end context merge_blk
	in 

	let builder = stmt builder (A.Block fdecl.A.body) 
	in

	add_terminal builder (match fdecl.A.ret_type with

		A.Datatype(typ) when typ = A.Float -> L.build_ret (L.const_float float_t 0.0)
		| A.Datatype(typ) when typ = A.Void -> L.build_ret_void
		| typ -> L.build_ret (L.const_int (lcmpd_type_infer struct_decl_map typ) 0)
	)
	in
  (* ending the functions conversion*)
	List.iter build_func functions;
	the_module
