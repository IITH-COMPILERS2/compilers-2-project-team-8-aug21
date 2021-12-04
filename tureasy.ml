module StringMap = Map.Make(String)

type action = Ast | LLVM_IR | Compile

let _ =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./tureasy.native [-a|-l|-c] [file.tz]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  let ast =
    try
      Parser.start Lexer.token lexbuf
    with exn ->
      (
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        let failure_string = Scanf.unescaped(
          "Exception: " ^ Printexc.to_string exn ^ "\n" ^
          "Line number: " ^ (string_of_int line) ^ "\n" ^
          "Character: " ^ (string_of_int cnum) ^  "\n" ^
          "Token: " ^ tok
        ) in
        raise (Failure failure_string)
      )
  in
  Semant.semantic_check ast;
  (* ast; *)
  match !action with
    Ast -> print_string (Ast.print_program_string ast)
  | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
  | Compile -> let m = Codegen.translate ast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
