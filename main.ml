exception InvalidProg
exception NoInputFile
open Printf

let usage = Printf.sprintf("Syntax to compile: <filename>")

let prog_name source_path = 
	let directories = (Str.split (Str.regexp_string("/")) source_path) in
	let file = List.nth path ((List.length directories) - 1) in
	let prog = (Str.split (str.regexp_string ".") file) in 
				List.nth prog ((List.length prog) - 2)


let main () = 

	try

		let prog_name = if Array.length Sys.argv > 1 then
							prog_name Sys.argv.(1)
				   else
				   			raise NoInputFile in

		let input = open_in Sys.argv.(1) in

		let lex_buffer = Lexing.from_channel input in 
		let prog = Parser.start Lexer.tokens lex_buffer in 

		if Semantic.check_program prog 
			then Compile.program prog prog_name
		else
			raise InvalidProg 
						
			with
			| NoInputFile -> ignore Printf.printf("Please provide a Tureasy file in input\n"); exit 1
			| InvalidProg -> ignore Printf.printf("Invalid program. Semantic errors present in program\n"); exit 1
		| End_of_file -> printf "End of File!!"; exit 1


let _ = Printexc.print main ()
