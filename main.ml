exception InvalidProg
exception NoInputFile
open Printf
open Lexing
open Lexer
open Parser
open Semant

let usage = Printf.sprintf("Syntax to compile: <filename>")

let main () = begin

	(* try *)

		let prog_name = 
			    if (Array.length Sys.argv) = 2 then 
						Sys.argv.(1)
				  else
				   	raise (Failure ("Please provide a Tureasy file (extension .tz) as input") )
		in

		let input = open_in prog_name in
		let lex_buffer = Lexing.from_channel input in 
		let prog = Parser.start Lexer.token lex_buffer in 
		  Semant.semantic_check prog 

(* 		if Semant.semantic_check prog 
			then Compile.program prog prog_name
		else
			raise InvalidProg 
						
			with
			| NoInputFile -> ignore Printf.printf("Please provide a Tureasy file in input\n"); exit 1
			| InvalidProg -> ignore Printf.printf("Invalid program. Semantic errors present in program\n"); exit 1
		| End_of_file -> printf "End of File!!"; exit 1 *)


(* let _ = Printexc.print main () *)
end;;
main() ;;
