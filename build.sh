ocamllex lexer.mll
ocamlyacc parser.mly
ocamlc -c ast.ml parser.mli parser.ml
ocamlopt ast.ml semant.ml parser.ml lexer.ml main.ml
rm *.cmo *.cmi *.cmx *.o
