ocamllex lexer.mll                      # lexer.ml is output
ocamlyacc parser.mly                    #parser.ml and parser.mli are outputs
ocamlc -c ast.ml parser.mli parser.ml   # ast.cmi .cmo and parser.cmo .cmo generated
ocamlopt ast.ml semant.ml parser.ml lexer.ml main.ml    # rest of files with a.out