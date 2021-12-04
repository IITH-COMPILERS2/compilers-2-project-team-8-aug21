# Make sure ocamlbuild can find opam-managed packages: first run
#
# eval `opam config env`

# Easiest way to build: using ocamlbuild, which in turn uses ocamlfind

OBJS = ast.cmx codegen.cmx parser.cmx lexer.cmx semant.cmx tureasy.cmx
.PHONY : all
all : tureasy.native
tureasy : $(OBJS)
	ocamlfind ocamlopt -linkpkg -package llvm -package llvm.analysis $(OBJS) -o tureasy

.PHONY : tureasy.native
tureasy.native :
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 tureasy.native


.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff tureasy lexer.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.s *.ll *.out *.exe
	rm -rf parser.automaton parser.conflicts parser.output

scanner.ml : lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

# Start Parser Testing
yacc : parser.mly
	ocamlyacc -v parser.mly

menhir : parser.mly
	menhir -v parser.mly
# End Parser Testing


%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -c -package llvm $<


ast.cmo :
ast.cmx :
codegen.cmo : ast.cmo
codegen.cmx : ast.cmx
yamml.cmo : semant.cmo lexer.cmo parser.cmi codegen.cmo ast.cmo
yamml.cmx : semant.cmx lexer.cmx parser.cmx codegen.cmx ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
lexer.cmo : parser.cmi
lexer.cmx : parser.cmx
semant.cmo : ast.cmo
semant.cmx : ast.cmx
parser.cmi : ast.cmo