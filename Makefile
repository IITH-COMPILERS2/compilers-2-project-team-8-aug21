# Make sure ocamlbuild can find opam-managed packages: first run
# eval `opam config env`
#

.PHONY : all
all : tureasy.native

.PHONY : tureasy.native
tureasy.native :
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 tureasy.native


.PHONY : clean
clean :
	ocamlbuild -clean
