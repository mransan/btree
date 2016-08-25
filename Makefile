OCB_INC  += -I src/
OCB_FLAGS += -ocamlopt ocamloptp
OCB_FLAGS += -use-ocamlfind -pkgs unix 
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)

.PHONY: all build 

all: build
	export OCAMLRUNPARAM="b" && ./btree_test.native

perf: build 
	rm -f *.dump
	export OCAMLRUNPARAM="b" && ./btree_perf.native 3

build:
	$(OCB) btree_test.native
	$(OCB) btree_perf.native

clean:
	$(OCB) -clean


