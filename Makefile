OCB_INC  += -I src/
#OCB_FLAGS += -ocamlopt ocamloptp
OCB_FLAGS += -use-ocamlfind -pkgs unix 
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)

.PHONY: all build 

all: build
	export OCAMLRUNPARAM="b" && ./btree_test.native

perf: build 
	rm -f *.dump
	export OCAMLRUNPARAM="b" && ./btree_perf.native 3
  
unix: build
	rm -f data 
	export OCAMLRUNPARAM="b" && time ./btree_unix_test.native


build:
	$(OCB) btree_test.native
	$(OCB) btree_perf.native
	$(OCB) btree_unix_test.native
	$(OCB) btree_bytes.native

clean:
	$(OCB) -clean


