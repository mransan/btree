OCB_INC  += -I src/
#OCB_FLAGS += -ocamlopt ocamloptp
OCB_FLAGS += -use-ocamlfind -pkgs unix 
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)

.PHONY: all build perf unix test 

all: build test perf

perf: build 
	rm -f *.dump
	rm -f data 
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 3
	export OCAMLRUNPARAM="b" && time ./btree_unix_perf.native
  
test: build 
	export OCAMLRUNPARAM="b" && ./btree_bytes_test.native

build:
	$(OCB) btree_bytes_test.native
	$(OCB) btree_bytes_perf.native
	$(OCB) btree_unix_perf.native

clean:
	rm -f data
	$(OCB) -clean
