OCB_INC  += -I src/
#OCB_FLAGS += -ocamlopt ocamloptp
OCB_FLAGS += -use-ocamlfind -pkgs unix 
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)

.PHONY: all build perf unix test 

all: build test perf

perf.byte: build 
	rm -f *.dump
	rm -f ./btree_*.ml
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 3
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 7
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 15
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 31
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 63
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 127
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 255
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 511

perf.unix: build 
	rm -f *.data 
	@export OCAMLRUNPARAM="b" && ./btree_unix_perf.native 3 
	@export OCAMLRUNPARAM="b" && ./btree_unix_perf.native 7 
	@export OCAMLRUNPARAM="b" && ./btree_unix_perf.native 15
	@export OCAMLRUNPARAM="b" && ./btree_unix_perf.native 31
	@export OCAMLRUNPARAM="b" && ./btree_unix_perf.native 63
	@export OCAMLRUNPARAM="b" && ./btree_unix_perf.native 127
	@export OCAMLRUNPARAM="b" && ./btree_unix_perf.native 255
	@export OCAMLRUNPARAM="b" && ./btree_unix_perf.native 511
  
test: build 
	export OCAMLRUNPARAM="b" && ./btree_bytes_test.native

build:
	$(OCB) btree_bytes_test.native
	$(OCB) btree_bytes_perf.native
	$(OCB) btree_unix_perf.native

clean:
	rm -f *.data
	$(OCB) -clean
