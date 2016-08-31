OCB_INC  += -I src/
#OCB_FLAGS += -ocamlopt ocamloptp
OCB_FLAGS += -use-ocamlfind -pkgs unix 
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)

.PHONY: all build perf unix test 

all: build test perf

perf.byte: build 
	rm -f *.dump
	rm -f ./btree_*.ml
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 3 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 7 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 15 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 31 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 63 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 127 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 255 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 511 run

perf.unix: build 
	rm -f *.data 
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 35
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 37
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 39
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 41
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 45
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 51
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 55
  
test: build 
	export OCAMLRUNPARAM="b" && ./btree_bytes_test.native

build:
	$(OCB) btree_bytes_test.native
	$(OCB) btree_bytes_perf.native
	$(OCB) btree_unix_perf.native

clean:
	rm -f *.data
	$(OCB) -clean
