OCB_INC  += -I src/ -I tests/ -X js/
#OCB_FLAGS += -ocamlopt ocamloptp
OCB_TESTS_FLAGS += -use-ocamlfind -pkgs unix
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)

.PHONY: all build perf unix test js 

all: build test perf

# The core library dbtlk does not depend on any other 
# packages

lib.byte:
	$(OCB) dbtlk.cma

lib.native:
	$(OCB) dbtlk.cmxa
	$(OCB) dbtlk.cmxs

perf.byte: build 
	rm -f *.dump
	rm -f ./btree_*.ml
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 3 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 7 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 15 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 31 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 41 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 63 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 127 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 255 run
	export OCAMLRUNPARAM="b" && ./btree_bytes_perf.native 511 run

perf.unix: build 
	rm -f *.data 
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 15
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 21
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 25
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 31
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 35
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 41
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 45
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 51
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 55
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 61
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 65
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 71
	@export OCAMLRUNPARAM="b" && rm -f *.data && ./btree_unix_perf.native 75
  
test: build 
	export OCAMLRUNPARAM="b" && time ./btree_bytes_test.native

js: build
	cd js/node_modules/btree && npm run build && time node src/btree_bytes_test.js  

table: build
	export OCAMLRUNPARAM="b" && ./table_test.native

build:
	$(OCB) $(OCB_TESTS_FLAGS) btree_bytes_test.native
	$(OCB) $(OCB_TESTS_FLAGS) btree_bytes_perf.native
	$(OCB) $(OCB_TESTS_FLAGS) btree_unix_perf.native
	$(OCB) $(OCB_TESTS_FLAGS) table_test.native

clean:
	rm -f *.data
	$(OCB) -clean
