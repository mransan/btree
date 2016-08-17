OCB_INC  += -I src/
OCB_FLAGS = -use-ocamlfind 
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)


.PHONY: all build 

all: build
	export OCAMLRUNPARAM="b" && ./btree_test.native

build:
	$(OCB) btree_test.native

clean:
	$(OCB) -clean


