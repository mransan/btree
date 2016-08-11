OCB_INC  += -I src/
OCB_FLAGS = -use-ocamlfind 
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)


all:
	$(OCB) btree_test.native
	export OCAMLRUNPARAM="b" && ./btree_test.native
