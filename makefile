
byt:
	ocamlc fcdll.mli fcdll.ml -a -o fcdll.cma

opt:
	ocamlopt fcdll.mli fcdll.ml -a -o fcdll.cmxa

world: byt opt

install:
	mkdir `ocamlc -where`/fcdll
	mv *.cm[ia] *.cmxa `ocamlc -where`/fcdll

clean:
	rm *.cm[iaox] *.a *.cmxa
