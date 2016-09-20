all:
	ocamlbuild -use-ocamlfind tableau.native

clean:
	ocamlbuild -clean
