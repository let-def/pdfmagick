all:
	ocamlbuild -use-ocamlfind -tag "package(camlpdf)" main.native
