all:
	ocamlbuild -use-ocamlfind -tag "package(camlpdf)" main.native

clean:
	ocamlbuild -clean
