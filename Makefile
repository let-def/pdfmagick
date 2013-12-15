all:
	ocamlbuild -use-ocamlfind -tag "package(camlpdf,yojson)" main.native

clean:
	ocamlbuild -clean
