note:
	ocamlbuild -use-ocamlfind -package str,unix main.byte 

init: 
	ocamlbuild -use-ocamlfind -package str,unix init.byte 