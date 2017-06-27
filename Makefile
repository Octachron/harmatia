OCAMLC = ocamlc -I +compiler-libs
OCAMLOPT= ocamlopt -I +compiler-libs

all: harmatia harmatia.cma

printer.cmx: printer.ml
	$(OCAMLOPT) -c printer.ml -o printer.cmx

printer.cmo: printer.ml
	$(OCAMLC) -c printer.ml -o printer.cmx

harmatia: plugin.ml printer.cmx
	$(OCAMLOPT) printer.cmx -shared plugin.ml -o $@

harmatia.cma: plugin.ml printer.cmo
	$(OCAMLC) -a printer.cmo plugin.ml -o $@
