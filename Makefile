.PHONY: all clean

all:
	ocamlbuild diamond.byte

clean:
	ocamlbuild -clean
