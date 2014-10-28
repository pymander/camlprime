# See this for instructions:
# http://mmottl.github.io/ocaml-makefile/
SOURCES=lazyList.mli lazyList.ml rand.mli rand.ml math.mli math.ml prime.mli prime.ml
RESULT=camlprime
PACKS=num

LIBINSTALL_FILES = $(wildcard META *.mli *.cmi *.cma *.cmxa *.a *.so)
OCAMLDOCFLAGS = -stars
NO_CUSTOM = yes
THREADS = yes

all: byte-code-library native-code-library

byte: byte-code-library

opt: native-code-library

install: libinstall

uninstall: libuninstall

update: uninstall install

-include OCamlMakefile
