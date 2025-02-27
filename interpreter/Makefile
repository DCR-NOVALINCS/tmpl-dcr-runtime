# Targets
.SILENT: pre-build build watch clean
.PHONY: test install format doc

# Variables
OUTPUT_DOC_PATH = ../docs

setup:
	opam update
	opam install . --deps-only

pre-build:
	eval $(opam env)

build:
	make pre-build
	# make clean
	dune build
	printf "\nBuild successful\n"

watch:
	make pre-build
	dune build -w --terminal-persistence=clear-on-rebuild
	clear

check:
	make pre-build
	make build
	dune runtest --force -w

install:
	make pre-build
	dune install

format:
	make pre-build
	dune fmt

clean:
	make pre-build
	dune clean
	rm -rf docs

doc:
	make pre-build
	make clean
	dune build @doc
	mkdir -p $(OUTPUT_DOC_PATH) 
	cp -r _build/default/_doc/_html/* $(OUTPUT_DOC_PATH)
	touch $(OUTPUT_DOC_PATH)/.nojekyll # Prevents github from ignoring the docs folder
