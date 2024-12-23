# Description: Makefile for building and testing the ocaml project

# Variables
INTERPRETER_PATH := .
OUTPUT_DOC_PATH := docs
OUTPUT_JS_PATH := ../server/src/runtime
# OUTPUT_JS_PATH := .

setup:
	cd $(INTERPRETER_PATH)
	opam update
	opam install . --deps-only
	make build

pre-build:
	cd $(INTERPRETER_PATH)
	eval $(opam env)

build:
	make pre-build
	# make clean
	dune build
	cp -rf $(INTERPRETER_PATH)/_build/default/bin/main.exe tmpl_dcr.exe
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
	make clean
	make pre-build
	dune build @doc
	mkdir -p $(OUTPUT_DOC_PATH) 
	cp -r _build/default/_doc/_html/* $(OUTPUT_DOC_PATH)
	touch $(OUTPUT_DOC_PATH)/.nojekyll # Prevents github from ignoring the docs folder

# Targets
.SILENT: pre-build build watch clean
.PHONY: test install format doc