# Description: Makefile for building and testing the ocaml project

# Variables
INTERPRETER_PATH := .
OUTPUT_DOC_PATH := docs
OUTPUT_JS_PATH := ../server/src/runtime
# OUTPUT_JS_PATH := .

# Targets
.SILENT: pre-build build watch clean js

default:
	opam update
	opam install . --deps-only --yes
	make build

pre-build:
	cd $(INTERPRETER_PATH)
	eval $(opam env)

build:
	make pre-build
	make clean
	dune build
	printf "\nBuild successful\n"

watch:
	make pre-build
	dune build -w --terminal-persistence=clear-on-rebuild

test:
	make pre-build
	dune runtest

install:
	make pre-build
	dune install

lint:
	make pre-build
	dune build @lint
	dune build @fmt

format:
	make pre-build
	dune build @fmt --auto-promote

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

js:
	make pre-build
	dune build @js
	rm -rf $(OUTPUT_JS_PATH)/runtime.js
	cp -r $(INTERPRETER_PATH)/_build/default/bin/runtime.js $(OUTPUT_JS_PATH)/runtime.js
	printf "\nGenerated runtime successful\n"
