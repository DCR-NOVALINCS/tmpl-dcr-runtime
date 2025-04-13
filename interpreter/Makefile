# Variables
OUTPUT_DOC_PATH = ../docs
DOCKER_IMAGE_NAME = template-dcr-interpreter

setup:
	opam update
	opam install . --deps-only

pre-build:
	eval $(opam env)

GREEN = \033[1;32m
NC = \033[0m
build:
	make pre-build
	# make clean
	dune build
	cp -f _build/install/default/bin/tmpl_dcr ./tmpl_dcr
	printf "${GREEN}Success: ${NC}Build created\n"

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

docker: 
	make pre-build
	docker build -t $(DOCKER_IMAGE_NAME) .

# Targets
.SILENT: pre-build build watch clean
.PHONY: test install format doc