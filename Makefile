default:
	opam update
	opam install . --deps-only --yes
	make build

.SILENT: build
build:
	make pre-build
	make clean
	dune build
	printf "\nBuild successful! 🎉\n"

.SILENT: watch
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

.SILENT: clean
clean:
	make pre-build
	dune clean
	rm -rf docs

doc:
	make clean
	make pre-build
	dune build @doc
	mkdir -p docs 
	cp -r _build/default/_doc/_html/* docs

.SILENT: pre-build
pre-build:
	eval $(opam env)

