# Tests

This directory contains all the tests for the project. In order to run the tests, you need to have the project built before-hand (see [here](../../README.md)).

## Running the tests

To run all tests, you can run the following command in the `interpreter` directory of the project.

```bash
make check
```

Or you can run the following command if you don't trust some random `Makefile`.

```bash
dune runtest
```

## Core tests

The core tests are located in the `files` directory of the project.
In this directory, the tests are divided into the following categories:

- `annotations`: tests for the control flow annotations (`if` and `foreach`).
- `export-events`: tests for the export events of templates.
- `runtime`: tests for the runtime/transitions of the DCR graph.
- `typechecker`: tests for the typechecker.

**Note:** Some `annotations` tests are not working properly because of the way that the `if` and `foreach` annotations evaluate the expressions that are dynamic (e.g. from `@trigger`).
