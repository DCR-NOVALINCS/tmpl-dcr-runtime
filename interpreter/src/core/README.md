# Core Module

The core module is responsible for providing the core functionality of the interpreter.
The module consists of the following components:

| Module                         | Description                                                                                                                    |
| ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------ |
| [`typing`](typing/README.md)   | The typing module is responsible for type checking the program.                                                                |
| [`dcr`](dcr/README.md)         | The DCR module is responsible for applying the semantics of the Dynamic Condition Response Graphs (DCR Graphs) to the program. |
| [`ast`](ast/README.md)         | The AST module is responsible for defining the syntax and other things for the interpreter.                                    |
| [`parsing`](parsing/README.md) | The parser module is responsible for parsing the input program.                                                                |