# REPL Module

The REPL module is responsible for providing a Read-Eval-Print Loop (REPL) interface for the interpreter. 
The module consists of the following components:

| File                         | Description                                                    |
| ---------------------------- | -------------------------------------------------------------- |
| [`debug.ml`](./debug.ml)     | Provides debugging functionalities for the REPL.               |
| [`errors.ml`](./errors.ml)   | Defines error handling utilities for the REPL.                 |
| [`exec.ml`](./exec.ml)       | Handles the execution of commands within the REPL.             |
| [`execute.ml`](./execute.ml) | Executes the event provided by the user in the REPL.           |
| [`export.ml`](./export.ml)   | Handles exporting the current state of the graph to a file.    |
| [`help.ml`](./help.ml)       | Implements the help command for listing available commands.    |
| [`quit.ml`](./quit.ml)       | Implements the quit command for exiting the REPL.              |
| [`state.ml`](./state.ml)     | Manages the state of the REPL (Read-Eval-Print Loop).          |
| [`view.ml`](./view.ml)       | Provides functionality to view the current state of the graph. |
