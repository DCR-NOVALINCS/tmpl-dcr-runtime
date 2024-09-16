> [!WARNING] This project is still under development.

# Table of Content
- [Table of Content](#table-of-content)
- [Requirements](#requirements)
- [Running the interpreter](#running-the-interpreter)
  - [How to compile](#how-to-compile)
  - [How to run](#how-to-run)
    - [Available Commands](#available-commands)
      - [Help (h)](#help-h)
      - [View (v)](#view-v)
      - [Debug (d)](#debug-d)
      - [Exec (e)](#exec-e)
      - [Exit (q)](#exit-q)

# Requirements

In order to run this tool, you need the following tools:
- OCaml

This project is being developed with the version of the `ocaml-base-compiler`: `5.1.1`

# Running the interpreter 

## How to compile 

To compile this project, you need to execute the following command:

```bash
dune build
```

## How to run

To run the interpreter, you need to execute the following command:

```bash
dune exec tmpl_dcr
```

### Available Commands

#### Help (h)

This command will show the available commands and their descriptions.

```bash
> h
Commands: 
- view (v): View the current program
- debug (d): View the current program with relations
- exec (e) <event_id> <expr>: Execute an event with an expression
- exit (q): Exit the program
- help (h): Display this message
```

#### View (v)

This command will show the current program.

```bash
> v
a: A[?: Number] -> ()
b: B[0] -> 0
```

#### Debug (d)

This command will show the current program with relations.

```bash
> d
a: A[?: Number] -> ()
b: B[0] -> 0
;
a -->% b
```

#### Exec (e)

This command will execute an event with an expression.

```bash
> v 
a: A[?: Number] -> ()
b: B[0] -> 0
> e a 1
a: A[?: Number] -> 1
b: B[0] -> 0
```

#### Exit (q)

This command will exit the program.