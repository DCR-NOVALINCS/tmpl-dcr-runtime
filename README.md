
<div align="center">
  <h1>tmpl-dcr-runtime</h1>
  <p>
    Templates in Dynamic Conditional Response (DCR) Graphs.
  </p>


  [![documentation](https://img.shields.io/badge/documentation-unavailable-red)]("")
  [![license](https://img.shields.io/badge/license-MIT-green)](https://github.com/DCR-NOVALINCS/tmpl-dcr-runtime/blob/main/LICENSE)
  
  ![demo](assets/gifs/demo.gif)
</div> 

> [!WARNING]
> This project is still in development.

# Table of Contents

- [Table of Contents](#table-of-contents)
- [About](#about)
  - [Features](#features)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
  - [Usage](#usage)
- [License](#license)

# About 

**t-dcr** is a cli runtime to create and execute [DCR graphs]() with templates in a simple and easy way. 

This project is a part of a master thesis of one of the authors to study and implement a way to abstract and reuse parts of a DCR graph. 

<!-- TODO: Put a complex-ish example -->

Here is a quick sneak peek of how to express a DCR graph with templates in this project.

```
tmpl add(n: Number, m: Number): Result {
  (r: Result)[n + m]
} => r

tmpl sub(n: Number, m: Number): Result {
  (r: Result)[n - m]
} => r

tmpl max(r1: Result, r2: Result) {
  r1 -[r1.value > r2.value]->% r2
  r2 -[r2.value > r1.value]->% r1
}

(i: Input)[?: { l: Number, r: Number }]

i -->> {
  add(n = @trigger.value.l, m = @trigger.value.r) => result0
  sub(n = result0.value, m = @trigger.value.r) => result1
  max(r1 = result0, r2 = result1)
}
```

>[!NOTE] 
> This syntax may change in the future, so be aware of that. 😅


## Features

Here is a list of the features that are available in this project.

| Features                      | Done? |
| ----------------------------- | ----- |
| Basic Semantic of DCR Graphs  | ✅     |
| Templates in DCR Graphs       | ✅     |
| Export to JSON                | ✅     |
| Export back to `.tdcr`        | ✅     |
| Export to `.dot`              | ❌     |
| Reactive data values          | ❌     |
| Interaction between processes | ❌     |

If you have any suggestions or want to contribute to this project, feel free to open an issue or a pull request. 🙂

# Getting Started

## Prerequisites

In order to run this project you need to have the following installed:

- [OCaml](https://ocaml.org/docs/install.html) (5.1.1 or newer)
- [Opam](https://opam.ocaml.org/doc/Install.html) (2.1.2 or newer)

This are the minimum versions that are guaranteed to work and worked during development and tested with. 

## Installation

To install the project you need to clone the repository and install the dependencies. 

```bash
git clone https://github.com/DCR-NOVALINCS/tmpl-dcr-runtime.git
```

After cloning the repository, run the following command in the `interpreter` directory of the project, where the `Makefile` and the `.opam` file are located.

```bash
make setup
```

If you don't trust some strange `Makefile` you can run the following command to install the dependencies manually under the `interpreter` directory.

```bash
opam install . --deps-only
``` 

For some reason you don't have the `tmpl_dcr.opam` file, you can run the following command to install the dependencies.

```bash
opam install ocaml dune odoc menhir yojson cmdliner alcotest
```

And finally, you can build the project by running the following command.

```bash
make build
```

Or `dune build`. And you should be ready to go. 😁 

## Usage

To start the interpreter, you can run the following command in the `interpreter` folder and execute the following command:

```bash
dune exec tmpl_dcr -- <filename.tdcr>
```

Where `<filename>` is the name of the file that contains the DCR graph with templates.

You can see some examples in the [`examples`](/examples) directory of the project to see how to write a DCR graph with templates.

>[!IMPORTANT] 
> The file extension must be `.tdcr` in order to be recognized by the program.

> [!NOTE]
> You can also run the program with the `--help` flag to see the available options or the `--log <level>` to see the debug information. 

---

After running the command, you should see the following prompt.

```
> 
```

From this prompt, you can run the following commands:
<!-- TODO: for each command, show a gif to illustrate to command -->

- **help**: Print the help message.
  ```
  > help
  Available Commands:
  - export <FILENAME>: Creates a file named <FILENAME> with a textual representation of the current state of the graph.
  - execute <EVENT_ID> <EXPR_STRING>: Executes the event <EVENT_ID> with the expression <EXPR_STRING> if needed.
  - view : Views the current state of the graph.
  - exit : Exit the program.
  ```

- **exit**: Exit the program and close the CLI.
  ```
  > exit
  ```

- **view**: View the current state of the program, i.e., the enabled events.
  ```
  > view [-d | --disabled] [-r | --relations] [-t | --templates] [-v | --value]
  ```
  **Flags:**
  - `-d` or `--disabled`: View the disabled events.
  - `-r` or `--relations`: View the relations between the events.
  - `-t` or `--templates`: View the templates that are available in the program.
  - `-v` or `--value`: View the values of the events.

- **execute**: Execute the event with the given `event_id` and the given `expr` as the input.
  ```
  > execute <event_id> <expr>
  ```

- **export**: Export the current state of the program in any of the available modes to the file with the given `name`.
  ```
  > export [-m <mode> | --mode <mode>] <name1> <name2> ... <nameN>
  ```
  **Flags:**
  - `-m <mode>` or `--mode <mode>`: Export the program in the given mode `<mode>`. Available modes are:
    - `tdcr` (default)
    <!-- - `dot` -->
    - `json`

# License

This project is licensed under the **MIT License**.

See [LICENSE](https://github.com/DCR-NOVALINCS/tmpl-dcr-runtime/blob/main/LICENSE) for more information.

<!-- TODO: Add acknowledgements and references.  -->
