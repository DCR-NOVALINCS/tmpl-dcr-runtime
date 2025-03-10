
<div align="center">
  <h1>Templates in Dynamic Conditional Response (DCR) Graphs.</h1>
</div> 

## Table of Contents

- [Table of Contents](#table-of-contents)
- [About](#about)
- [Getting Started](#getting-started)
  - [Docker Image](#docker-image)
    - [Prerequisites](#prerequisites)
    - [Example](#example)
  - [Building from Source](#building-from-source)
    - [Prerequisites](#prerequisites-1)
    - [Installation](#installation)
- [Usage](#usage)
- [License](#license)

## About 

This project is a interpreter that can create and execute DCR graphs with templates. 

The main goal of this prototype is to validate the usability and the expressiveness of the templates in DCR graphs and to see how it can be used in various scenarios.

Here is a quick sneak peek of how to express a DCR graph with templates in this project. This example is located in the [`examples`](./examples/) directory of the project.

```tdcr
tmpl reviewer_process(n: Number): Approve, Reject {
  (r: Reviewer)[n]
  !(rv: Review)[?: Unit]
  (ap: Approve)[?: Unit]
  (rj: Reject)[?: Unit]
  ;
  rv -->* ap
  rv -->* rj
  ap -->% rj
  rj -->% ap
} => ap, rj
;
# ---- Process ------------------------------------------ #

# Initialize pull request with N reviewers.
(pr: PullRequest)[?: Number]
;
pr -->> {
  (sa: SetApproved)[?: Unit]
  %(sr: SetChangesRequested)[?: Unit]
  ; 
  foreach i in @range(1, @trigger.value) {
    reviewer_process(n = i) => approve, reject
    ; 
    approve -->* sa
    reject -->% sa
    reject -->+ sr
  }
}
```

## Getting Started

Is possible to run this project two ways:
- Using the Docker image available in the `interpreter` directory. 
- Building the project from source.

The first option is the easiest way to run the project, but if you want to build the project from source, you can follow the instructions below.

### Docker Image

#### Prerequisites

In order to run the Docker image, you need to have Docker installed on your machine. You can follow the instructions in this [link](https://docs.docker.com/get-docker/) to install Docker.

After installing Docker, you can run the following command to build the Docker image in the `interpreter` directory of the project.

```bash
docker build -t template-dcr-interpreter .
```

After building the Docker image, you can run the following command to start the interpreter.

```bash
docker run -it -v $(pwd)/<path>:/app/<path> template-dcr-interpreter:
latest <path>/<file>.tdcr
```

Where `<path>` is the path to the directory that contains the `.tdcr` file and `<file>` is the name of the file that contains the DCR graph with templates.

#### Example

Here is an example of how to run the Docker image with the example file located in the `examples` directory of the project, in the `interpreter` directory.

```bash
docker run -it -v $(pwd)/../examples:/app/examples template-dcr-interpreter:latest examples/ex-reviewers/main.tdcr
```

---

### Building from Source

#### Prerequisites

In order to run this project you need to have the following installed:

- [OCaml](https://ocaml.org/docs/install.html) (base compiler version 5.1.1 or newer)
- [Opam](https://opam.ocaml.org/doc/Install.html) (2.1.2 or newer)

This are the minimum versions that are guaranteed to work and worked during development and tested with. 

In order to install OCaml and Opam, we suggest to follow the instructions in this [link](https://ocaml.org/docs/installing-ocaml).

#### Installation

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
opam install dune odoc menhir yojson ppx_yojson_conv cmdliner alcotest
```

And finally, you can build the project by running the following command.

```bash
make build
```

Or `dune build`. And you should be ready to go.

## Usage

To start the interpreter, you can run the following command in the `interpreter` folder and execute the following command:

```bash
dune exec tmpl_dcr -- <filename.tdcr>
```

Where `<filename>` is the name of the file that contains the DCR graph with templates.

You can see some examples in the [`examples`](./examples/) directory of the project to see how to write a DCR graph with templates.

**Note:** The file extension must be `.tdcr` in order to be recognized by the program.

---

After running the command, you should see the following prompt.

```
> 
```

From this prompt, you can run the following commands:

- **help**: Print the help message, displaying the available commands and their usage.
  ```
  > help
  ```
  
- **exit**: Exit the program and close the prototype.
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
    - `json`

## License

This project is licensed under the **MIT License**.

See [LICENSE](./LICENSE) for more information.
