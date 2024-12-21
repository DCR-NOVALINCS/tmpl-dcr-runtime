# `0-hello`

This is a simple example to show how to initialize the cli tool and see all the available commands.

## Usage 

After building the project (see [here](../../README.md)), you can run the following command to execute the cli tool in the [`interpreter`](../../interpreter/) folder:

```bash
dune exec tmpl_dcr -- ../examples/0-hello/main.tdcr
```

This will show the following output:

```
> 
```

You can type `help` to see all the available commands:

```
> help
Available Commands:
- export <FILENAME>: Creates a file named <FILENAME> with a textual representation of the current state of the graph.
- execute <EVENT_ID> <EXPR_STRING>: Executes the event <EVENT_ID> with the expression <EXPR_STRING> if needed.
- view : Views the current state of the graph.
- exit : Exit the program.
```

You can type `view` to see the current state of the graph. This will show the following output:

```
(hw: HelloWorld)["Hello, World!"]
```

We will see more about the graph in the next examples.

## Next Examples

- [`1-events`](../1-events/README.md): Events and its types.
- [`2-data-types`](../2-data-types/README.md): Data types of the events and expressions.


