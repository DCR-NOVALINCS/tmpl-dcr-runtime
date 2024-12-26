# `5-spawn-relation`

In DCR Graphs, the spawn relation allows the creation of new sub-graphs when an event is executed. Once the event is executed, the sub-graph is incorporated into the main graph.

### Example

Consider the following example (also available in [this file](main.tdcr)):

```
(a: A)[?]

a -->> {
    (b: B)[?: Number]
    !(c: C)[?: String]

    b -->* c
}
```

#### Initial State

When first initialized, the graph appears as follows:

```
(a: A)[?]
```

**Note:** To view all the relations in the graph, use the `--relation` flag (or `-r`) with the `view` command.

#### After Execution

When the command `execute a` is run in the CLI, the output is:

```
(a:A)[?]
(b_0:B)[?: Number]
!(c_1:C)[?: String]

b_0 -->* c_1
```

This output shows the main graph combined with the sub-graph created by the spawn relation.

**Next Examples**

- [`6-enabledness`](../6-enabledness/README.md): see what events can be executed
- [`7-templates`](../7-templates/README.md): see how to create templates.
