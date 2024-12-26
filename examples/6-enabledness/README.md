# `6-enabledness`

In DCR Graphs, the enabledness of an event is a way to determine if an event can be executed at a given point in time. 

An event is enabled if the following conditions are met:
- The event is included.
- Every event that has a condition relation (`-->*`) to the said event is executed.
- Every event that has a milestone relation (`--><>`) to the said event is not pending.

With this in mind, in this example (also available in [this file](main.tdcr)):

```
(a: A)[?]
%(b: B)[?: Number]
!(c: C)[?: String]
(d: D)[?: {n: Number, b: Boolean}]

a -->+ b
a *--> c
b -->* c
c --><> b
```

when we execute the command `view` in the cli, we get the following output:

```
(a:A)[?]
(d:D)[?: { n: Number, b: Boolean }]
```

This is because:
- `a` and `d` are included and have no conditions or milestones relations.
- `b` is excluded, as it has a condition relation to `c`, which is not executed.
- `c` is pending and has a milestone relation to `b`.

This is a simple example, but it shows how the enabledness of an event can affect the marking of the graph.

**Next examples**

- [`7-templates`](../7-templates/README.md): see how to create templates for DCR Graphs.