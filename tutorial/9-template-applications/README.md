# `9-template-applications`

Building on the previous example, we can now use the template `t3` to construct a more intricate DCR Graph. Here's an example:

```dcr
...
;
t3() => a0
;
a0 -->% a0
```

In this snippet, the event `a` from the template `t3` is remapped to `a0` within the DCR Graph. This remapping allows `a0` to behave as a standard event in the graph while still serving as an exported event from `t3`. This feature facilitates the seamless integration of template events into larger workflows.

When you execute the command `view -rd` in the CLI, the output will look like this:

```dcr
> view -rd
(a0:A)[?]
(b_0:B)[?]
b_0 -[1 > 0]->* a0
```

This output illustrates the remapped event `a0` and the event `b_0`, which is an instance of the template `t1`. The relationship between `b_0` and `a0` is also displayed, with the value of `n` set to `1`. This example demonstrates how templates can be leveraged to create complex DCR Graphs with reusable components.

## Next Steps

- [`10-control-flow-annotations`](../10-control-flow-annotations/README.md): Learn how to use control flow annotations in DCR Graphs.