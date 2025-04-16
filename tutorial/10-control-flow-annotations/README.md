# `10-control-flow-annotations`

In this tutorial, we delve into control flow annotations in DCR Graphs. These annotations are instrumental in defining sub-graph applications at runtime, either within a constraint or across multiple instances of a sub-graph.

There are two primary types of control flow annotations:

- **Conditional**: Executes one of two sub-graphs based on a specified condition. If the condition evaluates to `true`, the first sub-graph is executed; otherwise, the second sub-graph is instantiated.
- **Loop**: Repeatedly instantiates a sub-graph based on the values in a list.

## Conditional 

The following example demonstrates a conditional annotation. It executes one of two sub-graphs depending on whether a condition is met:

```dcr
(a: A)[?]
;
if true {
    (b: B)[?]
    ;
    b -->+ a
} else {
    (c: C)[?]
    ;
    c -->+ a
}
```

## Loop

The next example illustrates a loop annotation. It executes a sub-graph multiple times, iterating over the values in a list:

```dcr
(a: A)[?]
;
foreach i in [1, 2, 3] {
    (b: B)[?]
    ;
    b -[i > 2]->+ a
}
```

Where `i` is the loop variable that takes on the values `1`, `2`, and `3` in each iteration. The sub-graph containing event `b` is instantiated for each value of `i`, and the relation between `b` and `a` is established based on the condition `i > 2`.
