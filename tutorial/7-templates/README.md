# `7-templates`

As the name suggests, we will explore how to create templates for DCR Graphs.

## Definition

A template in DCR Graphs is a sub-graph with free names that can be instantiated with different values. This allows for the creation of reusable components that can be used in different contexts.

In order to define a template, we use the `tmpl` keyword, followed by the name of the template and its parameters. The parameters can be of any type. The body of the template is defined in the same way as a regular DCR Graph, but with the parameters replaced by their values.
Lastly, we define the exported events of the template (explained in detail in this [tutorial](../8-export-events/README.md)) with the `=>` operator.

## Example

Take the following example (also available in [this file](main.tdcr)):
```dcr
tmpl t1(n: Number, a: A) {
    (b: B)[?]
    ;
    b -[n > 0]->* a
}
```

This defines a template `t1` with two parameters: `n` of type `Number` and `a` with label `A`. The body of the template is a DCR Graph with one event `b`, which has a condition relation to `a`. The template can be instantiated with different values for `n` and `a`, allowing for the creation of reusable components.

By itself, this template does not do much. However, we can apply it to a DCR Graph to create a more complex graph.

## Next examples

- [`8-export-events`](../8-export-events/README.md): see how to export events from a template.
- [`9-template-applications`](../9-template-applications/README.md): see how to use templates in DCR Graphs.
- [`10-control-flow-annotations`](../10-control-flow-annotations/README.md): see how to use control flow annotations in DCR Graphs.