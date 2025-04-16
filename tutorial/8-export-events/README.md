# `8-export-events`

*Building on the previous tutorial ([`7-templates`](../7-templates/README.md)), this section delves into export events and their usage in DCR Graphs.*

Consider the following example (also available in [this file](main.tdcr)):

```dcr
tmpl t1(n: Number, a: A) {
    (b: B)[?]
    ;
    b -[n > 0]->* a
}

tmpl t2(a: A) {
    t1(n = 1, a = a)
}

tmpl t3(): A {
    (a: A)[?]
    ;
    t2(a = a)
} => a
;
...
```

Here, we define templates `t1` and `t2` as before, but now we introduce a new template, `t3`. Template `t3` uses `t2` and exports the event `a` using the `=>` operator. Exporting an event allows it to be accessed and utilized in the DCR Graph when `t3` is instantiated.

### Defining Exported Events

To specify which events in a template's sub-graph are exported, you must declare the exported event labels in the template definition. These labels must meet the following criteria:

1. Exported event identifiers must be unique within the template.
2. Identifiers must correspond to event labels defined in the DCR Graph.

While the same identifier can be reused for different events across different templates, it cannot be reused for multiple events within the same template.

Exported events are particularly useful for extending workflows by adding control-flow relations to the exported events. For example:

### Next examples

- [`9-template-applications`](../9-template-applications/README.md): see how to use templates in DCR Graphs.
- [`10-control-flow-annotations`](../10-control-flow-annotations/README.md): see how to use control flow annotations in DCR Graphs.