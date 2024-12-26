# `4-control-relations`

We have seen how to create events, its types and its data. 
Now we will see how to affect the marking of the events using relations, more specifically the control relations.

In DCR Graphs, there are five control relations that can affect the marking of events:

| Relation  | Representation                                                      | Description                                                  |
| --------- | ------------------------------------------------------------------- | ------------------------------------------------------------ |
| Include   | $a \color{forestgreen}\rightarrow\!\!+\space \color{default} b$     | When event `a` is executed, event `b` becomes included.      |
| Exclude   | $a \color{red}\rightarrow\!\!\% \space \color{default} b$           | When event `a` is executed, event `b` becomes excluded.      |
| Response  | $a \color{cyan}\space\bullet\!\!\rightarrow \color{default} b$      | When event `a` is executed, event `b` becomes pending.       |
| Condition | $a \color{orange}\rightarrow\!\!\bullet \space \color{default} b$   | Event `a` must be executed before event `b` can be executed. |
| Milestone | $a \color{magenta}\rightarrow\!\!\diamond \space \color{default} b$ | Event `a` must not be pending for event `b` to be executed.  |

**Next examples**

- [`5-spawn-relation`](../5-spawn-relation/README.md): see how to create new sub-graphs when an event is executed.
- [`6-enabledness`](../6-enabledness/README.md): see what events can be executed at any time of the execution.
