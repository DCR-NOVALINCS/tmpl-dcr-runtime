# `4-control-relations`

We have seen how to create events, their types and their data. 
Now we will see how to affect the events' marking using relations, specifically the control relations.

In DCR Graphs, five control relations can affect the marking of events:

| Relation  | Representation | Description                                                  |
| --------- | -------------- | ------------------------------------------------------------ |
| Include   | `a` -->+ `b`   | When event `a` is executed, event `b` becomes included.      |
| Exclude   | `a` -->% `b`   | When event `a` is executed, event `b` becomes excluded.      |
| Response  | `a` *--> `b`   | When event `a` is executed, event `b` becomes pending.       |
| Condition | `a` -->* `b`   | Event `a` must be executed before event `b` can be executed. |
| Milestone | `a` --><> `b`  | Event `a` must not be pending for event `b` to be executed.  |

**Next examples**

- [`5-spawn-relation`](../5-spawn-relation/README.md): see how to create new sub-graphs when an event is executed.
- [`6-enabledness`](../6-enabledness/README.md): see what events can be executed at any time of the execution.
