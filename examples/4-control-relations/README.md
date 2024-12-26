# `4-control-relations`

We have seen how to create events, its types and its data. 
Now we will see how to affect the marking of the events using relations, more specifically the control relations.

In DCR Graphs, there are five control relations that can affect the marking of events:

| Relation  | Representation                                           | Description                                                  |
| --------- | -------------------------------------------------------- | ------------------------------------------------------------ |
| Include   | `a` <span style="color:forestgreen">**-->+**</span> `b`  | When event `a` is executed, event `b` becomes included.      |
| Exclude   | `a` <span style="color:red">**-->%**</span> `b`          | When event `a` is executed, event `b` becomes excluded.      |
| Response  | `a` <span style="color:deepskyblue">**\*-->**</span> `b` | When event `a` is executed, event `b` becomes pending.       |
| Condition | `a` <span style="color:orange">**-->\***</span> `b`      | Event `a` must be executed before event `b` can be executed. |
| Milestone | `a` <span style="color:magenta">**-->\<\>**</span>  `b`  | Event `a` must not be pending for event `b` to be executed.  |

**Next examples**

- [`5-spawn-relation`](../5-spawn-relation/README.md): see how to create new sub-graphs when an event is executed.
- [`6-enabledness`](../6-enabledness/README.md): see what events can be executed at any time of the execution.
