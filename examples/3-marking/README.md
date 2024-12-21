# `3-marking`

One of the properties of the events is the marking. 

An marking tells the current state of the event. 

The event marking contains the following information:
- `included`, i.e the event is included.
- `pending`, i.e the event is pending from execution.
- `executed`, i.e the event is already executed.

This information is important to see which events can be executed in anytime of the execution (see later in [here](../5-enabledness/README.md)).

You can set the default marking of the events in the modelling process.
Consider the following example:

```
(a: A)[?]
%(b: B)[?: Number]
!(c: C)[?: String]
```

**Note:** if is not specified, the default marking of an event is included, not pending and not executed. 

| Id  | Included | Pending | Executed |
| --- | -------- | ------- | -------- |
| `a` | true     | false   | false    |
| `b` | false    | false   | false    |
| `c` | true     | true    | false    |

**Next Examples**

- [`4-relations`](../4-control-relations/README.md): see how to affect the marking of other events when executing one event. 
