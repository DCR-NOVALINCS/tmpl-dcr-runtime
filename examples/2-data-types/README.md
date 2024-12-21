# `2-data-types`

In the previous example, we saw that events can contain data of different types. In this example, we will explore the various data types that can be used in events.

## Data Types

The data types that can be used in events are:

| Type                                           | Description                                                    |
| ---------------------------------------------- | -------------------------------------------------------------- |
| `Unit`                                         | Represents the absence of a value.                             |
| `Boolean`                                      | Represents a boolean value.                                    |
| `Number`                                       | Represents an integer value.                                   |
| `String`                                       | Represents a string value.                                     |
| `List[<A>]`                                    | Represents a list of values of type `<A>`.                     |
| `{prop0: <T0>, prop1: <T1>, ..., propN: <TN>}` | Represents a record of keys `propI` and values of type `<TI>`. |

## Example

The following example demonstrates the usage of these data types:

```
(n: NumberEvent)[0 + 1]
(b: BooleanEvent)[true AND 0 > 1]
(s: StringEvent)["Hello world!"]
(l: ListEvent)[[1, 2, 3]]
(r: RecordEvent)[{a: 1, b: true}]
```

| Id  | Type                        |
| --- | --------------------------- |
| `n` | `Number`                    |
| `b` | `Boolean`                   |
| `s` | `String`                    |
| `l` | `List[Number]`              |
| `r` | `{ a: Number, b: Boolean }` |

**Next Examples**

- [`3-marking`](../3-marking/README.md): Explore some important properties of events.