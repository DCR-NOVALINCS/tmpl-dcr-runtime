# `ex-pokemon`

This example models a simple Pokémon battle system. The example has the following events:

- `addPokemon`: Adds a Pokémon to the battle.
- `Battle`: Starts a battle between two Pokémon.

The events have the following data types:

- `addPokemon`: `{name: String, type: String}`: The name and type of the Pokémon. Note that this example only covers the types `Fire`, `Water`, and `Grass`.
- `Battle`: `{pokemon1: Pokemon, pokemon2: Pokemon}`: The event references of the Pokémon.

A `Pokémon` event has the data type `{name: String, type: String}`. Is possible to create a Pokemon with the template `pokemon_process`.

The graph is defined in the file [main.tdcr](main.tdcr).

## Example Usage

After executing the cli with the file `main.tdcr`, the following output is shown:

```
> view -d
(ap:addPokemon)[?: { name: String, type: String }]
(b:Battle)[?: { p1: Pokemon, p2: Pokemon }]
```

This output shows the two events available in the graph: `addPokemon` and `Battle`.

To add a Pokémon to the battle, you can execute the event `addPokemon` with the following expression:

```
> execute ap { name: "Charmander", type: "Fire" }
Executed event ap with expression { name: "Charmander", type: "Fire" }

> execute ap { name: "Squirtle", type: "Water" }
Executed event ap with expression { name: "Squirtle", type: "Water" }
```

view the graph with the Pokémon added:

```
> view -d
✓(ap:addPokemon)[?: { name: String, type: String }]
(b:Battle)[?: { p1: Pokemon, p2: Pokemon }]
(p_0:Pokemon)[{ name: "Charmander", type: "Fire" }]
(p_1:Pokemon)[{ name: "Squirtle", type: "Water" }]
```

The event `Battle` is responsible for starting a battle between two Pokémon. To start a battle, you can execute the event `Battle` with the following expression:

```
> execute b { p1: p_0, p2: p_1 }
Executed event b with expression { p1: p_0, p2: p_1 }
```

and should see the following graph with the battle result:

```
> view -d
✓(ap:addPokemon)[?: { name: String, type: String }]
✓(b:Battle)[?: { p1: Pokemon, p2: Pokemon }]
(bl_2:BattleLog)[{ winner: "Squirtle" }]
(p_0:Pokemon)[{ name: "Charmander", type: "Fire" }]
(p_1:Pokemon)[{ name: "Squirtle", type: "Water" }]
```

The event `BattleLog` is responsible for storing the winner of the battle. In this case, `Squirtle` wins the battle against `Charmander`.

Consider adding more Pokémon to the battle and starting more battles to see how the graph evolves.

## Other Examples

- [`ex-energy-communities`](../ex-energy-communities/README.md): see how to calculate a consumer’s energy bill based on the selected plan.
- [`ex-reviewers`](../ex-reviewers/README.md): see how to manage reviewers for a pull request.