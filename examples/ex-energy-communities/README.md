# `ex-energy-communities`

In this example, we model the calculation of a consumer’s energy bill based on the selected plan.

## Example Usage 

After executing the cli with this example, you can type `view` to see the current state of the graph. This will show the following event:

```
> view
(ap:addProsumer)[?: { id: String, consumption: Number, production: Number, type: String }]
```

This event is responsible for adding a prosumer to the community. The input expression is a record with the following fields:
- `id`: the prosumer’s identification.
- `consumption`: the prosumer’s energy consumption.
- `production`: the prosumer’s energy production.
- `type`: the plan for the prosumer, which can be either `fixed` or `timed`.

To add a prosumer to the community, you can execute the event `ap` with the following expression:

```
> execute ap { id: "prosumer1", consumption: 100, production: 50, type: "fixed" }
Executed event ap with expression { id: "prosumer1", consumption: 100, production: 50, type: "fixed" }
```

and should see the following graph with the prosumer added:

```
> view -d
✓(ap:addProsumer)[?: { id: String, consumption: Number, production: Number, type: String }]
(cmc_1:calculateMonthlyCost)[?: { isPeakTime: Boolean, rate: Number }]
(p_0:ProsumerInfo)["prosumer1"]
(plan_2:Plan)[{ consumption: 100, production: 50 }]
```

The event `cmc` is responsible for calculating the monthly cost of a prosumer based on the selected plan, defined by the event `Plan` containing the prosumer’s consumption and production (in this case, `prosumer1` with consumption 100 and production 50 presented in the event with id `plan_0`).

To calculate the monthly cost of a prosumer, you can execute the event `calculateMonthlyCost` with the following expression:

```
> execute cmc_1 { isPeakTime: false, rate: 2 }
Executed event cmc_1 with expression { isPeakTime: false, rate: 2 }
```

and should see the following graph with the monthly cost calculated:

```
> view -d
✓(ap:addProsumer)[?: { id: String, consumption: Number, production: Number, type: String }]
(c_3:MonthlyCost)[{ cost: 100 }]
✓(cmc_1:calculateMonthlyCost)[?: { isPeakTime: Boolean, rate: Number }]
(p_0:ProsumerInfo)["prosumer1"]
(plan_2:Plan)[{ consumption: 100, production: 50 }]
```

The event `MonthlyCost` is responsible for storing the calculated monthly cost of the prosumer, which is 100 = (100-50)x2 in this case.

Consider adding more prosumers to the community and calculating their monthly costs to see how the graph evolves.

For instance, you can add a prosumer with id `prosumer2` with consumption 200 and production 150, and type `timed`:

```
> execute ap { id: "prosumer2", consumption: 200, production: 150, type: "timed" }
Executed event ap with expression { id: "prosumer2", consumption: 200, production: 150, type: "timed" }
```

view the graph:

```
> view -d
✓(ap:addProsumer)[?: { id: String, consumption: Number, production: Number, type: String }]
(c_3:MonthlyCost)[{ cost: 100 }]
✓(cmc_1:calculateMonthlyCost)[?: { isPeakTime: Boolean, rate: Number }]
(cmc_5:calculateMonthlyCost)[?: { isPeakTime: Boolean, rate: Number }]
(p_0:ProsumerInfo)["prosumer1"]
(p_4:ProsumerInfo)["prosumer2"]
(plan_2:Plan)[{ consumption: 100, production: 50 }]
(plan_6:Plan)[{ consumption: 200, production: 150 }]
```

and calculate the monthly cost of `prosumer2`:

```
> execute cmc_5 { isPeakTime: false, rate: 1 }
Executed event cmc_5 with expression { isPeakTime: true, rate: 1 }
```

view the graph:

```
> view -d
✓(ap:addProsumer)[?: { id: String, consumption: Number, production: Number, type: String }]
(c_3:MonthlyCost)[{ cost: 100 }]
(c_7:MonthlyCost)[{ cost: 40 }]
✓(cmc_1:calculateMonthlyCost)[?: { isPeakTime: Boolean, rate: Number }]
✓(cmc_5:calculateMonthlyCost)[?: { isPeakTime: Boolean, rate: Number }]
(p_0:ProsumerInfo)["prosumer1"]
(p_4:ProsumerInfo)["prosumer2"]
(plan_2:Plan)[{ consumption: 100, production: 50 }]
(plan_6:Plan)[{ consumption: 200, production: 150 }]
```

The monthly cost of `prosumer2` is 40 = (200-150)x1x0.8. 
Consider adding more prosumers and calculating their monthly costs to see how the graph evolves.

## Other Examples

- [`ex-bad-typing`](../ex-bad-typing/README.md): see how the interpreter handles type errors.
- [`ex-reviewers`](../ex-reviewers/README.md): see how to model a review process for a pull request.
- [`ex-pokemon`](../ex-pokemon/README.md): see how to simulate a Pokémon battle.