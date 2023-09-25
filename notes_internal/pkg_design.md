# Workflow 

## Specify food information

Purpose: use the intake and nutrient/ghge to compute

- intake (unit)
- nutrient (per unit)
- ghge (per unit)





<span style = 'color:orange'>Requirement </span>

- if want flexibility to add food later on, should be expecting both one food, and multiple foods 
- also flexibility on categories (carbs, protein, ghge, ...)



## Run optimization with target constraints

Purpose: find a solution

This part consists 4 parts

- set objective (qp on food intake)
- set constraints (upper and lower limit on food * contrib per intake)
- lower and upper bound for food intake
- run `nloptr::nloptr()`



<span style = 'color:orange'>Requirement</span>

- the first 3 steps need to be flexible



## Result collection

Purpose: quickly identify if the result is desirable



<span style = 'color:orange'>Requirement</span>

- Runtime 

