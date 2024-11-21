Attaching a copy of the code with annotations. Also adding a file that includes the input files I've used for one of my analyses - I have copies of the same input files with some small changes made for each of the 4 diet scenarios I've run. Within each scenario there are **9 files that represent the different step-wise carbon footprint constraints (0%, -5%, -10%, -15%, -20%)** etc. In each of these files you'll find the code I ran for that specific iteration, along with the input data, nutrient constraints, nutrient reference values (for comparing results to), and realism constraints. Just adding these so you can see the way the data has been set up. 

One important element in Rcplex is that the constraint matrix needs to have the same dimensions vertically and horizontally. So if there are 20 rows of nutrient constraints, there need to be 20 columns of nutrient input data, etc.

### Aims

A) The code will be more understandable for someone who is seeing it for the first time. So sort an 'insert X here' type of vibe with explanation. And with **illustrations** of how the input data/files need to be set up.

b) That we can develop the code further so it can be suitable also for **individual level optimization**, using individual food intake and nutrient needs instead of basing everything on the population level average. This will involve writing the **loop code** so the model runs continuously through the list of >1000 individuals, and troubleshooting hiccups the model might meet like people whose diets can't be optimized etc. Also need to set it up so that the model knows which nutrient and realism constraints to use -- since the realism constraints are based on individual level food intake, and the nutrient constraints will be based on a combination of gender and age. 





# Notes



> 1. different settings: which `.xlsx` have different values?
> 2. change col names: I suggest source it from a table so it's more readable
> 3. note that size of Amat: doesn't have to be a square 53 by 53, but need to match the number of nutrients
> 4. figure out the metrics after computation, what kind of reporting 







9 folders for each setting 

- meat reduction
- meat redistribution
- meat max
- meat current



#### Input.xlsx

53 food groups, inputs should be the same across all settings 

why is it called **constraint matrix**?



#### Nutrient dir

2 columns, one per nutrient (not food)

- Dir: E, L, R (equal, less equal, greater equal)
- rhs

#### Realism

53 food groups, mean, 10%, 95% of current intake (based on survey)

- mean
- 10%
- 95%





## Objective function

**obs** here is the same as first column in `realism`

if we have **53 food groups, 10 nutrients**: 

dimension requirement:

objective: min 1/2 x_transpose Qmat x + c_transpose x

- x, x_transpose: vector, 53 by 1; 1 by 53
- Qmat (diagnal matrix), 53 by 53
- c, c_transpose: 53 by 1; 1 by 53



inequality constraint: Amat x <= b

- Amat: 10 by 53
- x: 53 by 1
- b: 10 by 1



constraint on x bounds

- lb: 53 by 1
- ub: 53 by 1





## Result processing



- Nutrients
- diet
- diet departure
- ruminant meat departure
- red meat departure
- total meat departure



