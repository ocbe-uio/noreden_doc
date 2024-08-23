> Use SPADE software for intake estimation

#### Installation

On Posit Cloud. Failed on local machine.



# functionality

### daily intake (1 part)

foods consumed by almost everyone: e.g. bread; or nutrient

the model is simpler, because only intake needs to be estimated, not the frequency.

### episodic intake (2 part)

greater number of zeros, intake frequencey and amount to be estimated

e.g. potatoes, meat, fish

All subjects are assumed to be potential consumers, e.g. meat eater





<span style = 'color:red'>find out in our data about food frequency</span>

- stratified by 0, 1, 2 measurements



# 1 part

### Input data (1 part): food consumption

At least 2 positive intakes per person 

- `id`
- `age` 
- `sex` 1M 2F
- intake data (no specific name required)



### Function

```{r}
f.spade(
  frml.ia = ca ~ fp(age), # model for intake amounts,
  frml.if = "no.if", # "no.if" = no zeroes, no intake frequencies
  data = d,
  min.age = 18,
  max.age = 65,
  sex.label = "female",
  seed = 29062020,
  spade.output.path = "0_SPADE_OUTPUT"
)
```





### Model

transform outcome using box-cox

by default, one part model is 

`intake.trans ~ I(age/100) + (1|id)`

after model estimated, back transform

### Result

within individual variance, between individual variance, ratio

weighted sample mean intake, overall mean of habitual intake 

age groups



# 2 part 

### Model

- probability of having positive intake on a day (mean p per age) with cubic splines
  - `cs(age)`: cubic spline with logistic beta-binomial model
- model habitual amount consumed on consumption days
  - at least 50 participants with 2 or more intakes is required
- combine the two parts together 

COMPLICATION: never consumers, need to be modeled separately then put back



### Function

```{r}
f.spade(
frml.ia = potato ~ fp(age), # formula for intake amounts (ia)
frml.if = potato ~ cs(age), # formula for intake frequencies (if)
data = DNFCSmanual,
min.age = 1,
max.age = 79,
sex.label = "female",
seed = 10,
spade.output.path = "0_SPADE_OUTPUT")
```



# Bootstrap



It does provide bootstrap percentiles, and it is the distribution of mean (habitual)

This does not help with estimating the proportion itself





# Thresholds



Manual 3.3.5 - 6, where 2.1.2 describes how the data for cutoff should look like

EAR: estimated average requirement, proportion of population with intakes less than this value

UL: tolerable upper intake, proportion greater than this cutoff 

AI: adequate intake (probably we don't care)

require

- Column with age
- column with cutoff points for each nutrient

optional: sex 



````
> head(ref_ear)
  age  ca
1  18 900
2  19 900
3  20 900
4  21 900
5  22 900
6  23 900
````

provide this to the spade function: `EAR.names = 'ref_ear', UL.names = 'ref_ul'`

```R
f.spade(
  frml.ia = ca ~ fp(age), # model for intake amounts,
  frml.if = "no.if", # "no.if" = no zeroes, no intake frequencies
  data = d,
  min.age = 18,
  max.age = 65,
  sex.label = "female",
  seed = 29062020,
  EAR.names = 'ref_ear', # provide the reference 
  UL.names = 'ref_ul',
  spade.output.path = "0_SPADE_OUTPUT"
)
```





