# Norkost project progress log 

## Up to 2024.4.15

Decide to use SPADE 

discuss the needs

#### Analysis

Received Norkost3 data

installed SPADE on the cloud, tested how to use it with fake calcium data

## Up to 2024.4.29 

#### Methodology

Update on the reading of SPADE manual, found out that the result might not be useful

Decided to follow up with SPADE team, find out what they can provide

#### Analysis 

Receivd data with age information, used for stratification

simulation 

#### Plan for next step

Do a descriptive analysis for the existing food groups, figure out the frequency and range of values

## Up to 2024.5.27

#### Methodology

Received feedback from SPADE team, seems to be easy enough to do

- provide the data (in the form of a data.frame) with threshold per age /age group
- Lower: EAR (required, considered as lower)
- Upper: UL (upper limit)



#### Analysis

*Tested on bread data: how to use LMM from scratch - not needed for the update*

Descriptive analysis, understand food frequency and range of values, match the method in SPADE

- zero proportion varies, bread 0.04, meat 0.12, fruit 0.21, cheese 0.26, fish 0.55, egg 0.69

Tried in the simulated ca data

- provide EAR and UL, try to get some output in the 1-part model, seems to work



#### TO DO

- ask for reference data

- more information on the food intake units, some foods have very large values

- figure out whether we want nutrients (1-part model is enough), or food (2-part model needed for episodically consumed food)

  





## Up to 2024.6.10

### Reference thresholds

EAR: proportion of LESS than this threshold. 

When I tried to use 350 for meat, it's 100%

#### Fish

- above 300
- above 450

#### Meat

Above 350

mean 146, mean 120, max 1346, q90 321 zero 0.12



#### Whole grain

- above 90

#### Fruit and vegetables

- above 500
- Above 800

#### nuts

- above 20
- above 30



#### TO DO

- Clarify the thresholds, seems very high

- fruit and vegetable? put together? which variable is nuts?




## 2024.7.11

Send in preliminary results before summer



## 2024.8.22

#### Previous analysis

double check the results from before, along with visualization



#### Two new datasets:

- with supplements
- without supplements

Need to stratify by gender, find quartiles.

## 2024.8.23

Discussed the results, will keep the method as is

More analysis

- food: stratify by gender; unsalted nuts;
- nutrients: for both supplemented and unsupplemented foods, stratify by gender. Use narrowed variables. Report percentiles. For nutrients it is not necessary to compare with thresholds. 





