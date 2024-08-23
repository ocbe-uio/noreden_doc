# Goal

#### (Bird2023)

Proportion of population with habitual intake 

- below EAR (estimated average requirement)
- above UL (upper level)



Nutrients: calcium, iron, zinc, vA, vB6, folate, vD, vE

Data

- Demographics (age, gender)
- Food supplement intake
- Season (of the year)

24h recall, with interval of 4 weeks

# SPADE: statistical program to assess dietary exposure

1 part model: computes base, base+ diet

- model dietary components consumed on a daily basis by almost al participants
- in the study almost all subjects have non-zero intake on 2 observed days
  - transform (box-cox) to obtain normal distribution
  - model transformed intake as a function of age, estimate within- between person variances, obtain shrunken distribution on the transformed scale
  - transform back  to original scale, eliminate within-person variance

2 part model: episodically

- some have 2 measurements, some have 1, some 0
- model probability of consumption as a function of age, with beta-binomial model
- omit those with zero intake

3 part model

- total habitual nutrient intake from foods and dietary supplement

  

# VKM mixed model

Chapter 7.5

#### fixed effect

y_transformed ~ sex + age + education + region + weekday + month

#### random effect 

subject 

#### Simulate habitual intake and chronic exposure distributions

- fit model: coefficients for fixed effects, var-cov matrics for random effects
- Simulate 365*100 daily observation for each subject  - 100 years daily data
- reverse transformation
- take average. this reduced within-subject variation
- compute 100 weighted population distribution, controlling for demographic











