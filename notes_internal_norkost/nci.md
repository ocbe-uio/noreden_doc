# NCI method: usual dietary intakes

https://prevention.cancer.gov/research-groups/biometry/measurement-error-impact/measurement-error-nutritional-epidemiology

usual intake: average long-term intake

24h recall data

- Estimate distribution of usual intake for population / subpopulation
- assess effects on non-dietary covraiates on usual consumption
- address some measurement error



#### Assumption

Usual intake = prob of consumption on a given day * average consumption on a consumption day



Episodically-consumed: two part model (whether you eat, how much you eat)

- Prob consumption: logistic regression with person-specific random effect
- Consumption day amount: linear regression (transformed scale), person-specific random effect

correlated person-specific effect 





<span style= 'color:red'>Questions</span>

subject-level random intercept. wouldn't it just be the average between 2 measurements?

probability of consuming one food: could it be just 0, 0.5 or 1?





# Measurement error in epidemiology

Long-term self report: frequency of food over 3m, 6m 1yr

Short-term self report: 24h recall, all food consumed 

- regularly consumed
- episodically
- never

Biomarkers





### Model

**linear measurement error model**, intercept is a random effect across individuals

Yi = a0 + a0i + bXi + ei

a0: population level 

a0i: individual level (person specific bias)

bXi: regressor

































