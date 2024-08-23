# Feature of the NORKOST data

Survey data, 2 measurements per subject

Numerical measurements 

Potentially sparse - many columns do not have any records

Data measured on different days, uneven distribution across 7 days

Measured for a small subset of people, but interested in population

Inference for population level



## Weighting

Missing by design: small subset of subjects selected

Unit non-response: selected participants, but not answer

Item non-response: answered participants ignored some questions

(for this project, issues are first 2, not the 3rd)

We are only using respondent data, ignored the non-respondent. This could indicate a loss of information from the original sampling plan.



Address the issue of unbalanced sampling

- Days of a week
- Age group
- population 

#### Current approach

Use **inverse probability weighting**



#### Other ideas

IPW, estimate unit non-response probability with lr (equivalent to propensity)

- issue: we do not have any information on non-responders



## Population distribution

2 measurements per subject, wish to get some picture of the global distribution 



#### Current approach

Linear mixed model, each person as a group



#### Other ideas (not gonna push)

Bayesian approach





