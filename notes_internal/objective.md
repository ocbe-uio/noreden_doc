energy composition

- energy
- macronutrients

epi

- follow fbdg guideline (except red meat recommendation)
- fruit, portions of low fat, fish, whole grain products

Acceptability 

- in terms of diet weight, within common range (e.g. not 5 apples a day)

cost 





## Food groups

| Food groups    | Energy  | Protein | Fat    | Carb   | GHGE   |
| -------------- | ------- | ------- | ------ | ------ | ------ |
| Bread          | 10.6956 | 0.0912  | 0.0302 | 0.4413 | 0.0011 |
| Vegetables     | 1.5653  | 0.0149  | 0.0084 | 0.0498 | 0.0010 |
| Fruit, berries | 2.7289  | 0.0076  | 0.0041 | 0.1341 | 0.0007 |
| Meat           | 7.7432  | 0.1800  | 0.1211 | 0.0113 | 0.0110 |
| Fish           | 6.0863  | 0.1698  | 0.0748 | 0.0245 | 0.0031 |
| Milk, yogurt   | 1.9797  | 0.0359  | 0.0111 | 0.0559 | 0.0014 |





### Objective 

minimize deviation from current diet

(x1 - 175.4)^2 + 

(x2 - 154.6)^2 + 

(x3 - 171.5)^2 + 

(x4 - 151.1)^2 + 

(x5 - 69.5)^2 + 

(x6 - 306.1)^2



### Contraints, 5 categories

(intake g 3681.2 )

Energy kj 9314

protein g 97.7

fat g 86.3

carbs g 235.1

GHGE <= 4.7

| Constraint | Energy | Protein | Fat  | Carb  | GHGE |
| ---------- | ------ | ------- | ---- | ----- | ---- |
| Lower      | 9000   | 55      | 61.8 | 250   | 0    |
| Upper      | 10000  | 111.5   | 98.8 | 334.6 | 4.7  |



# Formulation (QP)

### minimize 

$(x_1 - 175.4)^2 + (x_2 - 154.6)^2 + ...  $

in total 25 food groups

### subject  to 

energy 

$e_1 x_1 + e_2 x_2 + ... e_{25}x_{25} >9000$

$e_1 x_1 + e_2 x_2 + ... e_{25}x_{25} <10000$

protein 

$p_1 x_1 + p_2 x_2 + ... p_{25}x_{25} >55$

$p_1 x_1 + p_2 x_2 + ... p_{25}x_{25} <111.5$

fat

$f_1 x_1 + f_2 x_2 + ... f_{25}x_{25} >61.8$

$f_1 x_1 + f_2 x_2 + ... f_{25}x_{25} <98.8$

carb

$c_1 x_1 + c_2 x_2 + ... c_{25}x_{25} >250$

$c_1 x_1 + c_2 x_2 + ... c_{25}x_{25} <334.6$

ghge

$g_1 x_1 + g_2 x_2 + ... g_{25}x_{25} >0$

$g_1 x_1 + g_2 x_2 + ... g_{25}x_{25} <4.7$

and non negativity 

$x_1, ...,  x_{25}>=0 $







