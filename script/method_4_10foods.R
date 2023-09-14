# use 10 selected foods, wider bounds
# 
library(data.table)
library(readxl)

# food data
foods <- read.csv('./data_public/foods.csv')
setDT(foods)
foods


# take 10 foods
fd <- foods[food %in% c('Bread', 
                        'Potatoes',
                        'Vegetables', 
                        'Fruit, berries',
                        'Red meat', 
                        'Fish',
                        'Eggs',
                        'Milk, yoghurt',
                        'Cheese',
                        'Sugar, sweets'
)]
fd


# constraints for food (this is used in opt, for possible values to take)
const_realistic <- read_excel("data_example/Realism constraints Rcplex 11.09.xlsx")
const_realistic <- setDT(const_realistic)
const_realistic
# change colnames

setnames(const_realistic, 'Foodgroup', 'food')
setnames(const_realistic, 'Mean10MJ', 'intake_mean')
setnames(const_realistic, '0.1xmean10MJ', 'intake_lwr')
setnames(const_realistic, '95thpercentile10MJ', 'intake_upr')


# put these two together
fd <- merge.data.table(fd, const_realistic, by = 'food')

fd
#write.csv(fd, file = 'documentation/data/foods_0914.csv', row.names = F)


contrib_pergram <- fd[, .(energy, protein, fat, carbs, sugar, alcohol, ghge)]
contrib_pergram

# note: intake from the food.csv is slightly different from the new file
current_diet <- fd$intake
current_diet


# 10 foods contribution (maximum)
const_max_10foods <- t(as.matrix(current_diet)) %*% as.matrix(contrib_pergram)
const_max_10foods

# exclude sugar, alcohol
const_max_10foods <- const_max_10foods[, c('energy', 'protein', 'fat', 'carbs', 'ghge')]


# target constraint on energy and nutrients
# set lower to be 0.9; upper remain the current max
const_lwrupr <- rbind(const_max_10foods*0.9, const_max_10foods*1)
rownames(const_lwrupr) <- c('lwr', 'upr')
const_lwrupr <- data.table(const_lwrupr)
const_lwrupr


# standardize ----
# standardize food contribution per gram
# find sd for each category: energy, protein.. ghge
# divide by these coef
contrib_pergram <- contrib_pergram[, c('energy', 'protein', 'fat', 'carbs', 'ghge')]

sd_coef <- apply(contrib_pergram, MARGIN = 2, sd)

contrib_pergram_std <- sweep(contrib_pergram, MARGIN = 2, 1/sd_coef, FUN = '*')
contrib_pergram_std

# standardize constraint
# test the previous constraint
const_lwrupr
const_lwrupr_std <- sweep(const_lwrupr, MARGIN = 2, 1/sd_coef, FUN = '*')
const_lwrupr_std



# reduce ghge ----
# this should be tested out: different reduction percentage
# 80% is probably too progressive
cstr <- copy(const_lwrupr_std)
cstr$ghge <- cstr$ghge * 0.85 # reduce to 0.9
cstr



# define objective ----
# 10 foods, therefore 10 elements
# minimize the deviation from the current intake, on 10 foods
objective <- function(x)
{
  return ( (x[1]- current_diet[1])^2 + 
             (x[2]- current_diet[2])^2 + 
             (x[3]- current_diet[3])^2 + 
             (x[4]- current_diet[4])^2 + 
             (x[5]- current_diet[5])^2 + 
             (x[6]- current_diet[6])^2 + 
             (x[7]- current_diet[7])^2 + 
             (x[8]- current_diet[8])^2 + 
             (x[9]- current_diet[9])^2 + 
             (x[10]- current_diet[10])^2)
}



# define constraints ----
contrib_pergram_std

# define the inequality constraints
inequalconstr <- function (x) {
  
  cps <- contrib_pergram_std
  
  constr <- c(
    # energy
    - x[1]*cps$energy[1] - x[2]*cps$energy[2] - x[3]*cps$energy[3] - x[4]*cps$energy[4] - x[5]*cps$energy[5] - x[6]*cps$energy[6] -  x[7]*cps$energy[7] - x[8]*cps$energy[8] - x[9]*cps$energy[9]- x[10]*cps$energy[10] + cstr$energy[1], # lower
    x[1]*cps$energy[1] + x[2]*cps$energy[2] + x[3]*cps$energy[3] + x[4]*cps$energy[4] + x[5]*cps$energy[5] + x[6]*cps$energy[6] + x[7]*cps$energy[7] + x[8]*cps$energy[8] + x[9]*cps$energy[9] + x[10]*cps$energy[10] - cstr$energy[2], # upper
    
    # protein
    # - x[1]*cps$protein[1] - x[2]*cps$protein[2] - x[3]*cps$protein[3] + cstr$protein[1],
    # x[1]*cps$protein[1] + x[2]*cps$protein[2] + x[3]*cps$protein[3] - cstr$protein[2],

    - x[1]*cps$protein[1] - x[2]*cps$protein[2] - x[3]*cps$protein[3] - x[4]*cps$protein[4] - x[5]*cps$protein[5] - x[6]*cps$protein[6] -  x[7]*cps$protein[7] - x[8]*cps$protein[8] - x[9]*cps$protein[9]- x[10]*cps$protein[10] + cstr$protein[1], # lower
    x[1]*cps$protein[1] + x[2]*cps$protein[2] + x[3]*cps$protein[3] + x[4]*cps$protein[4] + x[5]*cps$protein[5] + x[6]*cps$protein[6] + x[7]*cps$protein[7] + x[8]*cps$protein[8] + x[9]*cps$protein[9] + x[10]*cps$protein[10] - cstr$protein[2], # upper
    
    # fat
    # - x[1]*cps$fat[1] - x[2]*cps$fat[2] - x[3]*cps$fat[3]+ cstr$fat[1],
    # x[1]*cps$fat[1] + x[2]*cps$fat[2] + x[3]*cps$fat[3] - cstr$fat[2],
    
    - x[1]*cps$fat[1] - x[2]*cps$fat[2] - x[3]*cps$fat[3] - x[4]*cps$fat[4] - x[5]*cps$fat[5] - x[6]*cps$fat[6] -  x[7]*cps$fat[7] - x[8]*cps$fat[8] - x[9]*cps$fat[9]- x[10]*cps$fat[10] + cstr$fat[1], # lower
    x[1]*cps$fat[1] + x[2]*cps$fat[2] + x[3]*cps$fat[3] + x[4]*cps$fat[4] + x[5]*cps$fat[5] + x[6]*cps$fat[6] + x[7]*cps$fat[7] + x[8]*cps$fat[8] + x[9]*cps$fat[9] + x[10]*cps$fat[10] - cstr$fat[2], # upper
    
    # carbs
    # - x[1]*cps$carbs[1] - x[2]*cps$carbs[2] - x[3]*cps$carbs[3]+ cstr$carbs[1],
    # x[1]*cps$carbs[1] + x[2]*cps$carbs[2] + x[3]*cps$carbs[3] - cstr$carbs[2],
    
    - x[1]*cps$carbs[1] - x[2]*cps$carbs[2] - x[3]*cps$carbs[3] - x[4]*cps$carbs[4] - x[5]*cps$carbs[5] - x[6]*cps$carbs[6] -  x[7]*cps$carbs[7] - x[8]*cps$carbs[8] - x[9]*cps$carbs[9]- x[10]*cps$carbs[10] + cstr$carbs[1], # lower
    x[1]*cps$carbs[1] + x[2]*cps$carbs[2] + x[3]*cps$carbs[3] + x[4]*cps$carbs[4] + x[5]*cps$carbs[5] + x[6]*cps$carbs[6] + x[7]*cps$carbs[7] + x[8]*cps$carbs[8] + x[9]*cps$carbs[9] + x[10]*cps$carbs[10] - cstr$carbs[2], # upper
    
    # ghge
    # - x[1]*cps$ghge[1] - x[2]*cps$ghge[2] - x[3]*cps$ghge[3]+ cstr$ghge[1],
    # x[1]*cps$ghge[1] + x[2]*cps$ghge[2] + x[3]*cps$ghge[3] - cstr$ghge[2]
    # 
    - x[1]*cps$ghge[1] - x[2]*cps$ghge[2] - x[3]*cps$ghge[3] - x[4]*cps$ghge[4] - x[5]*cps$ghge[5] - x[6]*cps$ghge[6] -  x[7]*cps$ghge[7] - x[8]*cps$ghge[8] - x[9]*cps$ghge[9]- x[10]*cps$ghge[10] + cstr$ghge[1], # lower
    x[1]*cps$ghge[1] + x[2]*cps$ghge[2] + x[3]*cps$ghge[3] + x[4]*cps$ghge[4] + x[5]*cps$ghge[5] + x[6]*cps$ghge[6] + x[7]*cps$ghge[7] + x[8]*cps$ghge[8] + x[9]*cps$ghge[9] + x[10]*cps$ghge[10] - cstr$ghge[2] # upper
    
  )
  return (constr)
}




# set options ----

# Initial values
# (try different ones!)
x0 <- fd$intake

# lower and upper bounds of x (10 foods)
lb <- fd$intake_lwr
ub <- fd$intake_upr


opts <- list( "algorithm" = "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 160000,
              "tol_constraints_ineq" = rep( 1.0e-10, 10 ))

# run the algorithm
res <- nloptr::nloptr(
  x0          = x0,        # initial value for x
  eval_f      = objective, # objective function
  lb          = lb,        # lower bound for x
  ub          = ub,        # upper bound for x
  eval_g_ineq = inequalconstr, # inequality constraint
  opts        = opts       # options
)

print(res)


# check output ----
# save result
res_diet <- res$solution

# fd$food

# print with the current diet, and change percentage
# also print the boundary, in case it hit boundary

diet_result <- data.frame(
  name = fd$food,
  current = current_diet, 
  new = res_diet, 
  percent_change = round((res_diet - current_diet)/current_diet, 3),
  lower_limit = lb, 
  upper_limit = ub
)
diet_result


# verify whether it falls within 

output_newdiet <- t(as.matrix(res_diet)) %*% as.matrix(contrib_pergram_std)
output_newdiet

# cstr
const_result <- t(rbind(output_newdiet, cstr))
colnames(const_result) <- c('new_diet','const_lwr', 'const_upr')
const_result <- data.table(const_result)
# conditions
const_result[, is_ok := 'Yes']
const_result[new_diet < const_lwr, is_ok := 'beyond lower']
const_result[new_diet > const_upr, is_ok := 'beyond upper']

# relative difference (since we rescaled the targets)
const_result[, relative_dev := 0]
const_result[is_ok == 'beyond lower', relative_dev := round((new_diet - const_lwr)/const_lwr, 3)]
const_result[is_ok == 'beyond upper', relative_dev := round((new_diet - const_upr)/const_upr, 3)]


const_result














