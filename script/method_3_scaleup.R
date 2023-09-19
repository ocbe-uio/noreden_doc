# 1. find when it does not meet the constraint
# 2. standardize the numeric values, see if it improves stability
# (ask Julie to test it out)

# (after Basel)
# 3. recode the program, needs to be flexible enough
# should also profile the code. test the speed for different size

# IF DOESN'T WORK
# optim
# RCplex

library(data.table)


# raw data of foods ----

foods <- read.csv('./data_public/foods.csv')
setDT(foods)
foods


# standardize target values ----


# start with 3 foods
fd <- foods[food %in% c('Bread', 'Vegetables', 'Red meat')]
fd

current_diet <- fd$intake
current_diet

contrib_pergram <- fd[, .(energy, protein, fat, carbs, sugar, alcohol, ghge)]
contrib_pergram

# original constraint for all foods (maximum)
# this is the upper limit for all diet (g)* contrib per g

const_max_allfoods <- c(9314.3, 98.2, 85.8, 234.7, 39.2, 8.6, 3.8)


# 3 foods contribution (maximum)
const_max_3foods <- t(as.matrix(current_diet)) %*% as.matrix(contrib_pergram)
const_max_3foods

# exclude sugar, alcohol
const_max_3foods <- const_max_3foods[, c('energy', 'protein', 'fat', 'carbs', 'ghge')]

# set lower to be 0.9; upper remain the current max
const_lwrupr <- rbind(const_max_3foods*0.9, const_max_3foods*1)
rownames(const_lwrupr) <- c('lwr', 'upr')
const_lwrupr <- data.table(const_lwrupr)
const_lwrupr


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


# lower ghge ----
# this should be tested out: different reduction percentage
# 80% is probably too progressive
cstr <- copy(const_lwrupr_std)
cstr$ghge <- cstr$ghge * 0.9 # reduce to 0.9
cstr




# define objective ----
# 3 foods, therefore 3 elements
# minimize the deviation from the current intake, on 3 foods
objective <- function(x)
{
  return ( (x[1]- current_diet[1])^2 + 
             (x[2]- current_diet[2])^2 + 
             (x[3]- current_diet[3])^2)
}


# define constraints ----
contrib_pergram_std

# define the inequality constraints
inequalconstr <- function (x) {
  
  cps <- contrib_pergram_std
  
  constr <- c(
    # energy
    - x[1]*cps$energy[1] - x[2]*cps$energy[2] - x[3]*cps$energy[3] + cstr$energy[1], # lower
    x[1]*cps$energy[1] + x[2]*cps$energy[2] + x[3]*cps$energy[3] - cstr$energy[2], # upper
    
    # protein
    - x[1]*cps$protein[1] - x[2]*cps$protein[2] - x[3]*cps$protein[3] + cstr$protein[1],
    x[1]*cps$protein[1] + x[2]*cps$protein[2] + x[3]*cps$protein[3] - cstr$protein[2],
    
    # # fat
    # - x[1]*cps$fat[1] - x[2]*cps$fat[2] - x[3]*cps$fat[3]+ cstr$fat[1],
    # x[1]*cps$fat[1] + x[2]*cps$fat[2] + x[3]*cps$fat[3] - cstr$fat[2],
    # 
    # # carbs
    # - x[1]*cps$carbs[1] - x[2]*cps$carbs[2] - x[3]*cps$carbs[3]+ cstr$carbs[1],
    # x[1]*cps$carbs[1] + x[2]*cps$carbs[2] + x[3]*cps$carbs[3] - cstr$carbs[2],
    
    # ghge
    - x[1]*cps$ghge[1] - x[2]*cps$ghge[2] - x[3]*cps$ghge[3]+ cstr$ghge[1],
    x[1]*cps$ghge[1] + x[2]*cps$ghge[2] + x[3]*cps$ghge[3] - cstr$ghge[2]
  )
  return (constr)
}


# class(inequalconstr)

# test functional approach ----
# function factory?

# objective2 <- function(x)
# {
#   p1 <- (x[1]- current_diet[1])^2
#   p2 <- (x[2]- current_diet[2])^2
#   p3 <- (x[3]- current_diet[3])^2
#   
#   l <- list(p1, p2, p3)
#   f <- sum(unlist(p1, p2, p3))
#   
#   return (f)
# }

objective3 <- function(x)
{
  # this works
  f <- sum((x- current_diet)^2)
  return (f)
}



inequalconstr2 <- function (x) {
  
  cps <- contrib_pergram_std
  
  energy <- cps$energy
  protein <- cps$protein
  ghge <- cps$ghge
  
  constr <- c(
    # energy
    - sum(x * energy) + cstr$energy[1],
    sum(x * energy) - cstr$energy[2],
    
    # protein
    - sum(x * protein) + cstr$protein[1],
    sum(x * protein) - cstr$protein[2],
    
    # ghge
    - sum(x * ghge) + cstr$ghge[1],
    sum(x * ghge) - cstr$ghge[2]
    )
  return (constr)
}


# set options ----
# lower and upper bounds of x (3 foods)
lb <- c(160, 140, 100)
ub <- c(180, 160, 120)

# Initial values
# (try different ones!)
x0 <- c(175, 150, 110)

opts <- list( "algorithm" = "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 160000,
              "tol_constraints_ineq" = rep( 1.0e-10, 6 ))

# run the algorithm
res <- nloptr::nloptr(
  x0          = x0,        # initial value for x
  eval_f      = objective3, # objective function
  lb          = lb,        # lower bound for x
  ub          = ub,        # upper bound for x
  eval_g_ineq = inequalconstr2, # inequality constraint
  opts        = opts       # options
)

print(res)














# _____________ ----
# check output ----
# save result
res_diet <- res$solution

# print with the current diet, and change percentage
# also print the boundary, in case it hit boundary

diet_result <- data.frame(
  name = c('Bread', 'Vegetables', 'Red meat'),
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







