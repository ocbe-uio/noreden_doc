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
    
    # fat
    - x[1]*cps$fat[1] - x[2]*cps$fat[2] - x[3]*cps$fat[3]+ cstr$fat[1],
    x[1]*cps$fat[1] + x[2]*cps$fat[2] + x[3]*cps$fat[3] - cstr$fat[2],

    # carbs
    - x[1]*cps$carbs[1] - x[2]*cps$carbs[2] - x[3]*cps$carbs[3]+ cstr$carbs[1],
    x[1]*cps$carbs[1] + x[2]*cps$carbs[2] + x[3]*cps$carbs[3] - cstr$carbs[2],
    
    # ghge
    - x[1]*cps$ghge[1] - x[2]*cps$ghge[2] - x[3]*cps$ghge[3]+ cstr$ghge[1],
    x[1]*cps$ghge[1] + x[2]*cps$ghge[2] + x[3]*cps$ghge[3] - cstr$ghge[2]
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






# ____ DEV _____ -----
# test different coefficients to standardize the contribution of energy, ghge etc

energy_3fd_original <- fd$energy
protein_3fd_original <- fd$protein
ghge_3fd_original <- fd$ghge

# divide by sd (not substracting mean, since we want positive values)
energy_3fd_new <- energy_3fd_original/sd(energy_3fd_original)
energy_3fd_new

protein_3fd_new <- protein_3fd_original/sd(protein_3fd_original)
protein_3fd_new

ghge_3fd_new <- ghge_3fd_original/sd(ghge_3fd_original)
ghge_3fd_new

# construct new target: divide original by the sd of each column
c3
fd_5target <- fd[, c('energy', 'protein', 'fat', 'carbs', 'ghge')]
fd_5target
sd_coef <- apply(fd_5target, MARGIN = 2, sd)

fd_5target_standard <- sweep(fd_5target, MARGIN = 2, 1/sd_coef, FUN = '*')
# also the constraint
# test the previous constraint


c3 <- c3foods[, c('energy', 'protein', 'fat', 'carbs','ghge')]
c3
# c3_original <- copy(c3)

# lower ghge
c3$ghge <- c3$ghge * 0.9 # you can try different limits
c3
# use the standardized
c3 <- sweep(c3, MARGIN = 2, 1/sd_coef, FUN = '*')

# see if the constraints make sense with the new scale
t(as.matrix(c(175, 154, 117))) %*% as.matrix(fd_5target_standard)
# ok, seems to be consitent

# try the new one ----


# this one remains the same
objective <- function(x)
{
  return ( (x[1]- fd$intake[1])^2 + 
             (x[2]- fd$intake[2])^2 + 
             (x[3]- fd$intake[3])^2)
}


# define the inequality constraints
inequalconstr <- function (x) {
  
  fd <- fd_5target_standard # so that i don't need to change stuff later
  constr <- c(
    # energy
    - x[1]*fd$energy[1] - x[2]*fd$energy[2] - x[3]*fd$energy[3] + c3$energy[1], # lower
    x[1]*fd$energy[1] + x[2]*fd$energy[2] + x[3]*fd$energy[3] - c3$energy[2], # upper
    
    # protein
    - x[1]*fd$protein[1] - x[2]*fd$protein[2] - x[3]*fd$protein[3] + c3$protein[1],
    x[1]*fd$protein[1] + x[2]*fd$protein[2] + x[3]*fd$protein[3] - c3$protein[2],
    
    # # fat
    # - x[1]*fd$fat[1] - x[2]*fd$fat[2] - x[3]*fd$fat[3]+ c3$fat[1],
    # x[1]*fd$fat[1] + x[2]*fd$fat[2] + x[3]*fd$fat[3] - c3$fat[2],
    # 
    # # carbs
    # - x[1]*fd$carbs[1] - x[2]*fd$carbs[2] - x[3]*fd$carbs[3]+ c3$carbs[1],
    # x[1]*fd$carbs[1] + x[2]*fd$carbs[2] + x[3]*fd$carbs[3] - c3$carbs[2],
    
    # ghge
    - x[1]*fd$ghge[1] - x[2]*fd$ghge[2] - x[3]*fd$ghge[3]+ c3$ghge[1],
    x[1]*fd$ghge[1] + x[2]*fd$ghge[2] + x[3]*fd$ghge[3] - c3$ghge[2]
  )
  return (constr)
}



