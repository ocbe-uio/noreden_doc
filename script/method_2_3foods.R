# develop based on 3 foods, see if method makes sense

foods

# bread, vegetables, red meat 

three_foods <- foods[food %in% c('Bread', 'Vegetables', 'Red meat')]
three_foods
fd <- three_foods # for parsimony 

# assume it is only 3 out of 25, set the upper limit to 3/25 constraints
# this is probably too generous 

constraints
constr_threefoods <- constraints[, 2:8] * 3/25

# let x1, x2, x3 be the grams of food 
# obj: (x1-bread)^2 + (x2-vege)^2 + (x3-redmeat)^2
# s.t. 
# x1*e1 + x2*e2 + x3*e3 > 1080
# x1*e1 + x2*e2 + x3*e3 < 1200
# ...

# reformulate: 
# -(x1*e1 + x2*e2 + x3*e3)+1080 <=0
# ... 



# compute different constraints ----
# for now use these
const_max_allfoods <- c(9314.3, 98.2, 85.8, 234.7, 39.2, 8.6, 3.8)


# what are the 3 foods contribution
const_max_3foods <- t(as.matrix(fd$intake)) %*% as.matrix(fd[, .(energy, protein, fat, carbs, sugar, alcohol, ghge)])
const_max_3foods

c3foods <- rbind(const_max_3foods*0.9, 
            const_max_3foods*1)
rownames(c3foods) <- c('lwr', 'upr')
c3foods <- data.frame(c3foods)
c3foods

# const_max_3foods * 0.9




# nloptr ----
# find a qp algorithm 


objective <- function(x)
{
  return ( (x[1]- fd$intake[1])^2 + 
             (x[2]- fd$intake[2])^2 + 
             (x[3]- fd$intake[3])^2)
}


# Inequality constraints g(x)<= 0

inequalconstr <- function (x) {
  constr <- c(
    # energy
    - x[1]*fd$energy[1] - x[2]*fd$energy[2] - x[3]*fd$energy[3] + c3$energy[2], # lower
    x[1]*fd$energy[1] + x[2]*fd$energy[2] + x[3]*fd$energy[3] - c3$energy[3], # upper
    
    # protein
    - x[1]*fd$protein[1] - x[2]*fd$protein[2] - x[3]*fd$protein[3] + c3$protein[2],
    x[1]*fd$protein[1] + x[2]*fd$protein[2] + x[3]*fd$protein[3] - c3$protein[3]
    
    # # fat
    # - x[1]*fd$fat[1] - x[2]*fd$fat[2] - x[3]*fd$fat[3] + 7.416,
    # x[1]*fd$fat[1] + x[2]*fd$fat[2] + x[3]*fd$fat[3] - 11.856,
    # 
    # # carb
    # - x[1]*fd$carbs[1] - x[2]*fd$carbs[2] - x[3]*fd$carbs[3] + 30,
    # x[1]*fd$carbs[1] + x[2]*fd$carbs[2] + x[3]*fd$carbs[3] - 40.152,
    # 
    # # sugar
    # - x[1]*fd$sugar[1] - x[2]*fd$sugar[2] - x[3]*fd$sugar[3] +0,
    # x[1]*fd$sugar[1] + x[2]*fd$sugar[2] + x[3]*fd$sugar[3] - 6.576, 
    # 
    # # alcohol
    # - x[1]*fd$alcohol[1] - x[2]*fd$alcohol[2] - x[3]*fd$alcohol[3]+0,
    # x[1]*fd$alcohol[1] + x[2]*fd$alcohol[2] + x[3]*fd$alcohol[3] - 1.2, 
    # 
    # # ghge
    # - x[1]*fd$ghge[1] - x[2]*fd$ghge[2] - x[3]*fd$ghge[3]+0,
    # x[1]*fd$ghge[1] + x[2]*fd$ghge[2] + x[3]*fd$ghge[3] - 0.564
  )
  return (constr)
}

# Lower and upper bounds of x (not constraint)
lb <- c(160, 140, 100)
ub <- c(180, 160, 120)

# Initial values
x0 <- c(175, 150, 110) # try a set slightly different 

opts <- list( "algorithm" = "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 160000,
              "tol_constraints_ineq" = rep( 1.0e-10, 4 ))


res <- nloptr::nloptr(
  x0          = x0,
  eval_f      = objective,
  lb          = lb,
  ub          = ub,
  eval_g_ineq = inequalconstr,
  opts        = opts )

print(res)



# evaluate results 
constr_threefoods

objective(x = c(160,  140, 100)) # 760.8
objective(x = ub)

objective(x = fd$intake)


- x[1]*fd$energy[1] - x[2]*fd$energy[2] - x[3]*fd$energy[3] + 1080
x[1]*fd$energy[1] + x[2]*fd$energy[2] + x[3]*fd$energy[3] - 1200

# these constraints are not good: energy intake is much more than 3/25

inequalconstr(x = c(160,  140, 100)) %>% plot
inequalconstr(x = ub) %>% points
inequalconstr(x = fd$intake)



# ____________ ----
# run algorithm ----




# ____________ ----
# trouble shooting ----


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











