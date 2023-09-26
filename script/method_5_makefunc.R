# make functions

library(data.table)
library(readxl)

# load 28 foods
# this is not part of the function
input <- read_excel("data_example/input_0915.xlsx")
input <- setDT(input)
input

# change variable name
setnames(input, 'Mean10MJ', 'intake_mean')
setnames(input, '0.1xmean10MJ', 'intake_lwr')
setnames(input, '95thpercentile10MJ', 'intake_upr')


# benchmarking
# start_time <- Sys.time()
# # do something
# end_time <- Sys.time()
# td <- end_time - start_time
# td



target_foods <- c('Bread', 'Vegetables', 'Red meat')
target_nutrients <- c('energy', 'protein', 'carbs', 'fat')
target_envir <- c('ghge')

# f: food selector ----

select_food <- function(dt, food_names){
  
  # dt <- input
  res <- dt[foods %in% food_names]
  return(res)
}


# select_food(dt = input, food_names = target_foods)



# f: current intake ----

select_intake <- function(dt, intake_names){
  
  # dt <- input
  res <- dt[, ..intake_names]
  return(res)
}


select_intake(dt = input, intake_names = 'intake_mean')
select_intake(dt = input, intake_names = c('intake_mean', 'intake_lwr', 'intake_upr'))


# f: nutconst selector ----
# contribution per unit 


# dt[, .(energy, carbs)]
# dt[, ..target_nutrients]

select_nutrients_per_unit <- function(dt, nutrient_names){
  
  # dt <- input
  res <- dt[, ..nutrient_names]
  return(res)
}


select_nutrients_per_unit(dt = input, nutrient_names = target_nutrients)



# f: envconst selector ----
# contribution per unit

select_env_per_unit <- function(dt, env_names){
  
  # dt <- input
  res <- dt[, ..env_names]
  return(res)
}

select_env_per_unit(dt = input, env_names = target_envir)






# f: current diet contrib ----
# used as basis to compute constraint for nutrients, envimpact
# by default, also return standardized

# might be good if one by one


foods_selected <- select_food(dt = input, food_names = target_foods)
foods_selected

# use the smaller df to subset 
mean_intake <- select_intake(dt = foods_selected, 
                             intake_names = 'intake_mean')

nutri_pu <- select_nutrients_per_unit(dt = foods_selected, 
                                      nutrient_names = target_nutrients)

env_pu <- select_env_per_unit(dt = foods_selected, 
                              env_names = target_envir)



diet_contribution <- function(current_intake, 
                              dt_per_unit, 
                              tag_outcome, 
                              std_only = T){
  
  # compute total contrib X foods (contrib_pu * intake)
  # current_intake <- mean_intake
  # dt_per_unit <- nutri_pu
  # tag_outcome <- 'energy'
  
  total_contrib_raw <- sum(current_intake * dt_per_unit[, ..tag_outcome])
  
  # standardized as well
  stopifnot('Must have more than 1 food to compute sd' = nrow(current_intake)>1)
  sd <- sd(dt_per_unit[[tag_outcome]])
  total_contrib_std <- total_contrib_raw / sd
  
  if(std_only == T){
    res <- total_contrib_std
  }else{
    res <- data.frame('total_contrib_raw' = total_contrib_raw, 
                      'total_contrib_std' = total_contrib_std)
  }
  
  return(res)
  
}


diet_contribution(current_intake = mean_intake, 
                      dt_per_unit = nutri_pu, 
                      tag_outcome = 'energy')

# diet_contribution(current_intake = mean_intake, 
#                   dt_per_unit = nutri_pu, 
#                   tag_outcome = 'energy', std_only = F)


diet_contribution(current_intake = mean_intake, 
                      dt_per_unit = nutri_pu, 
                      tag_outcome = 'protein')

diet_contribution(current_intake = mean_intake, 
                      dt_per_unit = nutri_pu, 
                      tag_outcome = 'fat')

diet_contribution(current_intake = mean_intake, 
                      dt_per_unit = nutri_pu, 
                      tag_outcome = 'carbs')

diet_contribution(current_intake = mean_intake, 
                      dt_per_unit = env_pu, 
                      tag_outcome = 'ghge')



# f: set constraint -----
# the constraints are based on the current diet contribution
# for reduction (e.g. ghge) just set lower upper bound 

set_constraint <- function(base_contrib, scale_min, scale_max){
  
  const_min <- base_contrib * scale_min
  const_max <- base_contrib * scale_max
  res <- data.frame(min = const_min, 
                    max = const_max)
  return(res)
}


# set_constraint(base_contrib = 701.908, scale_min = 0.9, scale_max = 1.0)





# f: objective ----
# define qp
# (this should also be flexible, in case we use other)
# does it need a second arg for current diet?

f_objective <- function(x)
{
  # need to spepcify current diet
  current <- current_intake$intake_mean
  f <- sum((x - current)^2)
  return (f)
}





# f: ineq const ----
# define inequality constraints
# should match the items from const computer
# make a check for name


f_inequalc <- function (x) {
  
  # cps <- contrib_pergram_std
  # 
  # energy <- cps$energy
  # protein <- cps$protein
  # ghge <- cps$ghge
  # 
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



# RUN ----


# f: collect results ----










