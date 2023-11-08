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



# target_foods <- c('Bread', 'Vegetables', 'Red meat')
# target_nutrients <- c('energy', 'protein', 'carbs', 'fat')
# target_envir <- c('ghge')

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
select_intake(dt = input, 
              intake_names = c('intake_mean', 'intake_lwr', 'intake_upr'))


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
                              tag_outcome){
  
  # compute total contrib X foods (contrib_pu * intake)
  # current_intake <- mean_intake
  # dt_per_unit <- nutri_pu
  # tag_outcome <- 'energy'
  
  total_contrib_raw <- sum(current_intake * dt_per_unit[, ..tag_outcome])
  
  # standardized as well
  stopifnot('Must have more than 1 food to compute sd' = nrow(current_intake)>1)
  sd <- sd(dt_per_unit[[tag_outcome]])
  total_contrib_std <- total_contrib_raw / sd
  
  res <- data.frame('total_contrib_raw' = total_contrib_raw, 
                    'total_contrib_std' = total_contrib_std, 
                    'std_coef' = 1/sd)
  
  
  return(res)
  
}


diet_contribution(current_intake = mean_intake, 
                      dt_per_unit = nutri_pu, 
                      tag_outcome = 'energy')


# f: set constraint -----
# the constraints are based on the current diet contribution
# for reduction (e.g. ghge) just set lower upper bound 

set_constraint <- function(base_contrib, 
                           scale_min, 
                           scale_max){
  
  const_min <- base_contrib * scale_min
  const_max <- base_contrib * scale_max
  res <- data.frame(min = const_min, 
                    max = const_max)
  return(res)
}


# COMPUTE CONSTRAINTS ----

# first compute the basis (i.e. maximum total contrib)
# inequality constraints are based on this value

base_energy <- diet_contribution(current_intake = mean_intake, 
                                 dt_per_unit = nutri_pu, 
                                 tag_outcome = 'energy')

base_protein <- diet_contribution(current_intake = mean_intake, 
                                  dt_per_unit = nutri_pu, 
                                  tag_outcome = 'protein')

base_carbs <- diet_contribution(current_intake = mean_intake, 
                                dt_per_unit = nutri_pu, 
                                tag_outcome = 'carbs')

base_fat <- diet_contribution(current_intake = mean_intake, 
                              dt_per_unit = nutri_pu, 
                              tag_outcome = 'fat')


base_ghge <- diet_contribution(current_intake = mean_intake, 
                               dt_per_unit = env_pu, 
                               tag_outcome = 'ghge')

# set constraint parameters:
# 0.9, 1.0 for nutrients
# ghge: 0.9, 1.0 for now
# need to set to lower later
# set_constraint(base_contrib = 701.908, scale_min = 0.9, scale_max = 1.0)
lwrupr_energy <- set_constraint(base_contrib = base_energy$total_contrib_std, 
                                scale_min = 0.9, 
                                scale_max = 1.0)

lwrupr_protein <- set_constraint(base_contrib = base_protein$total_contrib_std, 
                                 scale_min = 0.9, 
                                 scale_max = 1.0)


lwrupr_carbs <- set_constraint(base_contrib = base_carbs$total_contrib_std, 
                               scale_min = 0.9, 
                               scale_max = 1.0)

lwrupr_fat <- set_constraint(base_contrib = base_fat$total_contrib_std, 
                             scale_min = 0.9, 
                             scale_max = 1.0)
# ghge_unchanged
lwrupr_ghge <- set_constraint(base_contrib = base_ghge$total_contrib_std, 
                              scale_min = 0.9, 
                              scale_max = 1.0)
# ghge reduced to 80%
# lwrupr_ghge_reduced <- 0.8 * lwrupr_ghge


std_coef_energy <- base_energy$std_coef
std_coef_protein <- base_protein$std_coef
std_coef_carbs <- base_carbs$std_coef
std_coef_fat <- base_fat$std_coef
std_coef_ghge <- base_ghge$std_coef

lwrupr_energy_raw <- lwrupr_energy / std_coef_energy
lwrupr_protein_raw <- lwrupr_protein / std_coef_protein
lwrupr_carbs_raw <- lwrupr_carbs / std_coef_carbs
lwrupr_fat_raw <- lwrupr_fat / std_coef_fat
lwrupr_ghge_raw <- lwrupr_ghge / std_coef_ghge

# also collect the total contrib for the current diet (raw)

current_diet_contrib_df <- rbind(base_energy, 
                                 base_protein, 
                                 base_carbs, 
                                 base_fat, 
                                 base_ghge)

current_diet_const_raw <- rbind(lwrupr_energy_raw, 
                                lwrupr_protein_raw, 
                                lwrupr_carbs_raw, 
                                lwrupr_fat_raw, 
                                lwrupr_ghge_raw)

# give it a name
current_diet_info <- cbind(tag_outcome = c('energy', 
                                           'protein', 
                                           'carbs', 
                                           'fat', 
                                           'ghge'),
                           current_diet_contrib_df, 
                           current_diet_const_raw)
current_diet_info




# _________________ ----
# SET UP PARAMETERS ----

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
# TO DO (future): make adding and removing constraints flexible

f_inequalc <- function (x) {
  
  # x is the diet vector (target)
  # contrib_per_unit
  # pu_energy <- nutri_pu$energy
  # pu_protein <- nutri_pu$protein
  # pu_carbs <- nutri_pu$carbs
  # pu_fat <- nutri_pu$fat
  # ghge
  # pu_ghge <- env_pu$ghge
  
  # if scaled:
  pu_energy <- nutri_pu$energy * std_coef_energy
  pu_protein <- nutri_pu$protein * std_coef_protein
  # pu_carbs <- nutri_pu$carbs * std_coef_carbs
  # pu_fat <- nutri_pu$fat * std_coef_fat
  # ghge
  pu_ghge <- env_pu$ghge * std_coef_ghge
  
  # constraints (lower, upper)
  lwrc_energy <- lwrupr_energy$min
  uprc_energy <- lwrupr_energy$max
  
  lwrc_protein <- lwrupr_protein$min
  uprc_protein <- lwrupr_protein$max
  
  # lwrc_carbs <- lwrupr_carbs$min
  # uprc_carbs <- lwrupr_carbs$max
  # 
  # lwrc_fats <- lwrupr_fat$min
  # uprc_fats <- lwrupr_fat$max
  
  lwrc_ghge <- lwrupr_ghge$min
  uprc_ghge <- lwrupr_ghge$min
  
  # put constraints in a vector
  constr <- c(
    # energy
    - sum(x * pu_energy) + lwrc_energy,
    sum(x * pu_energy) - uprc_energy,
    
    # protein
    - sum(x * pu_protein) + lwrc_protein,
    sum(x * pu_protein) - uprc_protein,
    
    # # carbs
    # - sum(x * pu_carbs) + lwrc_carbs,
    # sum(x * pu_carbs) - uprc_carbs,
    # 
    # 
    # # fat
    # - sum(x * pu_fat) + lwrc_fat,
    # sum(x * pu_fat) - uprc_fat,
    
    # ghge
    - sum(x * pu_ghge) + lwrc_ghge,
    sum(x * pu_ghge) - uprc_ghge
  )
  return (constr)
}



# RUN ----
# this is the 3 foods example

df_intake <- select_intake(dt = foods_selected, 
              intake_names = c('intake_mean', 'intake_lwr', 'intake_upr'))


# Initial values
# probably better to always start from the current diet
x0 <- df_intake$intake_mean

# lower and upper bounds of x (10 foods)
lb <- df_intake$intake_lwr
ub <- df_intake$intake_upr


# number of constraints should be automatically set

opts <- list( "algorithm" = "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 160000,
              "tol_constraints_ineq" = rep( 1.0e-10, 6))


# set timer
start_time <- Sys.time()
# run the algorithm

res <- nloptr::nloptr(
  x0          = x0,        # initial value for x
  eval_f      = f_objective, # objective function
  lb          = lb,        # lower bound for x
  ub          = ub,        # upper bound for x
  eval_g_ineq = f_inequalc, # inequality constraint
  opts        = opts       # options
)

end_time <- Sys.time()
td <- end_time - start_time
td


print(res)



# f: collect results ----

collect_result <- function(result_obj, 
                           food_names,
                           current_diet,
                           diet_bound_lwr, 
                           diet_bound_upr){
  
  # food_names <- foods_selected$food
  # put in a table
  new_diet <- result_obj$solution
  
  diet_result <- data.frame(
    food_name = food_names,
    current = round(current_diet, 2), 
    new = round(new_diet, 2), 
    absolute_change = round(new_diet - current_diet, 2),
    percent_change = 100*round((new_diet - current_diet)/current_diet, 3),
    diet_bound_lwr = round(diet_bound_lwr, 2), 
    diet_bound_upr = round(diet_bound_upr, 2)
  )
  
  return(diet_result)
}



# f: validate new diet ----



validate_diet <- function(new_diet, 
                          nutri_pu_mat, 
                          env_pu_mat, 
                          current_diet_info){
  
  # on original scale is fine
  # new_diet <- res$solution
  # nutri_pu_mat <- nutri_pu
  # env_pu_mat <- env_pu
  
  contrib_pu <- cbind(nutri_pu_mat, env_pu_mat)
  tag_outcome <- colnames(contrib_pu)
  
  total_contrib <- t(as.matrix(new_diet)) %*% as.matrix(contrib_pu)
  # add to the current
  # tc: total contrib
  tc_df <- cbind(current_diet_info, 
                 tc_new_diet = t(total_contrib))
  
  
  data.table::setDT(tc_df)
  # drop some columns 
  tc_df[, std_coef := NULL]
  tc_df[, total_contrib_std := NULL]
  
  # round to 2 digits
  tc_df[, total_contrib_raw := round(total_contrib_raw, 2)]
  tc_df[, min := round(min, 2)]
  tc_df[, max := round(max, 2)]
  tc_df[, tc_new_diet := round(tc_new_diet, 2)]
  
  # compare
  tc_df[, is_ok := 'Yes']
  tc_df[tc_new_diet > max, is_ok := 'beyond_upper']
  tc_df[tc_new_diet < min, is_ok := 'beyond_lower']
  
  # relative difference
  tc_df[, dev_if_not_ok := 0]
  tc_df[is_ok == 'beyond_upper', dev_if_not_ok := 
              round((tc_new_diet - max)/max, 3)]
  
  tc_df[is_ok == 'beyond_lower', dev_if_not_ok := 
              round((tc_new_diet - min)/min, 3)]

  
  tc_df <- as.data.frame(tc_df)
  
  return(tc_df)
  
}



tt <- validate_diet(new_diet = res$solution, 
                    nutri_pu_mat = nutri_pu, 
                    env_pu_mat = env_pu, 
                    current_diet_info = current_diet_info)








