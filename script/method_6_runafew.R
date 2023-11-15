# run a few combination, get a feeling of it
# load raw data ----
library(data.table)
library(readxl)

# load 28 foods
# this is not part of the function
input <- read_excel("data_example/input_0915.xlsx")
input <- setDT(input)
input

# change variable name
setnames(input, 'Vitamin C', 'vitaminc')
setnames(input, 'Calcium', 'calcium')
setnames(input, 'Mean10MJ', 'intake_mean')
setnames(input, '0.1xmean10MJ', 'intake_lwr')
setnames(input, '95thpercentile10MJ', 'intake_upr')


# set parameters ----

# food
# this list excludes coffee,tea; water; alcoholic bev
target_foods <- c('Bread', 
                  'Vegetables', 
                  'Red meat', 
                  'Milk, yoghurt', 
                  'Fish', 
                  'Cheese', 
                  'Eggs', 
                  'Fruit, berries', 
                  'Potatoes'#, 
                  #'Sugar, sweets', 
                  #'Other grains', 
                  #'Butter, margarine, oil'#,
                  #'Juice',
                  #'White meat',
                  #'Cakes',
                  #'Legumes',
                  #'Nuts',
                  #'Cream, cream desserts', 
                  #'Sauces', 
                  #'Snacks', 
                  #'Spices', 
                  #'Soda, saft', 
                  #'Non-dairy milk', 
                  #'Vegetarian products', 
                  #'Other'
                  )



target_foods


# nutrients
target_nutrients <- c('energy', 
                      'protein', 
                      'carbs', 
                      'fat', 
                      'vitaminc', 
                      'calcium'
                      )


# env impact (for now just this one)
target_envir <- c('ghge')


# _____________--------
# take out food and nut/env -----

foods_selected <- select_food(dt = input, 
                              food_names = target_foods)
foods_selected

# use the smaller df to subset 
mean_intake <- select_intake(dt = foods_selected, 
                             intake_names = 'intake_mean')


nutri_pu <- select_nutrients_per_unit(dt = foods_selected, 
                                      nutrient_names = target_nutrients)

env_pu <- select_env_per_unit(dt = foods_selected, 
                              env_names = target_envir)

# nutri_pu and env_pu should be put together; while food intake separate
pu <- cbind(food_name = target_foods, nutri_pu, env_pu)
mean_intake <- cbind(food_name = target_foods, mean_intake)

input_data <- list(current_diet = mean_intake, 
                   unit_contrib = pu)
input_data
saveRDS(input_data, './data_processed/demo_9foods_input.rda')



# compute constraint ----
# compute current contribution to configure the constraint

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

base_vitaminc <- diet_contribution(current_intake = mean_intake, 
                              dt_per_unit = nutri_pu, 
                              tag_outcome = 'vitaminc')

base_calcium <- diet_contribution(current_intake = mean_intake, 
                              dt_per_unit = nutri_pu, 
                              tag_outcome = 'calcium')

base_ghge <- diet_contribution(current_intake = mean_intake, 
                               dt_per_unit = env_pu, 
                               tag_outcome = 'ghge')

# note down the coefficient for standardization

std_coef_energy <- base_energy$std_coef
std_coef_protein <- base_protein$std_coef
std_coef_carbs <- base_carbs$std_coef
std_coef_fat <- base_fat$std_coef
std_coef_vitaminc <- base_vitaminc$std_coef
std_coef_calcium <- base_calcium$std_coef

std_coef_ghge <- base_ghge$std_coef




# lower, upper
# these are already standardized
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

lwrupr_vitaminc <- set_constraint(base_contrib = base_vitaminc$total_contrib_std, 
                             scale_min = 0.9, 
                             scale_max = 1.0)

lwrupr_calcium <- set_constraint(base_contrib = base_calcium$total_contrib_std, 
                             scale_min = 0.9, 
                             scale_max = 1.0)

# ghge
lwrupr_ghge <- set_constraint(base_contrib = base_ghge$total_contrib_std, 
                              scale_min = 0.9, 
                              scale_max = 1.0)




# also keep the raw numbers for comparison latter
lwrupr_energy_raw <- lwrupr_energy / std_coef_energy
lwrupr_protein_raw <- lwrupr_protein / std_coef_protein
lwrupr_carbs_raw <- lwrupr_carbs / std_coef_carbs
lwrupr_fat_raw <- lwrupr_fat / std_coef_fat
lwrupr_vitaminc_raw <- lwrupr_vitaminc / std_coef_vitaminc
lwrupr_calcium_raw <- lwrupr_calcium / std_coef_calcium

lwrupr_ghge_raw <- lwrupr_ghge / std_coef_ghge



# current diet info ----
# also collect the total contrib for the current diet (raw)

current_diet_contrib_df <- rbind(base_energy, 
                                 base_protein, 
                                 base_carbs, 
                                 base_fat,
                                 base_vitaminc, 
                                 base_calcium,
                                 base_ghge)

current_diet_const_std <- rbind(lwrupr_energy, 
                                lwrupr_protein, 
                                lwrupr_carbs, 
                                lwrupr_fat, 
                                lwrupr_vitaminc, 
                                lwrupr_calcium,
                                lwrupr_ghge)

colnames(current_diet_const_std) <- c('constr_min_std', 'cosntr_max_std')

current_diet_const_raw <- rbind(lwrupr_energy_raw, 
                                lwrupr_protein_raw, 
                                lwrupr_carbs_raw, 
                                lwrupr_fat_raw, 
                                lwrupr_vitaminc_raw, 
                                lwrupr_calcium_raw,
                                lwrupr_ghge_raw)

colnames(current_diet_const_raw) <- c('constr_min_raw', 'constr_max_raw')

# give it a name
current_diet_info <- cbind(tag_outcome = c('energy', 
                                           'protein', 
                                           'carbs', 
                                           'fat', 
                                           'vitaminc',
                                           'calcium',
                                           'ghge'),
                           current_diet_contrib_df, 
                           current_diet_const_std,
                           current_diet_const_raw)
current_diet_info

saveRDS(current_diet_info, './data_processed/demo_9foods_constraints.rda')


# objective, inequalc ----

f_objective <- function(x)
{
  # need to spepcify current diet
  # name needs to match 
  current <- mean_intake$intake_mean
  f <- sum((x - current)^2)
  return (f)
}



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

  # ghge
  pu_ghge <- env_pu$ghge * std_coef_ghge
  
  # constraints (lower, upper)
  lwrc_energy <- lwrupr_energy$min
  uprc_energy <- lwrupr_energy$max
  
  lwrc_protein <- lwrupr_protein$min
  uprc_protein <- lwrupr_protein$max
  
  lwrc_ghge <- lwrupr_ghge$min
  uprc_ghge <- lwrupr_ghge$min
  
  
  
  # put constraints in a vector
  # connst_list <- list()
  
  # energy
  ene1 <- - sum(x * pu_energy) + lwrc_energy
  ene2 <- sum(x * pu_energy) - uprc_energy
  
  # protein
  pro1 <- - sum(x * pu_protein) + lwrc_protein
  pro2 <- sum(x * pu_protein) - uprc_protein
  
  # ghge
  ghge1 <- - sum(x * pu_ghge) + lwrc_ghge
  ghge2 <- sum(x * pu_ghge) - uprc_ghge
  
  
  constr <- c(
    ene1, ene2, pro1, pro2, ghge1, ghge2
  )
  return (constr)
}

f_inequalc



# (abstract: function factory) ----

info_energy <- list(pu_energy = nutri_pu$energy * std_coef_energy, 
                    lwrc_energy = lwrupr_energy$min, 
                    uprc_energy = lwrupr_energy$max)

f_factory <- function(namelist, datainfo){
  
  # namelist <- c('ene1')
  
  f_ineq <- function (x) {
    
    # energy
    ene1 <- - sum(x * datainfo$pu_energy) + datainfo$lwrc_energy
    ene2 <- sum(x * datainfo$pu_energy) - datainfo$uprc_energy
    
    # give name
    constr_full <- c(
      ene1 = ene1, 
      ene2 = ene2
    )
    # select the ones that matter:
    constr <- constr_full[namelist]
    
    return (constr)
  }
  return(f_ineq)
}

# in this way, the datainfo can also be scaled/unscaled.
f_inequalc <- f_factory(namelist = 'ene1', datainfo = info_energy)
f_inequalc <- f_factory(namelist = c('ene1', 'ene2'), datainfo = info_energy)



# other param ----
df_intake <- select_intake(dt = foods_selected, 
                           intake_names = c('intake_mean', 
                                            'intake_lwr', 
                                            'intake_upr'))
# Initial values
# probably better to always start from the current diet
x0 <- df_intake$intake_mean

# lower and upper bounds of x (10 foods)
lb <- df_intake$intake_lwr
ub <- df_intake$intake_upr


# number of constraints should be automatically set
nc <- 2 # this should match the length 
# nc <- 10
opts <- list( "algorithm" = "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 160000,
              "tol_constraints_ineq" = rep( 1.0e-10, nc))



# run ----
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


# print(res)


# collect results ----

new_diet_info <- collect_result(result_obj = res, 
                                food_names = foods_selected$foods,
                                current_diet = df_intake$intake_mean,
                                diet_bound_lwr = df_intake$intake_lwr, 
                                diet_bound_upr = df_intake$intake_upr)

new_diet_info

# validate
new_diet_valid <- validate_diet(new_diet = res$solution, 
                                nutri_pu_mat = nutri_pu, 
                                env_pu_mat = env_pu, 
                                current_diet_info = current_diet_info)


new_diet_valid


# saveRDS(new_diet_info, file = './data_processed/demo_12foods_res1.rda')
# saveRDS(new_diet_valid, file = './data_processed/demo_12foods_res2.rda')




# lonng 
# f_inequalc <- function (x) {
#   
#   # x is the diet vector (target)
#   # contrib_per_unit
#   # pu_energy <- nutri_pu$energy
#   # pu_protein <- nutri_pu$protein
#   # pu_carbs <- nutri_pu$carbs
#   # pu_fat <- nutri_pu$fat
#   # ghge
#   # pu_ghge <- env_pu$ghge
#   
#   # if scaled:
#   pu_energy <- nutri_pu$energy * std_coef_energy
#   pu_protein <- nutri_pu$protein * std_coef_protein
#   # pu_carbs <- nutri_pu$carbs * std_coef_carbs
#   # pu_fat <- nutri_pu$fat * std_coef_fat
#   # pu_vitaminc <- nutri_pu$vitaminc * std_coef_vitaminc
#   # pu_calcium <- nutri_pu$calcium * std_coef_calcium
#   
#   # ghge
#   pu_ghge <- env_pu$ghge * std_coef_ghge
#   
#   
#   
#   # constraints (lower, upper)
#   lwrc_energy <- lwrupr_energy$min
#   uprc_energy <- lwrupr_energy$max
#   
#   lwrc_protein <- lwrupr_protein$min
#   uprc_protein <- lwrupr_protein$max
#   
#   # lwrc_carbs <- lwrupr_carbs$min
#   # uprc_carbs <- lwrupr_carbs$max
#   # 
#   # lwrc_fat <- lwrupr_fat$min
#   # uprc_fat <- lwrupr_fat$max
#   # 
#   # lwrc_vitaminc <- lwrupr_vitaminc$min
#   # uprc_vitaminc <- lwrupr_vitaminc$max
#   # 
#   # lwrc_calcium <- lwrupr_calcium$min
#   # uprc_calcium <- lwrupr_calcium$max
#   # 
#   lwrc_ghge <- lwrupr_ghge$min
#   uprc_ghge <- lwrupr_ghge$min
#   
#   # put constraints in a vector
#   constr <- c(
#     # energy
#     - sum(x * pu_energy) + lwrc_energy,
#     sum(x * pu_energy) - uprc_energy,
#     
#     # protein
#     - sum(x * pu_protein) + lwrc_protein,
#     sum(x * pu_protein) - uprc_protein,
#     
#     # carbs
#     # - sum(x * pu_carbs) + lwrc_carbs,
#     # sum(x * pu_carbs) - uprc_carbs,
#     
#     # fat
#     # - sum(x * pu_fat) + lwrc_fat,
#     # sum(x * pu_fat) - uprc_fat,
#     
#     # vitaminc
#     # - sum(x * pu_vitaminc) + lwrc_vitaminc,
#     # sum(x * pu_vitaminc) - uprc_vitaminc,
#     
#     # calcium
#     # - sum(x * pu_calcium) + lwrc_calcium,
#     # sum(x * pu_calcium) - uprc_calcium,
#     
#     # ghge
#     - sum(x * pu_ghge) + lwrc_ghge,
#     sum(x * pu_ghge) - uprc_ghge
#   )
#   return (constr)
# }







