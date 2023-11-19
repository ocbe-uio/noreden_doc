# this is the script where we make the whole workflow in functions

# ____ STEP 1: INPUT ____ ----

d_diet <- readRDS('./data_processed/d_diet.rda')
d_perunit_contrib <- readRDS('./data_processed/d_perunit_contrib.rda')

d_perunit_contrib |> head()



# use the 9 food example

tag_food_9 <- c('Bread', 
              'Vegetables', 
              'Red meat', 
              'Milk, yoghurt', 
              'Fish', 
              'Cheese', 
              'Eggs', 
              'Fruit, berries', 
              'Potatoes'
)

tag_outcome_9 <- c('energy',
                   'protein', 
                   'carbs', 
                   'sugar', 
                   'fiber', 
                   'fat', 
                   'vitaminc', 
                   'calcium', 
                   'ghge')




# ____ STEP 2: COMPUTE CONSTR ____ ----

# we supply the entire dataset for all nutrients when computing the constraints
# need to specify reduction factor on ghge 


# d_diet

# filter(d_diet, food_name %in% tag_food)
# filter(d_perunit_contrib, food_name %in% tag_food)


diet_s <- select_diet(data_diet = d_diet, tag_food = tag_food_9)

puc_s <- select_perunit(data_perunit_contrib = d_perunit_contrib, 
               tag_food = tag_food_9, 
               tag_outcome = tag_outcome_9)


tc <- total_contrib(data_diet = diet_s, 
              data_perunit_contrib = puc_s)


# standardized total contrib
stdcoef_9 <- get_stdcoef(data_perunit_contrib = d_perunit_contrib)

coefs <- stdcoef_9$std_coef

cd_unit_contrib_std <- makestd_unit_contrib(
  uc_raw = puc_s,
  std_coef = coefs)
puc_s_std <- cd_unit_contrib_std$uc_std

tc_std <- total_contrib(data_diet = diet_s, 
                    data_perunit_contrib = puc_s_std)


# alternatively, directly use tc multiply by coef
tc$total_contrib$total_contrib * coefs$std_coef
# ok 



# COMPUTE CONSTRAINTS ----

# first compute the basis (i.e. maximum total contrib)
# inequality constraints are based on this value

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


base_energy <- diet_contribution(current_intake = mean_intake, 
                                 dt_per_unit = nutri_pu, 
                                 tag_outcome = 'energy')







# ____ STEP 3: ALGO ____ ----
# require 
# nutri_pu (per unit contribution) or envir_pu
# lwrupr_energy (computed as constraints)


demo_input <- readRDS('./data_processed/demo_9foods_input.rda')
demo_constraints <- readRDS('./data_processed/demo_9foods_constraints.rda')


# hot fix:
colnames(demo_constraints)[
  which(colnames(demo_constraints)=='cosntr_max_std')] <- 'constr_max_std'
# these constraints need to have an additional column that has the reduction 


# in this example: 9 foods, 7 constraints (might not need to be used for all)

# set param ----- 
# cd: current diet
cd <- as.data.frame(demo_input$current_diet)
cd_unit_contrib <- as.data.frame(demo_input$unit_contrib)
cd_constr <- as.data.frame(demo_constraints)



# diet: 
# initial values
# probably better to always start from the current diet
x0 <- cd$intake_mean

# lower and upper bounds of x (10 foods)
lb <- cd$intake_lwr
ub <- cd$intake_upr


# constraints: 
# by tag (e.g. energy, protein)
constval <- constr_by_tag(d_unit_contrib = cd_unit_contrib, 
                          d_constr = cd_constr)

constval$val_std
constval$val_std$energy



# set objective ----

fo <- f_make_objective_function(diet0 = x0)

# fo(x = (x0-2))


# set ineq constraints ----
# in this way, the datainfo can also be scaled/unscaled.
# tags <- c('energy', 'protein')
tags <- c('energy', 'protein', 'ghge')

f_inequalc <- f_make_constraint_function(
  constraint_values = constval$val_std, 
  tag_outcomes = tags)

# f_inequalc(x = x0)
# f_inequalc
# f_inequalc <- f_factory(namelist = c('ene1', 'ene2'), datainfo = info_energy)



# run ----

res <- find_new_diet(diet0 = cd$intake_mean, 
                     diet0_upr = cd$intake_upr, 
                     diet0_lwr = cd$intake_lwr, 
                     tag_outcomes = c('energy', 'protein', 'ghge'), 
                     constraint_val = constval$val_std, 
                     print_runtime = T)

res


collect_result(result_obj = res$run_optim, 
               food_names = foods_selected$foods,
               current_diet = cd$intake_mean,
               diet_bound_lwr = cd$intake_lwr, 
               diet_bound_upr = cd$intake_upr)





# _________ ----
#  utility  ----

select_diet <- function(data_diet, tag_food){
  
  diet_selected <- dplyr::filter(data_diet, food_name %in% tag_food) |> 
    dplyr::select(dplyr::all_of(c('food_name','intake_mean')))
  
  return(diet_selected)
}



select_perunit <- function(data_perunit_contrib, 
                           tag_food, 
                           tag_outcome){
  
  perunit_selected <- dplyr::filter(data_perunit_contrib, 
                                    food_name %in% tag_food) |> 
    dplyr::select(dplyr::all_of(c('food_name', tag_outcome)))
  
  return(perunit_selected)
}



total_contrib <- function(data_diet, 
                          data_perunit_contrib){
  
  # compute total contrib X foods (contrib_pu * intake)
  # data_diet <- diet_s
  # data_perunit_contrib <- puc_s
  
  # check if names match 
  if(!all.equal(data_diet$food_name, data_perunit_contrib$food_name)){
    stop('Food names do not match, check input data')
  }
  # at least have mean intake in the diet data
  if(!'intake_mean' %in% colnames(data_diet)){
    stop('Need to supply intake_mean for diet computation')
  }
  
  # remove food_nname for computation
  uc_tb <- dplyr::select(data_perunit_contrib, -c('food_name'))
  
  # total contrib
  # 1:9 times 9:9
  tc <- t(as.matrix(data_diet$intake_mean)) %*% as.matrix(uc_tb)
  
  tag_outcome <- colnames(uc_tb)
  food_name <- data_diet$food_name
  # make it in df 
  tc_table <- data.frame(tag_outcome = tag_outcome, 
                         total_contrib = as.numeric(tc))
  
  # print(tc_table)
  res <- list(total_contrib = tc_table, 
              tag_food = food_name, 
              tag_outcome = tag_outcome)
  return(res)
  
}



get_stdcoef <- function(data_perunit_contrib, 
                        method = 'sd'){
  
  # data_perunit_contrib <- puc_s
  
  if('food_name' %in% colnames(data_perunit_contrib)){
    uc_tb <- dplyr::select(data_perunit_contrib, -c('food_name'))
  }else{
    uc_tb <- data_perunit_contrib
  }
  
  # take the smaller subset
  
  if(method == 'sd'){
    # print('Method: divide by standard deviation of current diet wrt tag_outcome')
    stopifnot('Must have more than 1 food to compute sd' = 
                nrow(uc_tb)>1)
    
    sd_vec <- apply(uc_tb, MARGIN = 2, sd)
    std_coef <- 1/sd_vec
  }
  
  tag_outcome <- colnames(uc_tb)
  
  std_df <- data.frame(tag_outcome = tag_outcome, 
                       std_coef = as.numeric(std_coef))
  
  res <- list(std_coef = std_df, 
              method = method)
  
  return(res)
}


makestd_unit_contrib <- function(uc_raw, std_coef){
  
  # check tag_outcome name consistency
  if('food_name' %in% colnames(uc_raw)){
    uc_tb <- dplyr::select(uc_raw, -c('food_name'))
  }else{
    uc_tb <- uc_raw
  }
  if(!all.equal(colnames(uc_tb), std_coef$tag_outcome)){
    stop('tag_outcome names do not match')
  }
  
  # col1 * coef1, col2 * coef2, ...
  # t(t(matrix(c(1,1,1,2,2,2), nrow = 3)) * c(3,4))
 
  uc_tb_std <- data.frame(t(t(uc_tb) * std_coef$std_coef))
  
  if('food_name' %in% colnames(uc_raw)){
    # attach food name if it's in the original data
    uc_tb_std <- cbind(food_name = uc_raw$food_name, 
                       uc_tb_std)
  }
  

  res <- list(uc_raw = uc_raw, 
                std_coef = std_coef,
                uc_std = uc_tb_std)
    

  return(res)
}
# example:
# coefs <- cd_constr[, c('tag_outcome', 'std_coef')]
# cd_unit_contrib_std <- makestd_unit_contrib(
#   uc_raw = cd_unit_contrib,
#   std_coef = coefs)






constr_by_tag <- function(d_unit_contrib, d_constr){
  # this should be independent of whether std or not
  # return both
  
  # d_unit_contrib <- cd_unit_contrib
  # d_constr <- cd_constr
  
  # compute contrib (std)
  # we need standardized contrib per unit
  coefs <- d_constr[, c('tag_outcome', 'std_coef')]
  d_unit_contrib_std <- makestd_unit_contrib(
    uc_raw = d_unit_contrib, # original
    std_coef = coefs)$uc_std
  
  food_names <- d_unit_contrib$food_name
  tag_outcomes <- d_constr$tag_outcome
  
  val_raw <- list()
  val_std <- list()
  for (i in 1:length(tag_outcomes)){
    
    # i <- 1
    # per unit contrib (n foods)
    # select the column with matching tag_outcome
    # d_unit_contrib[, 2]
    id <- which(colnames(d_unit_contrib) == tag_outcomes[i])
    unit_contrib_raw <- d_unit_contrib[, id]
    unit_contrib_std <- d_unit_contrib_std[, id]
    
    # lwr, upr constraint (n tag)
    id2 <- which(d_constr$tag_outcome == tag_outcomes[i])
    lwr_raw <- d_constr[id2, ]$constr_min_raw
    upr_raw <- d_constr[id2, ]$constr_max_raw
    
    lwr_std <- d_constr[id2, ]$constr_min_std
    upr_std <- d_constr[id2, ]$constr_max_std
    
    val_raw[[i]] <- list(unit_contrib = unit_contrib_raw, 
                         lwr = lwr_raw, 
                         upr = upr_raw)
    
    val_std[[i]] <- list(unit_contrib = unit_contrib_std, 
                         lwr = lwr_std, 
                         upr = upr_std)
  }
  
  names(val_raw) <- tag_outcomes
  names(val_std) <- tag_outcomes
  
  return(list(food_names = food_names, 
              tag_outcomes = tag_outcomes, 
              val_raw = val_raw, 
              val_std = val_std))
  
}


# example
# constr_by_tag(d_unit_contrib = cd_unit_contrib, 
#               d_constr = cd_constr)




f_make_objective_function <- function(diet0, method = 'ss'){
  
  if(method == 'ss'){
    # sum of square difference
    f_obj <- function(x){
      res <- sum((x - diet0)^2)
      return(res)
    }
    # if there are other methods, implement later  
    
  }
  return(f_obj) 
}


# example
# fo <- f_make_objective_function(diet0 = x0)
# fo(x = (x0-2))




f_make_constraint_function <- function(constraint_values, tag_outcomes){
  
  # constraint_values <- constval$val_std
  # tag_outcomes <- c('energy', 'ghge')
  
  
  # this is the function we want to return
  f_constr <- function (x) {
    
    energy <- constraint_values$energy
    protein <- constraint_values$protein
    carbs <- constraint_values$carbs
    fat <- constraint_values$fat
    vitaminc <- constraint_values$vitaminc
    calcium <- constraint_values$calcium
    ghge <- constraint_values$ghge
    
    # a few computed constraints, where x is the new diet
    # it should be the complete set of constrants, 
    # as we select in the last step from 
    energy_output_lwr <- - sum(x * energy$unit_contrib) + energy$lwr
    energy_output_upr <- sum(x * energy$unit_contrib) - energy$upr
    
    protein_output_lwr <- - sum(x * protein$unit_contrib) + protein$lwr
    protein_output_upr <- sum(x * protein$unit_contrib) - protein$upr
    
    carbs_output_lwr <- - sum(x * carbs$unit_contrib) + carbs$lwr
    carbs_output_upr <- sum(x * carbs$unit_contrib) - carbs$upr
    
    fat_output_lwr <- - sum(x * fat$unit_contrib) + fat$lwr
    fat_output_upr <- sum(x * fat$unit_contrib) - fat$upr
    
    vitaminc_output_lwr <- - sum(x * vitaminc$unit_contrib) + vitaminc$lwr
    vitaminc_output_upr <- sum(x * vitaminc$unit_contrib) - vitaminc$upr
    
    calcium_output_lwr <- - sum(x * calcium$unit_contrib) + calcium$lwr
    calcium_output_upr <- sum(x * calcium$unit_contrib) - calcium$upr
    
    ghge_output_lwr <- - sum(x * ghge$unit_contrib) + ghge$lwr
    ghge_output_upr <- sum(x * ghge$unit_contrib) - ghge$upr
    
    
    
    # collect in a named vector
    constr_all <- c(
      energy_lwr = energy_output_lwr, 
      energy_upr = energy_output_upr, 
      
      protein_lwr = protein_output_lwr, 
      protein_upr = protein_output_upr, 
      
      carbs_lwr = carbs_output_lwr, 
      carbs_upr = carbs_output_upr, 
      
      fat_lwr = fat_output_lwr, 
      fat_upr = fat_output_upr, 
      
      vitaminc_lwr = vitaminc_output_lwr, 
      vitaminc_upr = vitaminc_output_upr, 
      
      calcium_lwr = calcium_output_lwr, 
      calcium_upr = calcium_output_upr, 
      
      ghge_lwr = ghge_output_lwr, 
      ghge_upr = ghge_output_upr
      
    )
    
    # key step:
    # select the ones that we want, for example, tag1
    # need to watch out for the names 
    tags_lwr <- paste0(tag_outcomes, '_lwr')
    tags_upr <- paste0(tag_outcomes, '_upr')
    
    constr <- constr_all[c(tags_lwr, tags_upr)]
    
    # res <- list(constr = constr, 
    #             tags_lwr = tags_lwr, 
    #             tags_upr = tags_upr)
    
    return (constr)
  }
  
  # possibly better to also return the input
  
  return(f_constr)
}





find_new_diet <- function(diet0, 
                          diet0_upr, 
                          diet0_lwr, 
                          tag_outcomes, 
                          constraint_val, 
                          print_runtime = T){
  
  
  # initial values and lower and upper bounds of n foods 
  # probably better to always start from the current diet
  # diet0 <- cd$intake_mean
  # diet0_lwr <- cd$intake_lwr
  # diet0_upr <- cd$intake_upr
  # constraint value list
  # constraint_val <- constval$val_std 
  # tag_outcomes <- c('energy', 'protein', 'ghge')
  
  # set objective
  f_obj <- f_make_objective_function(diet0 = diet0)
  
  # set inequ constraints
  f_ineq <- f_make_constraint_function(
    constraint_values = constraint_val, 
    tag_outcomes = tag_outcomes)
  
  # number of constraints
  # two times of the number of tags
  nc <- 2*length(tag_outcomes) 
  # other options
  opts <- list( "algorithm" = "NLOPT_GN_ISRES",
                "xtol_rel"= 1.0e-15,
                "maxeval"= 160000,
                # 2*n tags
                "tol_constraints_ineq" = rep( 1.0e-10, nc))
  
  # set timer
  start_time <- Sys.time()
  # run the algorithm
  run_optim <- nloptr::nloptr(
    x0          = diet0,        # initial value for x
    eval_f      = f_obj,        # objective function
    lb          = diet0_lwr,        # lower bound for x
    ub          = diet0_upr,        # upper bound for x
    eval_g_ineq = f_ineq,       # inequality constraint
    opts        = opts          # options
  )
  
  end_time <- Sys.time()
  runtime <- end_time - start_time
  
  # return results
  if(print_runtime == T){
    res <- list(run_optim = run_optim, 
                runtime = runtime)
  }else{
    res <- list(run_optim = run_optim)
  }
  
  return(res)
}




