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

# reduce ghge to 0.8

constr_coef_df <- set_constr_coef(tag_outcome = tag_outcome_9, 
                                  coef_lwr = rep(0.9, length(tag_outcome_9)), 
                                  coef_upr = rep(1.0, length(tag_outcome_9)))


constr_coef_df_red <- reduce_constr(data_constr_coef = constr_df, 
                                    tag_outcome_reduce = 'ghge', 
                                    coef_reduce = 0.9)



# ____ STEP 2: COMPUTE CONSTR ____ ----

# we supply the entire dataset for all nutrients when computing the constraints
# need to specify reduction factor on ghge 


# d_diet

# filter(d_diet, food_name %in% tag_food)
# filter(d_perunit_contrib, food_name %in% tag_food)


diet_s <- select_diet(data_diet = d_diet,
                      tag_food = tag_food_9)

puc_s <- select_perunit(data_perunit_contrib = d_perunit_contrib, 
                        tag_food = tag_food_9, 
                        tag_outcome = tag_outcome_9)

# compute total contrib for selected foods and tag_outcomes
tc <- total_contrib(data_diet = diet_s, 
                    data_perunit_contrib = puc_s)

# compute constraints
cd_constr_raw_red <- compute_constr(data_total_contrib = tc$total_contrib, 
                                    data_constr_coef = constr_coef_df_red)


# standardized total contrib (sd)
stdcoef_9 <- get_stdcoef(data_perunit_contrib = d_perunit_contrib)

coefs <- stdcoef_9$std_coef

cd_unit_contrib_std <- makestd_unit_contrib(
  uc_raw = puc_s,
  std_coef = coefs)
puc_s_std <- cd_unit_contrib_std$uc_std

tc_std <- total_contrib(data_diet = diet_s, 
                        data_perunit_contrib = puc_s_std)


# alternatively, directly use tc multiply by coef
# tc$total_contrib$total_contrib * coefs$std_coef
# ok 




# first compute the basis (i.e. maximum total contrib)
# inequality constraints are based on this value

# the constraints are based on the current diet contribution
# for reduction (e.g. ghge) just set lower upper bound 



cd_constr_std <- compute_constr(data_total_contrib = tc_std$total_contrib, 
               data_constr_coef = constr_coef_df)


cd_constr_std_red <- compute_constr(data_total_contrib = tc_std$total_contrib, 
               data_constr_coef = constr_coef_df_red)




# ____ STEP 3: ALGO ____ ----
# require 
# nutri_pu (per unit contribution) or envir_pu
# lwrupr_energy (computed as constraints)


# demo_input <- readRDS('./data_processed/demo_9foods_input.rda')
# demo_constraints <- readRDS('./data_processed/demo_9foods_constraints.rda')


# hot fix:
# colnames(demo_constraints)[
#   which(colnames(demo_constraints)=='cosntr_max_std')] <- 'constr_max_std'
# these constraints need to have an additional column that has the reduction 


# in this example: 9 foods, 7 constraints (might not need to be used for all)



# set param ----- 
# cd: current diet
# cd <- as.data.frame(demo_input$current_diet)
# cd_unit_contrib <- as.data.frame(demo_input$unit_contrib)
# cd_constr <- as.data.frame(demo_constraints)


cd <- diet_s

# diet: 
# initial values
# probably better to always start from the current diet
x0 <- cd$intake_mean

# lower and upper bounds of x (10 foods)
lb <- cd$intake_lwr
ub <- cd$intake_upr


# unit contrib
cd_unit_contrib_raw <- puc_s
cd_unit_contrib_std <- puc_s_std


# constraints: 
# by tag (e.g. energy, protein)
constval <- values_by_tag_outcome(data_unit_contrib = cd_unit_contrib_std, 
                                  data_constr = cd_constr_std)


constval$food_name
constval$tag_outcome
constval$val
constval$val$energy





# set objective ----

fo <- f_make_objective_function(diet0 = x0)

# fo(x = (x0-2))


# set ineq constraints ----
# in this way, the datainfo can also be scaled/unscaled.
# tags <- c('energy', 'protein')
tags <- c('energy', 'protein', 'ghge')

f_inequalc <- f_make_constraint_function(
  constraint_values = constval$val, 
  tag_outcomes = tags)

# f_inequalc(x = x0)
# f_inequalc
# f_inequalc <- f_factory(namelist = c('ene1', 'ene2'), datainfo = info_energy)



# run ----

res <- find_new_diet(diet0 = cd$intake_mean, 
                     diet0_upr = cd$intake_upr, 
                     diet0_lwr = cd$intake_lwr, 
                     tag_outcomes = c('energy', 'protein', 'ghge'), 
                     constraint_val = constval$val, 
                     print_runtime = T)

res


diet_s


new_diet <- return_new_diet(result_obj = res$run_optim, data_current_diet = diet_s)
compare_new_diet(data_new_diet = new_diet, data_current_diet = diet_s)



validate_diet_contrib(data_new_diet = new_diet, 
                      data_unit_contrib = puc_s,
                      data_constr = cd_constr_raw_red)



# _________ ----
#  utility  ----

select_diet <- function(data_diet, tag_food, minmax = T){
  
  diet_selected <- dplyr::filter(data_diet, food_name %in% tag_food) |> 
    dplyr::select(dplyr::all_of(c('food_name','intake_mean')))
  
  if(minmax == T){
    
    diet_selected <- dplyr::filter(data_diet, food_name %in% tag_food) |> 
      dplyr::select(dplyr::all_of(c('food_name',
                                    'intake_mean', 
                                    'intake_lwr', 
                                    'intake_upr')))
    
  }
  
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


set_constr_coef <- function(tag_outcome, coef_lwr, coef_upr){
  
  constr_coef <- data.frame(tag_outcome = tag_outcome, 
                            coef_constrlwr = coef_lwr, 
                            coef_construpr = coef_upr)
  
  return(constr_coef)
}


reduce_constr <- function(data_constr_coef, tag_outcome_reduce, coef_reduce){
  
  # data_constr_coef <- constr_df
  # tag_outcome_reduce <- 'ghge'
  # coef_reduce <- 0.8  
  
  id <- which(data_constr_coef$tag_outcome == tag_outcome_reduce)
  
  # multiply by a factor
  data_constr_coef[id, ]$coef_constrlwr <- 
    data_constr_coef[id, ]$coef_constrlwr *coef_reduce
  
  data_constr_coef[id, ]$coef_construpr <- 
    data_constr_coef[id, ]$coef_construpr *coef_reduce
  
  return(data_constr_coef)
  
}





compute_total_contrib <- function(data_diet, 
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



compute_stdcoef <- function(data_perunit_contrib, 
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



compute_std_unit_contrib <- function(uc_raw, std_coef){
  
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






compute_constr <- function(data_total_contrib, 
                           data_constr_coef){
  
  # data_total_contrib <- tc_std$total_contrib
  # data_constr_coef <- constr_df
  
  # check if names are consistent
  if(sum(data_total_contrib$tag_outcome %in% data_constr_coef$tag_outcome) !=
     nrow(data_constr_coef)){
    stop('tag_outcome do not match, check')
  }
  
  d <- dplyr::left_join(data_total_contrib, data_constr_coef, by = 'tag_outcome')
  # multiply
  d$constr_lwr <- d$total_contrib * d$coef_constrlwr
  d$constr_upr <- d$total_contrib * d$coef_construpr
  
  
  return(d)
}







values_by_tag_outcome <- function(data_unit_contrib, data_constr){
  # this should be independent of whether std or not
  
  # data_unit_contrib <- cd_unit_contrib_std
  # data_constr <- cd_constr_std
  
  food_name <- data_unit_contrib$food_name
  tag_outcome_uc <- colnames(data_unit_contrib)[colnames(data_unit_contrib)!='food_name']
  tag_outcome_constr <- data_constr$tag_outcome
  
  if(sum(tag_outcome_uc %in% tag_outcome_constr) != length(tag_outcome_uc)){
    stop('tag_outcome do not match. check')
  }
  
  to <- tag_outcome_uc
  
  val <- list()
  for (i in 1:length(to)){
    
    # i <- 1
    # per unit contrib (n foods)
    # select the column with matching tag_outcome
    # d_unit_contrib[, 2]
    id <- which(colnames(data_unit_contrib) == to[i])
    unit_contrib <- data_unit_contrib[, id]
    
    # lwr, upr constraint (n tag)
    id2 <- which(data_constr$tag_outcome == to[i])
    lwr <- data_constr[id2, ]$constr_lwr
    upr <- data_constr[id2, ]$constr_upr
    
    val[[i]] <- list(unit_contrib = unit_contrib, 
                     lwr = lwr, 
                     upr = upr)
  }
  
  names(val) <- to
  
  return(list(food_name = food_name, 
              tag_outcome = to, 
              val = val))
  
}


# example
# values_by_tag_outcome(data_unit_contrib = cd_unit_contrib_std, 
#                       data_constr = cd_constr_std)




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





return_new_diet <- function(result_obj, data_current_diet){
  
  new_diet <- result_obj$solution
  
  res <- data.frame(food_name = data_current_diet$food_name, 
                    new = new_diet, 
                    current = data_current_diet$intake_mean)
  return(res)
}

compare_new_diet <- function(data_new_diet, 
                             data_current_diet){
  
  # data_current_diet <- diet_s
  # data_new_diet <- new_diet
  # check current diet names, should have mean and lwr upr
  coln_input <- c('food_name', 'intake_mean', 'intake_lwr', 'intake_upr')
  if(sum(coln_input %in% colnames(data_current_diet)) != length(coln_input)){
    stop('Must supply food_name, mean and lwr upr in the current diet data')
  }
  # remove current, since we add it back from the other table
  data_new_diet <- dplyr::select(data_new_diet, -current)
  
  d <- dplyr::left_join(data_new_diet, data_current_diet, by = 'food_name')
  
  d <- dplyr::rename(d, 
                     current = intake_mean, 
                     current_lwr = intake_lwr, 
                     current_upr = intake_upr)
  d <- dplyr::mutate(d, 
                     abs_change = (new - current), 
                     perc_change = (new - current)/current)
  
  return(d)
}



validate_diet_contrib <- function(data_new_diet, 
                                  data_unit_contrib, 
                                  data_constr){
  
  # on original scale is fine
  # data_new_diet <- new_diet
  # data_unit_contrib <- puc_s
  
  # data_constr <- cd_constr_raw_red # original
  
  # compute new total contri 
  # consistent name
  d <- dplyr::select(data_new_diet, 
                     food_name,
                     intake_mean = new)
  tc_new <- compute_total_contrib(data_diet = d, 
                                  data_perunit_contrib = data_unit_contrib)
  
  d_tc_new <- tc_new$total_contrib
  d_tc_new <- dplyr::rename(d_tc_new, total_contrib_new = total_contrib)
  
  # put together, modify name
  tc_both <- dplyr::left_join(d_tc_new, data_constr, by = 'tag_outcome')
  
  tc_both <- mutate(tc_both, check = dplyr::case_when(
    total_contrib_new > constr_upr ~ 'beyond_upper', 
    total_contrib_new < constr_lwr ~ 'beyond_lwr',
    .default = 'ok'
  ))
  
  tc_both <- mutate(tc_both, deviation = dplyr::case_when(
    check == 'beyond_upper' ~ round((total_contrib_new - constr_upr)/constr_upr,3), 
    check == 'beyond_lower' ~ round((total_contrib_new - constr_lwr)/constr_lwr,3), 
    .default = 0
  ))
  
  return(tc_both)
  
}


