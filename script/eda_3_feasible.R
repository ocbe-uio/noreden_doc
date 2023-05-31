# understand feasibility of the current diet 

# data for foods defined at: 
# source("./script/data_food_const.R")

food$energy %*% food$current_diet_gram
food$protein %*% food$current_diet_gram
food$carb %*% food$current_diet_gram
food$fat %*% food$current_diet_gram
food$ghge %*% food$current_diet_gram

food$energy * food$current_diet_gram

