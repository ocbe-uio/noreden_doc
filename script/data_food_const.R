# make data frame for constraints

# table 1, food info ----
food <- data.frame(
  food_group = c('bread',  'vegetables', 'fruit_berry', 
                 'meat', 'fish', 'milk_yogurt'), 
  energy = c(10.6956, 1.5653, 2.7289, 7.7432, 6.0863, 1.9797), 
  protein = c(0.0912, 0.0149, 0.0076, 0.1800, 0.1698, 0.0359), 
  fat = c(0.0302, 0.0084, 0.0041, 0.1211, 0.0748, 0.0111), 
  carb = c(0.4413, 0.0498, 0.1341, 0.0113, 0.0245, 0.0559), 
  ghge = c(0.0011, 0.0010, 0.0007, 0.0110, 0.0031, 0.0014)
)

# add current diet consumption (g)
food$current_diet_gram <- c(175.4, 154.6, 171.5, 151.1, 69.5, 306.1)




# table 2, constraints ----

constraints <- data.frame(
  constraint = c('lower', 'upper'), 
  energy = c(9000, 10000), 
  protein = c(55, 111.5), 
  fat = c(61.8, 98.8), 
  carb = c(250, 334.6), 
  ghge = c(0, 4.7)
)
# constraints




