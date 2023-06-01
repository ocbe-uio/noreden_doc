# understand feasibility of the current diet 

# data for foods defined at: 
# source("./script/data_food_const.R")
head(foods)
setDT(foods)

foods$energy %*% foods$intake # 9314
foods$protein %*% foods$intake # 98.22
foods$fat %*% foods$intake # 85.76
foods$carbs %*% foods$intake # 234.72
foods$ghge %*% foods$intake # 3.78

foods$energy * foods$intake


t(as.matrix(foods$intake)) %*% as.matrix(foods[, .(energy, protein, fat, carbs, ghge)])



