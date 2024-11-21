# process aggregated food groups 

# raw data:
# library(readxl)
# aggregated_foods <- read_excel("data_raw/aggregated_foods.xlsx")
# library(magrittr)
# library(data.table)


# manipulation
colnames(aggregated_foods) <- c('food_group', 'intake','energy', 
                                'protein', 'fat', 'carbs', 
                                'sugar', 'alcohol', 'ghge')

aggregated_foods <- data.table(aggregated_foods)

# keep 3 digits
# aggregated_foods$energy <- round(aggregated_foods$energy, digits = 3)

aggregated_foods[, energy := round(energy, 3)]
aggregated_foods[, protein := round(protein, 3)]
aggregated_foods[, fat := round(fat, 3)]
aggregated_foods[, carbs := round(carbs, 3)]
aggregated_foods[, sugar := round(sugar, 3)]
aggregated_foods[, alcohol := round(alcohol, 3)]
aggregated_foods[, ghge := round(ghge, 3)]

foods <- aggregated_foods

write.csv(foods, file = './data_public/foods.csv', row.names = F)




