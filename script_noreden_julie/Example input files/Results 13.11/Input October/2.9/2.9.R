
# MINIMIZING SQUARED RELATIVE DEVIATION FROM CURRENT DIET FOR WHOLE POPULATION

######### Clear environment #########

rm(list=ls())

######### Load packages and set working directory #########

library(readxl)
library(dplyr)
library(Rcplex)

setwd("~/Documents/Input October/2.9") 

######### Read in data #########

## Import constraint matrix (information on amount of nutrient per g food group)
constraintmatrix <- read_excel("Input 2.9.xlsx")

## Import constraint limits (information on nutrient constraints)
limitsdata <- read_excel("Nutrient dir 2.9.xlsx") #containing data frame with nutrient, constraint direction and constraint limit (rhs=right-hand side) 

## Select relevant columns and rename them
constraintsdata <- constraintmatrix %>% 
  select("Foodgroup", 
         "Baseline" ="Means_pr10MJ",
         "Energy (MJ)" = "Energy (MJ)", 
         "Protein, g, lower" = "Protein1", 
         "Protein, g, upper" = "Protein2", 
         "Carbohydrates, g, lower"="Available carbohydrates1", 
         "Carbohydrates, g, upper"="Available carbohydrates2",
         "Added sugar, g"="Added Sugar",
         "Dietary fiber, g"= "Dietary fiber", 
         "Fat, g, lower"= "Fat1",  
         "Fat, g, upper"= "Fat2",
         "Saturated fatty acids, g"="Sum saturated fatty acids",
         "Trans fatty acids, g"="Sum trans fatty acids", #Added
         "n-3 fatty acids, g"="Sum n-3 fatty acids",
         "ALA, g" = "Sum ALA",
         "MUFA, g, lower" = "Sum monounsaturated fatty acids1",
         "MUFA, g, upper" = "Sum monounsaturated fatty acids2",
         "PUFA, g, lower" = "Sum polyunsaturated fatty acids1",
         "PUFA, g, upper" = "Sum polyunsaturated fatty acids2",
         "Vitamin A, RE µg"= "Vitamin A",
         "Vitamin E, alfa-TE"= "Vitamin E",
         "Thiamin (Vitamin B1), mg"= "Thiamin (Vitamin B1)",
         "Riboflavin (Vitamin B2), mg"= "Riboflavin (Vitamin B2)",
         "Niacin, NE"="Niacin equivalent",
         "Vitamin B6, mg"="Vitamin B6",
         "Folate, µg"="Folate",
         "Vitamin B12, µg"="Vitamin B12",
         "Vitamin C, mg"="Vitamin C",
         "Vitamin D, µg" = "Vitamin D",
         "Sodium, mg"="Sodium",
         "Potassium, mg"="Potassium",
         "Calcium, mg"="Calcium",
         "Magnesium, mg"= "Magnesium",
         "Phosphorus, mg"="Phosphorus",
         "Iron, mg"="Iron",
         "Zinc, mg"="Zinc",
         "Iodine, µg"="Iodine",
         "Selenium, µg"="Selenium",
         "Copper, µg"="Copper",
         "Alcohol, g" = "Alcohol",
         "GHGE, kg CO2-eq" = "GHGE",
         "FE, g P-eq" = "FE",
         "ME, g N-eq" = "ME",
         "TA, g SO2-eq" = "ACID",
         "WU, m2" = "WU",
         "LU, m3a" = "LU",
         "Whole grains, g" = "Whole grains",
         "Total fruit, g" = "Fruit",
         "Total vegetables, g" = "Vegetables",
         "Total dairy, minimum, g" = "Dairy1",
         "Total dairy, maximum, g" = "Dairy2",
         "Total fish, minimum, g" = "Fish1",
         "Total fish, maximum, g" = "Fish2",
         "Total red meat, maximum, g" = "Red meat" 
         )

## Import realism constraints (information on baseline intake, mean and percentiles)
intakepercentiles <- read_excel("Realism 2.9.xlsx") 
upperintake <- t(intakepercentiles[,"95thpercentile10MJ"]) # Upper intake constraint
lowerintake <-t(intakepercentiles[,"0.1xmean10MJ"]) # Lower intake constraint

######### Assign the objective function #########

#Objective function
#observed intake (fj):                                                                                        
#in this case observed intake can be found in constraintsdata (column 2).      
obs <- t(constraintsdata[,"Baseline"])    
obs

#Matrix of the quadratic coefficient of the objective function "Qmat":
obssq <- as.vector(1/obs^2)
Qmat <- 2*as.matrix(diag(obssq,53,53)) #edit to fit number of food groups

#Vector of the linear coefficients of the objective function:
cvec <- as.vector(-(2/obs))


######################### Problem 1: Nutri #########################
# Model with nutritional constraints + realism constraints

######### Set constraints #########

#Constraint matrix
Amat <- as.matrix(t(constraintsdata[, -c(1,2)]))

#Constraint right-hand sides and directions
bvec <- limitsdata$rhs
sense <- limitsdata$Dir   #Rcplex uses E, L, and R instead of ==, <= and >= 
bvec

#Set upper and lower bounds for realism constraints    
ub <- as.numeric(upperintake)
lb <- as.numeric(lowerintake)

####### Solve problem 1 (run model) ###########

problem_1 <- Rcplex(cvec, Amat, bvec, Qmat, lb=lb, ub= ub, objsense ="min", sense=sense) 

####### Interpret results ###########

##Create chart to compare food composition of diets (g of each food group in new vs baseline diet, percent change between diets, lower and upper intake limits aka realism constraints)
Nut <- round(problem_1$xopt, digits=1) #New diet name (in this case 'Nut' aka Nutrition, since this scenario includes only nutritional and realism constraints)
Baseline <- round(constraintsdata[,2],digits=1) #Baseline diet
percentchange <- round((Nut - Baseline)/Baseline, digits=3) #Percent change between new diet and baseline diet

diet_result <- data.frame(
  Foodgroup = c('Bread fine', 'Bread coarse', 'Flour grains', 'Rice', 'Pasta', 'Breakfast cereals', 'Cakes cookies crackers', 'Potatoes raw boiled', 'Potatoes fried', 'Legumes','Vegetables dark green', 'Vegetables red orange', 'Vegetables other', 'Vegetables salad', 'Nuts', 'Seeds', 'Juice', 'Berries', 'Pomme stone', 'Fruit other', 'Dried fruit', 'Meat dairy substitutes', 'Ruminant meat unprocessed', 'Ruminant meat processed', 'Pork unprocessed', 'Pork processed', 'Red meat mixed processed', 'White meat unprocessed', 'White meat processed', 'Other meat unprocessed', 'Other meat processed', 'Fish lean', 'Fish fatty', 'Shellfish other', 'Eggs', 'Milk low fat', 'Milk high fat', 'Dairy fermented', 'Dairy other', 'Cheese fresh', 'Cheese brown', 'Cheese other', 'Fats plant based', 'Fats animal based', 'Water', 'Coffee tea', 'Soft drinks sugar sweetened', 'Soft drinks sugar free', 'Alcoholic beverages', 'Sugar sweets', 'Snacks', 'Spices', 'Sauces miscellaneous'),
  Baseline = Baseline, 
  Nut = Nut, 
  percent_change = percentchange,
  lower_limit = lb, 
  upper_limit = ub
)

colnames(diet_result) <- c('Foodgroup','Baseline', 'New', 'Percent_change', 'Lower_limit', 'Upper_limit')
diet_result

##Create chart to compare nutrient content of diets (content of nutrients in new vs baseline diet, upper and lower nutrient constraints, relative deviation from constraint)
output_newdiet <- t(as.matrix(Nut)) %*% as.matrix(constraintsdata[,3:54])

#The previously imported nutrient constraints file has notation specific to Rcplex. The results will be easier to interpret if we import a file with nutrient constraints (upper and lower) in a format that is familiar to us.
cstr <- read_excel("Nutrient ref 2.9.xlsx")
cstr1 <- cstr %>% #Select and rename relevant columns
  select("Energy (MJ)",
         "Protein, g, lower" = "Protein1", 
         "Protein, g, upper" = "Protein2", 
         "Carbohydrates, g, lower"="Available carbohydrates1", 
         "Carbohydrates, g, upper"="Available carbohydrates2",
         "Added sugar, g"="Added Sugar",
         "Dietary fiber, g"= "Dietary fiber", 
         "Fat, g, lower"= "Fat1",  
         "Fat, g, upper"= "Fat2",
         "Saturated fatty acids, g"="Sum saturated fatty acids",
         "Trans fatty acids, g"="Sum trans fatty acids", #Added
         "n-3 fatty acids, g"="Sum n-3 fatty acids",
         "ALA, g" = "Sum ALA",
         "MUFA, g, lower" = "Sum monounsaturated fatty acids1",
         "MUFA, g, upper" = "Sum monounsaturated fatty acids2",
         "PUFA, g, lower" = "Sum polyunsaturated fatty acids1",
         "PUFA, g, upper" = "Sum polyunsaturated fatty acids2",
         "Vitamin A, RE µg"= "Vitamin A",
         "Vitamin E, alfa-TE"= "Vitamin E",
         "Thiamin (Vitamin B1), mg"= "Thiamin (Vitamin B1)",
         "Riboflavin (Vitamin B2), mg"= "Riboflavin (Vitamin B2)",
         "Niacin, NE"="Niacin equivalent",
         "Vitamin B6, mg"="Vitamin B6",
         "Folate, µg"="Folate",
         "Vitamin B12, µg"="Vitamin B12",
         "Vitamin C, mg"="Vitamin C",
         "Vitamin D, µg" = "Vitamin D",
         "Sodium, mg"="Sodium",
         "Potassium, mg"="Potassium",
         "Calcium, mg"="Calcium",
         "Magnesium, mg"= "Magnesium",
         "Phosphorus, mg"="Phosphorus",
         "Iron, mg"="Iron",
         "Zinc, mg"="Zinc",
         "Iodine, µg"="Iodine",
         "Selenium, µg"="Selenium",
         "Copper, µg"="Copper",
         "Alcohol, g" = "Alcohol",
         "GHGE, kg CO2-eq" = "GHGE",
         "FE, g P-eq" = "FE",
         "ME, g N-eq" = "ME",
         "TA, g SO2-eq" = "ACID",
         "WU, m2" = "WU",
         "LU, m3a" = "LU",
         "Whole grains, g" = "Whole grains",
         "Total fruit, g" = "Fruit",
         "Total vegetables, g" = "Vegetables",
         "Total dairy, minimum, g" = "Dairy1",
         "Total dairy, maximum, g" = "Dairy2",
         "Total fish, minimum, g" = "Fish1",
         "Total fish, maximum, g" = "Fish2",
         "Total red meat, maximum, g" = "Red meat"  
  )
library("data.table")

#Create chart and assign column names and row names
const_result <- t(rbind(output_newdiet, cstr1))
colnames(const_result) <- c('new_diet','const_lwr', 'const_upr')
nutnames <- c("Energy (MJ)",
              "Protein, g, upper", 
              "Protein, g, lower",
              "Carbohydrates, g, upper", 
              "Carbohydrates, g, lower", 
              "Added sugar, g",
              "Dietary fiber, g", 
              "Fat, g, upper",  
              "Fat, g, lower",
              "Saturated fatty acids, g",
              "Trans fatty acids, g",
              "n-3 fatty acids, g",
              "ALA, g",
              "MUFA, g, upper",
              "MUFA, g, lower",
              "PUFA, g, upper",
              "PUFA, g, lower",
              "Vitamin A, RE µg",
              "Vitamin E, alfa-TE",
              "Thiamin (Vitamin B1), mg",
              "Riboflavin (Vitamin B2), mg",
              "Niacin, NE",
              "Vitamin B6, mg",
              "Folate, µg",
              "Vitamin B12, µg",
              "Vitamin C, mg",
              "Vitamin D, µg",
              "Sodium, mg",
              "Potassium, mg",
              "Calcium, mg",
              "Magnesium, mg",
              "Phosphorus, mg",
              "Iron, mg",
              "Zinc, mg",
              "Iodine, µg",
              "Selenium, µg",
              "Copper, µg",
              "Alcohol, g",
              "GHGE, kg CO2-eq",
              "FE, g P-eq",
              "ME, g N-eq",
              "TA, g SO2-eq",
              "WU, m2",
              "LU, m3a",
              "Whole grains, g",
             "Total fruit, g",
         "Total vegetables, g",
         "Total dairy, minimum, g",
         "Total dairy, maximum, g",
         "Total fish, minimum, g",
         "Total fish, maximum, g",
         "Total red meat, maximum, g"
)
const_result <- data.table(const_result)
const_result[,':=' (nutrient =nutnames)]

#Check if results meet constraints
const_result[, is_ok := 'Yes']
const_result[new_diet < const_lwr, is_ok := 'beyond lower']
const_result[new_diet > const_upr, is_ok := 'beyond upper']
const_result[, relative_dev := 0]
const_result[is_ok == 'beyond lower', relative_dev := round((new_diet - const_lwr)/const_lwr, 3)]
const_result[is_ok == 'beyond upper', relative_dev := round((new_diet - const_upr)/const_upr, 3)]
const_result
const_result2 <-const_result[,c(4,1,2,3,5,6)]

currentintake <- as.vector(t(constraintsdata[,"Baseline"]))

dietdeparture <-     ((Nut[1]-currentintake[1])/currentintake[1])^2 +
  ((Nut[2]-currentintake[2])/currentintake[2])^2 +
  ((Nut[3]-currentintake[3])/currentintake[3])^2 +
  ((Nut[4]-currentintake[4])/currentintake[4])^2 +
  ((Nut[5]-currentintake[5])/currentintake[5])^2 +
  ((Nut[6]-currentintake[6])/currentintake[6])^2 +                     
  ((Nut[7]-currentintake[7])/currentintake[7])^2 +                     
  ((Nut[8]-currentintake[8])/currentintake[8])^2 +                     
  ((Nut[9]-currentintake[9])/currentintake[9])^2 +                     
  ((Nut[10]-currentintake[10])/currentintake[10])^2 +                     
  ((Nut[11]-currentintake[11])/currentintake[11])^2 +                     
  ((Nut[12]-currentintake[12])/currentintake[12])^2 +                     
  ((Nut[13]-currentintake[13])/currentintake[13])^2 +                     
  ((Nut[14]-currentintake[14])/currentintake[14])^2 +                     
  ((Nut[15]-currentintake[15])/currentintake[15])^2 +                     
  ((Nut[16]-currentintake[16])/currentintake[16])^2 +                     
  ((Nut[17]-currentintake[17])/currentintake[17])^2 +                     
  ((Nut[18]-currentintake[18])/currentintake[18])^2 +                     
  ((Nut[19]-currentintake[19])/currentintake[19])^2 +                     
  ((Nut[20]-currentintake[20])/currentintake[20])^2 +                     
  ((Nut[21]-currentintake[21])/currentintake[21])^2 +     
  ((Nut[22]-currentintake[22])/currentintake[22])^2 +
  ((Nut[23]-currentintake[23])/currentintake[23])^2 +
  ((Nut[24]-currentintake[24])/currentintake[24])^2 +
  ((Nut[25]-currentintake[25])/currentintake[25])^2 +                     
  ((Nut[26]-currentintake[26])/currentintake[26])^2 +                     
  ((Nut[27]-currentintake[27])/currentintake[27])^2 +                     
  ((Nut[28]-currentintake[28])/currentintake[28])^2 +                     
  ((Nut[29]-currentintake[29])/currentintake[29])^2 +                     
  ((Nut[30]-currentintake[30])/currentintake[30])^2 +                     
  ((Nut[31]-currentintake[31])/currentintake[31])^2 +                     
  ((Nut[32]-currentintake[32])/currentintake[32])^2 +                     
  ((Nut[33]-currentintake[33])/currentintake[33])^2 +                     
  ((Nut[34]-currentintake[34])/currentintake[34])^2 +                     
  ((Nut[35]-currentintake[35])/currentintake[35])^2 +                     
  ((Nut[36]-currentintake[36])/currentintake[36])^2 +                     
  ((Nut[37]-currentintake[37])/currentintake[37])^2 +                     
  ((Nut[38]-currentintake[38])/currentintake[38])^2 +                     
  ((Nut[39]-currentintake[39])/currentintake[39])^2 +                     
  ((Nut[40]-currentintake[40])/currentintake[40])^2 +    
  ((Nut[41]-currentintake[41])/currentintake[41])^2 +
  ((Nut[42]-currentintake[42])/currentintake[42])^2 +
  ((Nut[43]-currentintake[43])/currentintake[43])^2 +
  ((Nut[44]-currentintake[44])/currentintake[44])^2 +                     
  ((Nut[45]-currentintake[45])/currentintake[45])^2 +                     
  ((Nut[46]-currentintake[46])/currentintake[46])^2 +   
  ((Nut[47]-currentintake[47])/currentintake[47])^2 +  
  ((Nut[48]-currentintake[48])/currentintake[48])^2 

Number <- 1     
dietframe <- data.frame (Number, dietdeparture)



redmeatdeparture <- 
(((Nut[23] + Nut[24] + Nut[25] + Nut[26] + Nut[27])-(currentintake[23] + currentintake[24] +currentintake[25] +currentintake[26] + currentintake[27]))/(currentintake[23] + currentintake[24] +currentintake[25] +currentintake[26] + currentintake[27])) * 100

 Number2 <- 1     
dietframe2 <- data.frame (Number2, redmeatdeparture)
       

totalmeatdeparture <- 
(((Nut[23] + Nut[24] + Nut[25] + Nut[26] + Nut[27] + Nut[28] + Nut[29] + Nut[30] + Nut[31])-(currentintake[23] + currentintake[24] +currentintake[25] +currentintake[26] + currentintake[27] + currentintake[28] + currentintake[29] + currentintake[30] + currentintake[31]))/(currentintake[23] + currentintake[24] +currentintake[25] +currentintake[26] + currentintake[27] + currentintake[28] + currentintake[29] + currentintake[30] + currentintake[31])) * 100
 
 Number3 <- 1     
dietframe3 <- data.frame (Number3, totalmeatdeparture)

ruminantmeatdeparture <-
((((Nut[23] + Nut[24] + (0.5 * Nut[27])))-(currentintake[23] + currentintake[24] + (0.5 * currentintake[27])))/(currentintake[23] + currentintake[24] + (0.5 * currentintake[27]))) * 100
 
Number4 <- 1
dietframe4 <-  data.frame (Number4, ruminantmeatdeparture)

sink(file="2.9 Meat reduction NutriHealthGHGE40.txt")
#Print out
const_result2 #Check nutrient constraints
diet_result #Check food amounts in new diet
dietdeparture
ruminantmeatdeparture
redmeatdeparture
totalmeatdeparture
sink(file=NULL)


library("writexl")
write_xlsx(list("Nutrients" = const_result2, "Diet" = diet_result, "Diet departure" = dietframe, "Ruminant meat departure" = dietframe4, "Red meat departure" = dietframe2, "Total meat departure" = dietframe3), "~/Documents/Input October/2.9/2.9 Result.xlsx")

