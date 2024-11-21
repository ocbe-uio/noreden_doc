# code to clean up the data processing 
# target: elements as in d.RData


library(readxl)
library(dplyr)
library(data.table)

## Import constraint matrix (information on amount of nutrient per g food group)
constraintmatrix <- read_excel("dev/input.xlsx")
constraintmatrix

constraintmatrix |> colnames()

# this is a list of food names, nutrient, env impact 
# not sure what the last 3 means here


## Import constraint limits (information on nutrient constraints)
#containing data frame with nutrient, constraint direction and constraint limit (rhs=right-hand side) 
limitsdata <- read_excel("dev/nutdir.xlsx") 
limitsdata



constraintsdata <- constraintmatrix %>% 
  select("Foodgroup", 
        # "Baseline" ="Means_pr10MJ",
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
         "Total red meat, maximum, g" = "Red meat",
         "Total white meat, maximum, g" = "White meat" 
  )

## Import realism constraints (information on baseline intake, mean and percentiles)
intakepercentiles <- read_excel("dev/realism.xlsx") 
intakepercentiles

baseline_intake <- intakepercentiles$Mean10MJ
upper_intake <- intakepercentiles$`95thpercentile10MJ` # Upper intake constraint
lower_intake <- intakepercentiles$`0.1xmean10MJ` # Lower intake constraint

baseline_intake
upper_intake
lower_intake


# d$Qmat >0
# as.vector(d$Qmat - Qmat) != 0 



#Objective function
#observed intake (fj):                                                                                        
#in this case observed intake can be found in constraintsdata (column 2).      
obs <- baseline_intake   
obs


# Qmat ----
# Matrix of the quadratic coefficient of the objective function "Qmat":
# Qmat is a diagonal matrix, with only values on the diagonal and 0 otherwise

obssq <- 1/obs^2
n_food <- length(obs)

Qmat <- 2*as.matrix(diag(obssq, n_food, n_food)) 



# cvec ----
# Vector of the linear coefficients of the objective function:
cvec <- -(2/obs)


# Amat ----
# Constraint matrix

t(constraintsdata)
Amat <- as.matrix(t(constraintsdata[, -c(1,2)]))

constraintsdata[, c(1,2)]


#Constraint right-hand sides and directions
bvec <- limitsdata$rhs
sense <- limitsdata$Dir   #Rcplex uses E, L, and R instead of ==, <= and >= 
bvec

#Set upper and lower bounds for realism constraints    
ub <- as.numeric(upperintake)
lb <- as.numeric(lowerintake)




