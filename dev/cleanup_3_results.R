# nutrition ref is wide 


# load results 

results <- readRDS('~/Documents/GitHub/noreden_doc/data_public/result.RData')
results |> str()



# files needed during cleaning up: 
intake <- read_excel("data_public/bounds.xlsx") 
intake

# nutrition reference
nutref <- read_excel("data_public/nutref.xlsx")

# contstraints
constraints <- read_excel("data_public/constraints.xlsx") 
constraints |> head()


# result table -----
##Create chart to compare food composition of diets 
# (g of each food group in new vs baseline diet, 
# percent change between diets, 
# lower and upper intake limits aka realism constraints)


new_intake <- round(results$xopt, digits=1) 
# New diet name (in this case 'new_intake' aka Nutrition, 
# since this scenario includes only nutritional and realism constraints)

# Baseline <- round(constraintsdata[,2],digits=1) #Baseline diet
intake_baseline <- round(intake$mean, digits=1) #Baseline diet
intake_upr <- intake$upper_bound
intake_lwr <- intake$lower_bound
#lb <- intake$lower_bound
#ub <- intake$upper_bound


percent_change <- round((new_intake - Baseline)/Baseline, digits=3) #Percent change between new diet and baseline diet
percent_change

foodgroup <- intake$Foodgroup

diet_result <- data.frame(
  foodgroup = foodgroup,
  baseline = intake_baseline, 
  new_intake = new_intake, 
  percent_change = percentchange,
  lower_limit = intake_lwr, 
  upper_limit = intake_upr
)

head(diet_result)



# nutrition ref ----


output_newdiet <- t(as.matrix(new_intake)) %*%  as.matrix(cpu[,2:54])

# nutref

# Select and rename relevant columns
# this step can be omitted if the nutref file has consistent names

cstr <- nutref |> 
  dplyr::select("Energy (MJ)",
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


#Create chart and assign column names and row names
const_result <- t(rbind(output_newdiet, cstr))
colnames(const_result) <- c('new_diet','const_lwr', 'const_upr')
head(const_result)


# 
nutnames <- constraints$tag_outcome

const_result <- data.table(const_result)
const_result[, nutrient := nutnames]

#Check if results meet constraints
const_result[, is_ok := 'Yes']
const_result[new_diet < const_lwr, is_ok := 'beyond lower']
const_result[new_diet > const_upr, is_ok := 'beyond upper']
const_result[, relative_dev := 0]
const_result[is_ok == 'beyond lower', relative_dev := round((new_diet - const_lwr)/const_lwr, 3)]
const_result[is_ok == 'beyond upper', relative_dev := round((new_diet - const_upr)/const_upr, 3)]

head(const_result)



# compute departure ----

currentintake <- intake_baseline

head(diet_result)

# sum(diet_result$percent_change^2) # slightly different numbers

# departure
diet_departure <- ((diet_result$new_intake - diet_result$baseline)/diet_result$baseline)^2 |> sum()
diet_departure


# red meat departure
# select necessary data rows 
redmeat <- diet_result[c(23, 24, 25, 26, 27),]

redmeat_departure <- ((sum(redmeat$new_intake) - sum(redmeat$baseline))/sum(redmeat$baseline))*100
redmeat_departure

# can also use a function: 
f_meat_departure(meat_data = redmeat)

# total meat departure

totalmeat <- diet_result[c(23, 24, 25, 26, 27, 28, 29, 30, 31),]

f_meat_departure(meat_data = totalmeat)


# ruminant meat departure

ruminantmeat <- diet_result[c(23, 24),]
f_meat_departure(meat_data = ruminantmeat)





# function 
f_meat_departure <- function(meat_data){

  meat_departure <- ((sum(meat_data$new_intake) - sum(meat_data$baseline))/sum(meat_data$baseline))*100
  return(meat_departure)
}



