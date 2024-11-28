# pseudomise the data so it is not the original copy
# load the original 
library(readxl)
library(dplyr)
library(data.table)


# constraint ----
## Import constraint matrix (information on amount of nutrient per g food group)
constraintmatrix <- read_excel("dev/input.xlsx")
constraintmatrix

constraintmatrix |> colnames()

# this is a list of food names, nutrient, env impact 
# not sure what the last 3 means here

head(constraintmatrix)

cm <- copy(constraintmatrix)
# from 3:56, multiply by 1.05

for (i in 3:56){
  cm[, i] <- cm[, i]*1.05
}

cm |> head()
constraintmatrix |> head()

# remove food group id 
cm[, 'id'] <- NULL

# save 

writexl::write_xlsx(cm, path = 'data_public/contrib_per_unit.xlsx', col_names = T)




# limits -----
# Import constraint limits (information on nutrient constraints)
# containing data frame with nutrient, constraint direction and constraint limit 
# (rhs=right-hand side) 
limitsdata <- read_excel("dev/nutdir.xlsx") 
limitsdata

# multiple by 1.05
limitsdata$rhs <- limitsdata$rhs*1.05
# save

# rename, as not all are nutrients 
colnames(limitsdata)[1] <- 'tag_outcome'


writexl::write_xlsx(limitsdata, path = 'data_public/constraints.xlsx', col_names = T)




# intake_perc ----
## Import realism constraints (information on baseline intake, mean and percentiles)
intakepercentiles <- read_excel("dev/realism.xlsx") 
intakepercentiles <- data.table(intakepercentiles)


intake <- copy(intakepercentiles)

# set names 
setnames(intake, old = 'Mean10MJ', new = 'mean')
setnames(intake, old = '0.1xmean10MJ', new = 'lower_bound')
setnames(intake, old = '95thpercentile10MJ', new = 'upper_bound')

# make it 1.05 times what it was
coef <- 1.05
intake$mean <- coef * intake$mean
intake$lower_bound <- coef * intake$lower_bound
intake$upper_bound <- coef * intake$upper_bound

# save as excel 
# with a different name


writexl::write_xlsx(intake, path = 'data_public/bounds.xlsx', col_names = T)












