# test code for noreden 3 project


library(readxl)
library(data.table)

nut_per_g <- read_excel("~/Documents/Data/uio_noreden/noreden3/Format 1.xlsx", 
                        sheet = "Food nutrients per g")

grouping <- read_excel("~/Documents/Data/uio_noreden/noreden3/Format 1.xlsx", 
                        sheet = "Groupings and digestibility")

subject_eg <- read_excel("~/Documents/Data/uio_noreden/noreden3/Format 1.xlsx", 
                        sheet = "Intake foods")


setDT(nut_per_g)
setDT(grouping)
setDT(subject_eg)


nut_per_g
grouping
# 30 groups, 2831 foods
# food are less than the nut_per_g
hist(grouping$`food group id`) # uneven


nut_per_g[code == 1023]
grouping[code == 1023]


# subject_eg[code == 1023] # not necessarily 

# check what food has subjects consumed

subject_eg[`1001` != 0]
subject_eg[`1003` != 0]
subject_eg[`1004` != 0]
subject_eg[`1005` != 0]


# they have consumed different foods

# verify final product ----
# check the food groups for 1001

# grouping[code %in% c(102, 5812, 8403)]
# 4, 16, 19
# bread, cheese, beverages

# for 1003

food_code_1003 <- subject_eg[`1003` != 0, code]
group_dig_1003 <- grouping[code %in% food_code_1003]

# 4, 9, 16, 19
# bread, vegetables, cheese, beverages
# check the nutrients per food

nutrient_1003 <- nut_per_g[code %in% food_code_1003]
consumption_1003 <- subject_eg[`1003` != 0, .(code, name, `1003`)]

# digestibility factor is constant per food group 

group_dig_1003 # metadata
nutrient_1003 # metadata

consumption_1003

# single food: 18g 149 (bread)
18*nutrient_1003[1, -c(1,2)]
18*nutrient_1003[1, -c(1,2)]*0.9
# without the digestive factor, the numbers match

# multiple food in the same group
# easier to merge the tables

a <- merge(group_dig_1003, consumption_1003)
# might be better if transpose the nutrient table 
nutrient_1003

# TO DO: need to have a skeleton to match the food order before multiplying
consumption_nut_1003 <- a[, `1003`] * nutrient_1003[, -c(1,2)]

b <- cbind(group_dig_1003[,1], consumption_nut_1003)
b


b[, .(energy_group = sum(energy)), by = `food group id`]



# ____________ ----
# data processing ----


library(readxl)
library(data.table)

metadata <- read_excel("~/Documents/Data/uio_noreden/noreden3/Format 1.xlsx", 
                        sheet = "Food nutrients per g")

grouping <- read_excel("~/Documents/Data/uio_noreden/noreden3/Format 1.xlsx", 
                       sheet = "Groupings and digestibility")

intake <- read_excel("~/Documents/Data/uio_noreden/noreden3/Format 1.xlsx", 
                         sheet = "Intake foods")


setDT(metadata)
setDT(grouping)
setDT(intake)

metadata |> nrow() # 4031
grouping |> nrow() # 2831
intake |> nrow() # 1223


# sort the intake by id, easier to debug
grouping <- grouping[order(code)]
intake <- intake[order(code)]

# rename 
setnames(grouping, old = 'food group id', new = 'food_group_id')
setnames(grouping, old = 'digestibility factor', new = 'dig_factor')


# attach food group to the right of metadata
dt <- merge(metadata, grouping)
dt

# select the subset from nut_per_g, matching food ID
# dt[code %in% intake$code]
# if only want energy, use this
# dt[code %in% intake$code, .(code, name, energy)]

# multiplication
# it is more readable if using a for loop


# d[[nut_id]] * d[[as.character(subject_id)]]
# merge data, as this automatically checks the food ID matching
d <- merge(dt, intake)

# select subjects 
# note that they need to be characters, not numbers
id <- c(1001, 1003, 1004, 1005)
idc <- as.character(id)
idc


# energy ----

nut_id <- 'energy'

# compute energy per food
# this line selects the energy column, and multiply by intake 
nut_mat <- d[[nut_id]] * d[, ..idc]

# check that they are not all zeros
# the code below does column-wise summation
d[, ..idc] |> apply(2, sum)
nut_mat |> apply(2, sum)

# attach back the food group, need to do group-wise summation
nut_mat_w_group <- cbind(food_group_id = d$food_group_id, nut_mat)
nut_mat_w_group

# one subject by one subject
nut_mat_w_group[, .(energy_group = sum(`1001`)), 
                by = food_group_id]

# by multiple subjects
nut_mat_w_group[, lapply(.SD, sum, na.rm=TRUE), 
                by = food_group_id, 
                .SDcols = idc]



# ----- with digestive factor: one outcome ------

# ids are the same as before, extend as needed
# if you only have numeric ID, need to make them into characters

id <- c(1001, 1003, 1004, 1005)
idc <- as.character(id)
idc

# start with protein
# extend to multiple

nut_id <- 'prot'
nut_mat <- d[[nut_id]] * d[, ..idc]

# attach back the food group
# also the digestive factor, but attach afterwards
nut_mat_w_group <- cbind(food_group_id = d$food_group_id,
                         nut_mat)
nut_mat_w_group

# group sum by multiple subjects
# the results here match the one in Format2.xlsx!
nut_group <- nut_mat_w_group[, lapply(.SD, sum, na.rm=TRUE), 
                by = food_group_id, 
                .SDcols = idc]
nut_group

# with digestive factor, remove duplicate rows
dig_factor <- unique(d[, .(food_group_id, dig_factor)])
dig_factor

# merge with the computed table
nut_group_w_df <- merge(nut_group, dig_factor)

# weighted sum
nut_group_w_df_per_foodgroup <- nut_group_w_df[, ..idc] * nut_group_w_df[["dig_factor"]]
nut_group_w_df_per_foodgroup |> apply(2, sum)
# this is the result for protein, after adjusting for the digestive factor


# multiple outcomes ----
# put the above code into a function

f_nut_digfactor <- function(subject_id_char, 
                            d, 
                            nut_id){
  
  # subject_id_char <- c('1001', '1003', '1004', '1005')
  # d <- d
  # nut_id <- 'prot'
  # compute outcome per food
  nut_mat <- d[[nut_id]] * d[, ..idc]
  
  # attach back the food group
  # also the digestive factor, but attach afterwards
  nut_mat_w_group <- cbind(food_group_id = d$food_group_id,
                           nut_mat)
  # nut_mat_w_group
  
  # group sum by multiple subjects
  nut_group <- nut_mat_w_group[, lapply(.SD, sum, na.rm=TRUE), 
                               by = food_group_id, 
                               .SDcols = idc]
  
  # with digestive factor, remove duplicate rows
  dig_factor <- unique(d[, .(food_group_id, dig_factor)])
  
  nut_group_w_df <- merge(nut_group, dig_factor)
  
  # weighted sum
  nut_group_w_df_per_foodgroup <- nut_group_w_df[, ..idc] * nut_group_w_df[["dig_factor"]]
  res <- nut_group_w_df_per_foodgroup |> apply(2, sum)
  
  
  return(list(nut_id = nut_id, 
              subject_id = subject_id_char, 
              res = res))
  
}



# test for one outcome ---
# nut_id <- 'prot'
#id <- c(1001, 1003, 1004, 1005)
#subject_id_char <- as.character(id)

# note that data d needs to exist
subject_id_char <- c('1001', '1003', '1004', '1005')

# test for 'ala'
test <- f_nut_digfactor(
  subject_id_char = subject_id_char,
  d = d,
  nut_id = 'ala')
test




# run 3 outcomes ----
nut_id_list <- c('prot', 'ala', 'arg')

res_list <- list()

for(i in 1:length(nut_id_list)){
  # compute for the ith item
  res_list[[i]] <- f_nut_digfactor(
    subject_id_char = subject_id_char,
    d = d,
    nut_id = nut_id_list[i])
  
  cat('Computing for outcome: ', nut_id_list[i], '\n')
}

# check results
res_list

# gather the results 
library(purrr)
res_df <- map_df(res_list, function(x){x$res})
res_df

# combine with the outcome names
res_df <- cbind(outcome = nut_id_list,
                res_df)
res_df

