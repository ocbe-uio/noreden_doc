# match food groups 
library(data.table)
library(tidyr)


# food group reference -----
group_names <- readxl::read_excel("data_raw/Food groups.xlsx", skip = 1)
group_foods <- readxl::read_excel("data_raw/Food groups.xlsx", 
                                  sheet = "Report", skip = 3)

# group names
# 25 groups
group_names <- data.table(group_names)
group_names

# group with individual foods 
group_foods <- data.table(group_foods)
group_foods

# need a little bit procesing: long format data

food_group_uniq <- group_foods[!is.na(`Group name`)]
# get row id 
rowid <- which(!is.na(group_foods$`Group name`)) 
nrow(group_foods) # 2694+1 
# need an end point
rowid <- c(rowid, nrow(group_foods)+1)
rowid

n_repeatfoods <- diff(rowid)


# 1:188
# 189:365 (or 366-189)
# tail(group_foods)
# group_foods[187:191]
# group_foods[365:367]

name_list <- list()
for(i in 1:25){
  name_list[[i]] <- rep(food_group_uniq$Group[i], n_repeatfoods[i])
  cat(paste0('Creating ', n_repeatfoods[i], food_group_uniq$`Group name`[i],  '\n'))
}
name_vec <- unlist(name_list)
name_vec

# merge with individual food group 

group_foods$group_id <- name_vec
# check
group_foods[group_id == 4]

# merge with name 
food_ref <- group_names[, .(`Number:`, `Name:`)]
food_ref
setnames(food_ref, 'Number:', 'group_id')
setnames(food_ref, 'Name:', 'group_name')
food_ref

food_group_ref <- merge.data.table(group_foods, food_ref, by = 'group_id')
# remove duplicate columns 

food_group_ref[, Group := NULL]
food_group_ref[, `Group name` := NULL]
food_group_ref <- food_group_ref[!is.na(`Food names`)]
food_group_ref

# look good
# set food names
setnames(food_group_ref, 'Food codes', 'food_id')
setnames(food_group_ref, 'Food names', 'food_name')
food_group_ref

# save 
saveRDS(group_food_ref, 'data_processed/food_group_ref.RData')





# indiv foods -----

# use individual food category (wide format)

indiv_foods <- readRDS('data_processed/indiv_foods.RData')
# 1508 col
indiv_foods[, 1:4]

# need long format 
indiv_foods_long <- pivot_longer(data = indiv_foods, 
                                 cols = -Nr, 
                                 names_to = 'food_id', 
                                 values_to = 'value')

indiv_foods_long <- data.table(indiv_foods_long)
setnames(indiv_foods_long, 'Nr', 'subject_id')

# class(indiv_foods_long)

# link group -----
group_food_ref <- readRDS('data_processed/food_group_ref.RData')
group_food_ref

indiv_foods_bygroup <- merge.data.table(indiv_foods_long, group_food_ref,
                                        by = 'food_id')

indiv_foods_bygroup



# possibly remove empty cells

indiv_foods_bygroup_non0 <- indiv_foods_bygroup[value>0]
saveRDS(indiv_foods_bygroup_non0, 'data_processed/indiv_foods_group.RData')












