# examine food group pattern

foods_bygroup_non0 <- readRDS('data_processed/indiv_foods_group.RData')


# what is the most frequent group 
foods_bygroup_non0[, group_name] %>% table



foods_bygroup_non0[group_name == 'Fish']
foods_bygroup_non0[group_name == 'Fish']$food_name %>% table

foods_bygroup_non0[group_name == 'Cakes']
foods_bygroup_non0[group_name == 'Cakes']$food_name %>% table

foods_bygroup_non0[group_name == 'Sugar, sweets']
foods_bygroup_non0[group_name == 'Sugar, sweets']$food_name %>% unique





