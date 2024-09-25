# load results and process 

library(readxl)
library(data.table)


d <- read_excel("~/Documents/Data/uio_norkost/result/food_0925/spade_d_dairy_portion_b_18_80.xlsx", 
                      sheet = "HI")
d




# food -----
# path_food <- '~/Documents/Data/uio_norkost/result/spade_food/'
path_food <- '~/Documents/Data/uio_norkost/result/food_0925/'

file_list_food <- list.files(path = path_food, pattern = '*.xlsx')
file_list_food_abs <- paste0(path_food, file_list_food)

read_excel(file_list_food_abs[5], sheet = 'HI')

food_res_list <- lapply(file_list_food_abs, 
                        function(x){
                          read_excel(x, sheet = 'HI')
                        })
# take the first row, drop first column
food_res_list[[1]]

d1 <- food_res_list[[1]][1,]
round(d1$P95, digits = 4)

food_threshold <- sapply(food_res_list, function(x){x[['EAR1']][1]})
food_threshold_perc <- sapply(food_res_list, function(x){x[['EAR1.p']][1]})


# manually set NA
food_threshold[[36]] <- 0
food_threshold[[37]] <- 0
food_threshold[[38]] <- 0
food_threshold_perc[[36]] <- 0
food_threshold_perc[[37]] <- 0
food_threshold_perc[[38]] <- 0

food_threshold <- unlist(food_threshold)
food_threshold_perc <- unlist(food_threshold_perc)
# food_threshold_perc <- sapply(food_res_list, function(x){round(x[['EAR1.p']][1], digits = 3)})
# food_threshold <- sapply(food_res_list, function(x){round(x[['EAR1']][1])})
# add arithmetic mean
food_am <- sapply(food_res_list, function(x){round(x[['AM']][1], digits = 3)})

# process names
file_list_food
# stringr::str_split_fixed("spade_d_whole_grain_m_18_80.xlsx", '_', 7)
# stringr::str_split_fixed(file_list_food, '\\.', 2)
# substring('spade_d_fish_fatty_f_18_80.xlsx', 
#           first = 9, 
#           last = 20)


short_names_food <- lapply(file_list_food, 
       function(x){
         substring(x, 
                   first = 9, 
                   last = (nchar(x)-11)) 
       }) |> unlist()


food_res_df <- data.frame(short_names_food, 
                          average = food_am,
                          food_threshold, 
                          percentage = round(food_threshold_perc, 3))


# nutrient no supplements ----

path_nutnosup <- '~/Documents/Data/uio_norkost/result/spade_nosup/'

file_list_nutnosup <- list.files(path = path_nutnosup, pattern = '*.xlsx')
file_list_nutnosup_abs <- paste0(path_nutnosup, file_list_nutnosup)


read_excel(file_list_nutnosup_abs[5], sheet = 'HI')

nutnosup_res_list <- lapply(file_list_nutnosup_abs, 
                        function(x){
                          read_excel(x, sheet = 'HI')
                        })
# take the first row, drop first two columns
nutnosup_res_list[[1]]


nutnosup <- lapply(nutnosup_res_list, function(x){x[1, 3:7]})
nutnosup <- do.call(rbind, nutnosup) |> data.frame()
nutnosup <- round(nutnosup, digits = 3)



file_list_nutnosup

short_names_nutnosup <- lapply(file_list_nutnosup, 
                           function(x){
                             substring(x, 
                                       first = 9, 
                                       last = (nchar(x)-11)) 
                           }) |> unlist()


nutnosup_res_df <- data.frame(short_names_nutnosup, 
                              nutnosup)






# nutrient w supplements ----

path_nutwsup <- '~/Documents/Data/uio_norkost/result/spade_wsup/'

file_list_nutwsup <- list.files(path = path_nutwsup, pattern = '*.xlsx')
file_list_nutwsup_abs <- paste0(path_nutwsup, file_list_nutwsup)


read_excel(file_list_nutwsup_abs[5], sheet = 'HI')

nutwsup_res_list <- lapply(file_list_nutwsup_abs, 
                            function(x){
                              read_excel(x, sheet = 'HI')
                            })
# take the first row, drop first two columns
nutwsup_res_list[[1]]


nutwsup <- lapply(nutwsup_res_list, function(x){x[1, 3:7]})
nutwsup <- do.call(rbind, nutwsup) |> data.frame()
nutwsup <- round(nutwsup, digits = 3)



file_list_nutwsup

short_names_nutwsup <- lapply(file_list_nutwsup, 
                               function(x){
                                 substring(x, 
                                           first = 9, 
                                           last = (nchar(x)-11)) 
                               }) |> unlist()


nutwsup_res_df <- data.frame(short_names_nutwsup, 
                              nutwsup)



# nutrient 0925 ----

path_nut0925 <- '~/Documents/Data/uio_norkost/result/nut_0925/'

file_list_nut0925 <- list.files(path = path_nut0925, pattern = '*.xlsx')
file_list_nut0925_abs <- paste0(path_nut0925, file_list_nut0925)

read_excel(file_list_nut0925_abs[5], sheet = 'HI')

nut0925_res_list <- lapply(file_list_nut0925_abs, 
                           function(x){
                             read_excel(x, sheet = 'HI')
                           })
# take the first row, drop first two columns
nut0925_res_list[[1]]


nut0925 <- lapply(nut0925_res_list, function(x){x[1, 3:7]})
nut0925 <- do.call(rbind, nut0925) |> data.frame()
nut0925 <- round(nut0925, digits = 3)


short_names_nut0925 <- lapply(file_list_nut0925, 
                              function(x){
                                substring(x, 
                                          first = 9, 
                                          last = (nchar(x)-11)) 
                              }) |> unlist()


nut0925_res_df <- data.frame(short_names_nut0925, 
                             nut0925)

# save results ----
?write.xlsx
library(openxlsx)

write.xlsx(food_res_df, '~/Documents/Data/uio_norkost/result/result_food_0925.xlsx')
write.xlsx(nutnosup_res_df, '~/Documents/Data/uio_norkost/result/result_nosupplement_sep4.xlsx')
write.xlsx(nutwsup_res_df, '~/Documents/Data/uio_norkost/result/result_wsupplement_sep4.xlsx')

write.xlsx(nut0925_res_df, '~/Documents/Data/uio_norkost/result/result_nutrient_0925.xlsx')



