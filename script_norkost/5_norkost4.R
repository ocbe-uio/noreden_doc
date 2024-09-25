# norkost 4 data (foods)
library(readxl)
library(data.table)
library(ggplot2)

norkost4 <- read_excel("~/Documents/Data/uio_norkost/norkost4_0621.xlsx", 
                       sheet = "Inntak N4")
# norkost4 <- readxl::read_excel("~/Documents/Data/uio_norkost/norkost4.xlsx")
norkost4 <- data.table(norkost4)

norkost4
colnames(norkost4)



# ________ ----
# sep 11 ----
# additional analysis: milk dairy, fish
# merge into the same dataframe?
fish_2409 <- read_excel("~/Documents/Data/uio_norkost/fish_2409.xlsx")


# merge data
norkost4 <- dplyr::left_join(norkost4, fish_2409)
colnames(norkost4)
head(norkost4)

# ________ ----
# 2024.9.25 ----

meat_2409 <- read_excel("~/Documents/Data/uio_norkost/milk_meat_2409.xlsx")
head(meat_2409)
# drop milk
meat_2409 <- meat_2409[, -4]
colnames(meat_2409)[1] <- 'Nr' # need to change


# milk use this one
milk_2409 <- read_excel("~/Documents/Data/uio_norkost/milk_2409.xlsx")
head(milk_2409)
colnames(milk_2409)[2] <- 'Nr' # need to change ID to Nr


# merge data
norkost4 <- dplyr::left_join(norkost4, meat_2409)
norkost4 <- dplyr::left_join(norkost4, milk_2409)

colnames(norkost4)




# rename variables

setnames(norkost4, old = 'Fullkorn', new = 'whole_grain')
setnames(norkost4, old = 'Frukt_og_grønt_max_100_g_juice', new = 'fruit_veg')
setnames(norkost4, old = 'Rødt_kjøtt_vektendret', new = 'red_meat')
setnames(norkost4, old = 'Bearbeidet_rødt_hvitt_kjøtt_vektendret', new = 'processed_meat')
setnames(norkost4, old = 'Ren_fisk_totalt_vektendret', new = 'fish_total')
setnames(norkost4, old = 'Fet_fisk_totalt_vektendret', new = 'fish_fatty')
# setnames(norkost4, old = 'Vegetabilske_oljer', new = 'veg_oil')
setnames(norkost4, old = 'Sum_fettred_melk_yoghurt', new = 'milk_dairy')
setnames(norkost4, old = 'Usaltede_nøtter', new = 'nuts_nosalt')
setnames(norkost4, old = 'Alle_nøtter', new = 'nuts_all')

# two new: 9.11
setnames(norkost4, old = 'All fisk vektendret', new = 'fish_2409')
setnames(norkost4, old = 'Porsjoner meieri', new = 'dairy_portion')


# two new: 9.25
setnames(norkost4, old = 'Meieriporsjoner', new = 'dairy_240925')
setnames(norkost4, old = 'Bearbeidet kjøtt', new = 'procmeat_240925')



# demographics
setnames(norkost4, old = 'Løpedag', new = 'round')
setnames(norkost4, old = 'Kjønn', new = 'sex')
setnames(norkost4, old = 'Alder_v_første_kont', new = 'age')
# setnames(norkost4, old = 'ID', new = 'Nr') # to be consistent

class(norkost4)

# set sex: 1 male 2 female
norkost4[sex == 1, sexc := 'male']
norkost4[sex == 2, sexc := 'female']



# summary stat ----
# do some basic summary on the foods


# whole grain ----
# whole grain: >90
intake_summary(dt = norkost4, foodname = 'whole_grain')
# sum(norkost4$whole_grain > 90) 
sum(norkost4$whole_grain > 90)/nrow(norkost4)

plt <- vis_distribution(data = norkost4, foodname = 'whole_grain')
plt <- plt + geom_vline(xintercept = 90)
plt <- plt + labs(title = 'whole grain: >90')
plt

vis_point(data = norkost4, foodname = 'whole_grain')



# fruit veg ----
# fruit veg: > 500 or 800
# very much unfulfilled
intake_summary(dt = norkost4, foodname = 'fruit_veg')

sum(norkost4$fruit_veg > 500)/nrow(norkost4)

plt <- vis_distribution(data = norkost4, foodname = 'fruit_veg')
plt <- plt + geom_vline(xintercept = 500)
plt <- plt + geom_vline(xintercept = 800)
plt <- plt + labs(title = 'fruit veg: >500, 800')
plt

vis_distribution(data = norkost4, foodname = 'fruit_veg')
vis_point(data = norkost4, foodname = 'fruit_veg')



# red meat ----
# red meat
# <50
intake_summary(dt = norkost4, foodname = 'red_meat')

plt <- vis_distribution(data = norkost4, foodname = 'red_meat')
plt <- plt + geom_vline(xintercept = 50)
plt <- plt + labs(title = 'red meat: <50')
plt

vis_point(data = norkost4, foodname = 'red_meat')



# processed meat ----
# processed meat
# minimal, 0
intake_summary(dt = norkost4, foodname = 'processed_meat')
vis_distribution(data = norkost4, foodname = 'processed_meat')
vis_point(data = norkost4, foodname = 'processed_meat')



# fish total ----
# fish total
# >43, or 64
#sum(norkost4$fish_total < 43) 
sum(norkost4$fish_total < 43)/nrow(norkost4)

#sum(norkost4$fish_total < 64) 
sum(norkost4$fish_total < 64)/nrow(norkost4)

intake_summary(dt = norkost4, foodname = 'fish_total')
vis_distribution(data = norkost4, foodname = 'fish_total')
plt <- vis_distribution(data = norkost4, foodname = 'fish_total')
plt <- plt + geom_vline(xintercept = 43)
plt <- plt + geom_vline(xintercept = 64)

plt <- plt + labs(title = 'fish total: >43, 64')
plt

vis_point(data = norkost4, foodname = 'fish_total')



# fish fatty ----
# fish fatty
# >29

sum(norkost4$fish_fatty < 29)/nrow(norkost4)

intake_summary(dt = norkost4, foodname = 'fish_fatty')
vis_distribution(data = norkost4, foodname = 'fish_fatty')
plt <- vis_distribution(data = norkost4, foodname = 'fish_fatty')
plt <- plt + geom_vline(xintercept = 29)

plt <- plt + labs(title = 'fish fatty: >29')
plt


vis_point(data = norkost4, foodname = 'fish_fatty')



# nuts ----
# > 20, 30
head(norkost4)
intake_summary(dt = norkost4, foodname = 'nuts_all')
vis_distribution(data = norkost4, foodname = 'nuts_all')
plt <- vis_distribution(data = norkost4, foodname = 'nuts_all')
plt <- plt + geom_vline(xintercept = 20)
plt <- plt + geom_vline(xintercept = 30)
plt <- plt + labs(title = 'nuts_all: >20, 30')
plt

vis_point(data = norkost4, foodname = 'nuts_all')


# veg oil ----
# >25
#intake_summary(dt = norkost4, foodname = 'veg_oil')
#vis_distribution(data = norkost4, foodname = 'veg_oil')
#vis_point(data = norkost4, foodname = 'veg_oil')


# milk dairy ----
# >350
intake_summary(dt = norkost4, foodname = 'milk_dairy')
vis_distribution(data = norkost4, foodname = 'milk_dairy')

plt <- vis_distribution(data = norkost4, foodname = 'milk_dairy')
plt <- plt + geom_vline(xintercept = 350)
plt <- plt + labs(title = 'milk_dairy: >350')
plt

vis_point(data = norkost4, foodname = 'milk_dairy')


# ______ ----
# save ----
write.csv(norkost4, file = '~/Documents/Data/uio_norkost/norkost4_spade.csv')

norkost4














