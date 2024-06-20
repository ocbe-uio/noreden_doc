# norkost 4 data

library(data.table)
library(ggplot2)


norkost4 <- readxl::read_excel("~/Documents/Data/uio_norkost/norkost4.xlsx")
norkost4 <- data.table(norkost4)

norkost4
colnames(norkost4)

# rename variables

setnames(norkost4, old = 'Fullk', new = 'whole_grain')
setnames(norkost4, old = 'Sum_frukt_grønt_max100g_juice', new = 'fruit_veg')
setnames(norkost4, old = 'Sum_alt_rødt_kjøtt_vektendret', new = 'red_meat')
setnames(norkost4, old = 'Bearbeidet_rødt_hvitt_kjøtt_vektendret', new = 'processed_meat')
setnames(norkost4, old = 'All_fisk_vektendret', new = 'fish_total')
setnames(norkost4, old = 'Fet_fisk_vektendret', new = 'fish_fatty')
setnames(norkost4, old = 'Vegetabilske_oljer', new = 'veg_oil')
setnames(norkost4, old = 'Sum_fettred_melk_yoghurt', new = 'milk_dairy')
setnames(norkost4, old = 'NØTUSALT_stoffgruppering', new = 'nuts')

# demographics
setnames(norkost4, old = 'Kjønn', new = 'sex')
setnames(norkost4, old = 'Alder_v_første_kont', new = 'age')
setnames(norkost4, old = 'ID', new = 'Nr') # to be consistent

# set sex: 1 male 2 female
norkost4[sex == 1, sexc := 'male']
norkost4[sex == 2, sexc := 'female']



# summary stat ----
# do some basic summary on the foods

# whole grain: >90
intake_summary(dt = norkost4, foodname = 'whole_grain')
vis_distribution(data = norkost4, foodname = 'whole_grain')
vis_point(data = norkost4, foodname = 'whole_grain')

# fruit veg: > 500 or 800
# very much unfulfilled
intake_summary(dt = norkost4, foodname = 'fruit_veg')
vis_distribution(data = norkost4, foodname = 'fruit_veg')
vis_point(data = norkost4, foodname = 'fruit_veg')

# red meat
# <50
intake_summary(dt = norkost4, foodname = 'red_meat')
vis_distribution(data = norkost4, foodname = 'red_meat')
vis_point(data = norkost4, foodname = 'red_meat')


# processed meat
# minimal, 0
intake_summary(dt = norkost4, foodname = 'processed_meat')
vis_distribution(data = norkost4, foodname = 'processed_meat')
vis_point(data = norkost4, foodname = 'processed_meat')


# fish total
# >43, or 64
intake_summary(dt = norkost4, foodname = 'fish_total')
vis_distribution(data = norkost4, foodname = 'fish_total')
vis_point(data = norkost4, foodname = 'fish_total')


# fish fatty
# >29
intake_summary(dt = norkost4, foodname = 'fish_fatty')
vis_distribution(data = norkost4, foodname = 'fish_fatty')
vis_point(data = norkost4, foodname = 'fish_fatty')



# nuts
# > 20, 30
intake_summary(dt = norkost4, foodname = 'nuts')
vis_distribution(data = norkost4, foodname = 'nuts')
vis_point(data = norkost4, foodname = 'nuts')


# veg oil
# >25
intake_summary(dt = norkost4, foodname = 'veg_oil')
vis_distribution(data = norkost4, foodname = 'veg_oil')
vis_point(data = norkost4, foodname = 'veg_oil')


# milk dairy
# >350
intake_summary(dt = norkost4, foodname = 'milk_dairy')
vis_distribution(data = norkost4, foodname = 'milk_dairy')
vis_point(data = norkost4, foodname = 'milk_dairy')


# save 
write.csv(norkost4, file = '~/Documents/Data/uio_norkost/norkost4_spade.csv')






