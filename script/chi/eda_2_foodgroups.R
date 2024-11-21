# examine food group pattern
# food group from data ----
# (not used)
foods_bygroup_non0 <- readRDS('data_processed/indiv_foods_group.RData')


# what is the most frequent group 
foods_bygroup_non0[, group_name] %>% table



foods_bygroup_non0[group_name == 'Fish']
foods_bygroup_non0[group_name == 'Fish']$food_name %>% table

foods_bygroup_non0[group_name == 'Cakes']
foods_bygroup_non0[group_name == 'Cakes']$food_name %>% table

foods_bygroup_non0[group_name == 'Sugar, sweets']
foods_bygroup_non0[group_name == 'Sugar, sweets']$food_name %>% unique


# 25 food groups ----
fd <- read.csv(file = './data_public/foods.csv')
fd

# food on its own ----# 

plot(fd$intake, fd$energy)
plot(fd$ghge, fd$energy)
plot(fd$intake, fd$ghge)




# food vs constraints ----- # 

foods$energy * foods$intake

# compute the contribution (indiv * intake) for 28 foods
ftotal <- apply(X = foods[, c('energy', 'protein', 'fat', 
                              'carbs', 'sugar', 'alcohol', 'ghge')], 
                MARGIN = 2, 
                FUN = function(x){x*foods$intake})

ftotal
rownames(ftotal) <- fd$food

# divide by total of all 28 (upper constraints)
fsum <- apply(ftotal, 2, sum)
fsum

fprop <- t(apply(X = ftotal, MARGIN = 1, FUN = function(x){x/fsum}))
fprop
rownames(fprop) <- fd$food

summary(fprop)


# big groups ----
ftotal

grain <- c('Bread', 'Other grains', 'Cakes')
fruit_vege <- c('Potatoes', 'Vegetables', 'Legumes', 'Fruit, berries', 
                'Juice', 'Nuts', 'Vegetarian products')
meat <- c('Red meat', 'White meat')
fish_egg <- c('Fish', 'Eggs')
dairy <- c('Cream, cream desserts', 'Milk, yoghurt', 'Cheese')
fats <- c('Butter, margarine, oil')
beverages <- c('Coffee, tea', 'Soda, saft', 'Water', 
               'Alcoholic beverages', 'Non-dairy milk')
sugar_other <- c('Sugar, sweets', 'Snacks', 'Sauces', 'Spices', 'Other')




# ______ visualization ______ -----

# barplot(fd$ghge)
# barplot(ftotal[, 7])
library(ggplot2)
library(ggrepel)
library(RColorBrewer)

# require some data manip
# need big food group, food name (smaller food group)
pdt <- data.frame(ftotal) # total 
pdt$food_name <- row.names(pdt)

# attach big group
data.table::setDT(pdt)
pdt[food_name %in% grain, big_group := 'grain']
pdt[food_name %in% fruit_vege, big_group := 'fruit_vege']
pdt[food_name %in% meat, big_group := 'meat']
pdt[food_name %in% fish_egg, big_group := 'fish_egg']
pdt[food_name %in% dairy, big_group := 'dairy']
pdt[food_name %in% fats, big_group := 'fats']
pdt[food_name %in% beverages, big_group := 'beverages']
pdt[food_name %in% sugar_other, big_group := 'sugar_other']



pdt_long <- tidyr::pivot_longer(pdt, cols = -c(food_name, big_group), names_to = 'category')
pdt_long <- data.table::setDT(pdt_long)

# try to reorder food names 
names_ordered <- c(grain, fruit_vege, meat, fish_egg, dairy, fats, beverages, sugar_other)
# names_ordered

# pdt_long_copy <- copy(pdt_long)
pdt_long$food_name_order <- factor(pdt_long$food_name, 
                                   levels = names_ordered, 
                                   labels = names_ordered)
# pdt_long$food_name
# pdt_long$food_name_order

# p1 ----

p1 <- ggplot(data = pdt_long[category == 'energy'], 
             aes(x = food_name_order, y = value, fill = big_group))
p1 <- p1 + geom_bar(stat = 'identity')
p1 <- p1 + coord_flip()
p1 <- p1 + theme_bw()
p1 <- p1 + scale_fill_brewer(palette = 'Dark2')
# p1 + facet_wrap(~big_group)
p1 <- p1 + labs(title = 'Energy (multiplied by food intake)', 
                x = 'Food groups', 
                y = 'Energy')
p1 <- p1 + theme(axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 12), 
                   plot.title = element_text(size = 20))
p1



# p2 ----
p2 <- ggplot(data = pdt_long[category == 'ghge'], 
             aes(x = food_name_order, y = value, fill = big_group))
p2 <- p2 + geom_bar(stat = 'identity')
p2 <- p2 + coord_flip()
p2 <- p2 + theme_bw()
p2 <- p2 + scale_fill_brewer(palette = 'Dark2')
p2 <- p2 + labs(title = 'GHGE (multiplied by food intake)', 
                x = 'Food groups', 
                y = 'GHGE')
p2 <- p2 + theme(axis.text = element_text(size = 12), 
                 axis.title = element_text(size = 12), 
                 plot.title = element_text(size = 20))

p2







# p3 ----
# energy vs ghge
plot(ftotal[, 1], ftotal[,  7])


pdfd <- data.table::setDT(fd)
# remove water and coffee
pdfd <- pdfd[food != 'Water']
pdfd

# attach group 
pdfd[food %in% grain, big_group := 'grain']
pdfd[food %in% fruit_vege, big_group := 'fruit_vege']
pdfd[food %in% meat, big_group := 'meat']
pdfd[food %in% fish_egg, big_group := 'fish_egg']
pdfd[food %in% dairy, big_group := 'dairy']
pdfd[food %in% fats, big_group := 'fats']
pdfd[food %in% beverages, big_group := 'beverages']
pdfd[food %in% sugar_other, big_group := 'sugar_other']




p3 <- ggplot(data = pdfd, 
             aes(x = energy, y = ghge, size = intake, 
                 label = food, color = big_group))
p3 <- p3 + geom_point(alpha = 0.3) + xlim(-3, 30) + ylim(-0.002, 0.015)
p3 <- p3 + scale_size(range = c(0.1, 20))
p3 <- p3 + geom_text_repel(size = 4, max.overlaps = 15)
# p3 <- p3 + geom_text(size = 3, check_overlap = T)
p3 <- p3 + theme_bw()
p3 <- p3 + scale_color_brewer(palette = 'Dark2')
p3 <- p3 + labs(title = 'Energy vs GHGE', 
                x = 'Energy (per unit)', 
                y = 'GHGE (per unit)')
p3



# percentage ----
fprop
pdfp <- data.frame(fprop) # total 
pdfp$food_name <- row.names(pdfp)

# attach big group
data.table::setDT(pdfp)
pdfp[food_name %in% grain, big_group := 'grain']
pdfp[food_name %in% fruit_vege, big_group := 'fruit_vege']
pdfp[food_name %in% meat, big_group := 'meat']
pdfp[food_name %in% fish_egg, big_group := 'fish_egg']
pdfp[food_name %in% dairy, big_group := 'dairy']
pdfp[food_name %in% fats, big_group := 'fats']
pdfp[food_name %in% beverages, big_group := 'beverages']
pdfp[food_name %in% sugar_other, big_group := 'sugar_other']



pdfp_long <- tidyr::pivot_longer(pdfp, cols = -c(food_name, big_group), names_to = 'category')
pdfp_long <- data.table::setDT(pdfp_long)

# also add orders here
pdfp_long$food_name_order <- factor(pdfp_long$food_name, 
                                   levels = names_ordered, 
                                   labels = names_ordered)


p4 <- ggplot(data = pdfp_long[category %in% c('energy', 'protein', 'fat', 'carbs','ghge')], 
             aes(x = food_name_order, y = value, fill = big_group))
p4 <- p4 + geom_bar(stat = 'identity')
p4 <- p4 + coord_flip()
p4 <- p4 + facet_wrap(~category, ncol = 5)
p4 <- p4 + scale_fill_brewer(palette = 'Dark2')
p4 <- p4 + labs(title = 'Proportion of food contribution',
                subtitle = 'Each category sums up to 1',
                x = 'Food groups', 
                y = 'Percentage')
p4 <- p4 + theme_bw()
p4 <- p4 + theme(axis.text = element_text(size = 10), 
                 axis.title = element_text(size = 10), 
                 plot.title = element_text(size = 15), 
                 strip.text = element_text(size = 12), 
                 legend.position = 'none')
p4





# spider plot ----

# install.packages('fmsb')
# Library
library(fmsb)

# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )
data

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)
data

# Check your data, it has to look like this!
# head(data)

# The default radar chart 
radarchart(data)

devtools::install_github("ricardo-bion/ggradar")
library(ggradar)

data
ggradar(
  data[3, ], 
  values.radar = c("0", "10", "20"),
  grid.min = 0, grid.mid = 10, grid.max = 20
)




