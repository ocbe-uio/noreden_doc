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


# ______ visualization ______ -----
# p1 ----

# barplot(fd$ghge)
# barplot(ftotal[, 7])
library(ggplot2)
library(ggrepel)

# require some data manip
# need big food group, food name (smaller food group)
pdt <- data.frame(ftotal) # total 
pdt$food_name <- row.names(pdt)
pdt_long <- tidyr::pivot_longer(pdt, cols = -food_name, names_to = 'category')
pdt_long <- setDT(pdt_long)


p1 <- ggplot(data = pdt_long[category == 'energy'], 
             aes(x = food_name, y = value))
p1 <- p1 + geom_bar(stat = 'identity')
p1 <- p1 + coord_flip()
p1 <- p1 + theme_bw()
p1 <- p1 + labs(title = 'Energy (food intake)', 
                x = 'Food groups', 
                y = 'Energy')
p1


# p2 ----
p2 <- ggplot(data = pdt_long[category == 'ghge'], 
             aes(x = food_name, y = value))
p2 <- p2 + geom_bar(stat = 'identity')
p2 <- p2 + coord_flip()
p2 <- p2 + theme_bw()
p2 <- p2 + labs(title = 'GHGE (food intake)', 
                x = 'Food groups', 
                y = 'GHGE')
p2







# p3 ----
# energy vs ghge
plot(ftotal[, 1], ftotal[,  7])


pdfd <- setDT(fd)
# remove water and coffee
pdfd <- pdfd[food != 'Water']
pdfd

p3 <- ggplot(data = pdfd, 
             aes(x = energy, y = ghge, size = intake, label = food))
p3 <- p3 + geom_point(alpha = 0.3) + xlim(-3, 30) + ylim(-0.002, 0.015)
p3 <- p3 + scale_size(range = c(0.1, 20))
p3 <- p3 + geom_text_repel(size = 4, max.overlaps = 15)
# p3 <- p3 + geom_text(size = 3, check_overlap = T)
p3 <- p3 + theme_bw()
p3 <- p3 + labs(title = 'Energy vs GHGE', 
                x = 'Energy (per unit)', 
                y = 'GHGE (per unit)')
p3











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




