# rscript for making fig1 and fig2 in the manuscript

# load data
library(readxl)
library(data.table)
library(ggplot2)

fig1 <- read_excel("noreden_manuscript/Figure input Chi.xlsx", 
                   sheet = "Figure 1")

fig2 <- read_excel("noreden_manuscript/Figure input Chi.xlsx", 
                   sheet = "Figure 2")

# rename cols for ease of coding

fig1 <- data.table(fig1)
colnames(fig1) <- c('foodname', 'basis', 'meat', 'plant')

# fig2 <- data.table(fig2)
colnames(fig2) <- c('foodname', 'obsdiet', 'basis', 'meat', 'plant')


# food groups -----

grain <- c('Bread', 
           'Other grains',
           'Cakes cookies')

fruit_vege <- c('Potatoes', 
                'Vegetables', 
                'Legumes', 
                'Fruits and berries', 
                'Juice', 
                'Nuts and seeds')

meat <- c('Ruminant meat', 
          'Pork',
          'White meat',
          'Other meat')

fish_egg <- c('Fish', 
              'Eggs')

dairy <- c('Milk',
           'Other dairy',
           'Cheese')

fats <- c('Fats, plant-based',
          'Fats, animal-based')

sugar_other <- c('Sweets, snacks', 
                 'Other beverages',
                 'Other')

foodgroup_df <- rbind(
  data.frame(foodname = grain, foodgroup = 'Grain'),
  data.frame(foodname = fruit_vege, foodgroup = 'Fruit Vegetables'),
  data.frame(foodname = meat, foodgroup = 'Meat'),
  data.frame(foodname = fish_egg, foodgroup = 'Fish Egg'),
  data.frame(foodname = dairy, foodgroup = 'Dairy'),
  data.frame(foodname = fats, foodgroup = 'Fats'),
  data.frame(foodname = sugar_other, foodgroup = 'Sugar Other')
)

foodgroup_df

# can attach different color to foodgroups

foodgroups <- c('Grain',
               'Fruit Vegetables',
               'Meat',
               'Fish Egg',
               'Dairy',
               'Fats',
               'Sugar Other')

colors_f1 <- c('#f0c11a', 
            '#048a04', 
            '#a11c15',
            '#7da9db', 
            '#f7e3c8', 
            '#f2c4a2',
            '#c9bfbb')





# fig1 ----

fig1

# remove water
fig1 <- fig1[foodname != 'Water, coffee, tea']
fig1

fig1_long <- tidyr::pivot_longer(fig1, 
                                 cols = -c('foodname'), 
                                 names_to = 'type', 
                                 values_to = 'value')
fig1_long


# modify order for plotting
# use consistent names as in foodgroup_df
# can modify there

foodname_your_order <- foodgroup_df$foodname

fig1_long$foodname_order <- factor(fig1_long$foodname, 
                                   levels = foodname_your_order)


# merge with food groups
fig1_plot <- dplyr::left_join(fig1_long, foodgroup_df, by = 'foodname')



# f1 barplot (dodge) ----

f1 <- ggplot(data = fig1_plot,
             aes(x = forcats::fct_rev(foodname_order), 
                 y = value, 
                 fill = type))

# basic barplot
f1 <- f1 + geom_bar(stat = 'identity',
                    position = position_dodge())

f1 <- f1 + coord_flip()

# add baseline 0
f1 <- f1 + geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')

# theme and color
f1 <- f1 + theme_classic()

# using the palette preset
# if needed, can also manually specify (see fig2)
f1 <- f1 + scale_fill_brewer(palette = 'Set2')

# if you want to manully set the labels as well as colors manually,
# try this:
# f1 + scale_fill_manual(labels = c('Basis','Meat','Plant'),
#                        values = c('blue', 'red', 'green'))
# the ordering needs to match the order from the data


# change title, x, y axis (if needed)
# if you do not need, specify axis.title.x(or y) = element_blank() 
# in theme() -> next section

f1 <- f1 + labs(
  x = '', 
  y = 'Relative change (%)'
)

# modify layout, text size etc
f1 <- f1 + theme(axis.text = element_text(size = 12), 
                 axis.title = element_text(size =12),
                 plot.title = element_text(size = 15)#, 
                 #axis.title.x = element_blank() # if do not want x axis
)

# legend options
f1 <- f1 + theme(legend.position = 'right', 
                 legend.title = element_blank(),
                 legend.text = element_text(size = 10))



f1



# f1 barplot (split) ----
# use foodname_order as x axis
# forcats::fct_rev puts bread first
# can also just use foodname_order

f1 <- ggplot(data = fig1_plot, 
             aes(x = forcats::fct_rev(foodname_order), 
                 y = value, 
                 fill = foodgroup))

# basic barplot
f1 <- f1 + geom_bar(stat = 'identity')
f1 <- f1 + geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')
f1 <- f1 + coord_flip()
f1 <- f1 + theme_classic()
# f1 <- f1 + theme_minimal()


# use customized colors
f1 <- f1 + scale_fill_manual(breaks = foodgroups, 
                             values = colors_f1)

# change label name
# f1 <- f1 + scale_x_discrete(labels = c('NNR Basis', 
#                                        'NNR Meat', 
#                                        'NNR Plant-based'))


f1
f1 + facet_wrap(~type, ncol = 3)
f1 + facet_wrap(~type, ncol = 3, scales = 'free')




# fig1 lollipop ----

library(dplyr)
fig1_long

# have min max values per group to draw the range
df_summary <- group_by(fig1_long, foodname) |> 
  summarise(min = min(value), 
            max = max(value))

df_summary

# add the values to the original data to plot
fig1_long_l <- left_join(fig1_long, df_summary)

fig1_long_l

# fig1_plot_l <- dplyr::left_join(fig1_long_l, foodgroup_df, by = 'foodname')

# need to use the reversed order for foodnames
f1l <- ggplot(data = fig1_long_l, aes(x = value, 
                                      y = forcats::fct_rev(foodname_order), 
                                      color = type))

# plot baseline 0 first
f1l <- f1l + geom_vline(xintercept = 0, color = 'black', linetype = 'dashed')

# line segment 
f1l <- f1l + geom_segment(aes(x = min, 
                              xend = max, 
                       y = forcats::fct_rev(foodname_order), 
                       yend = forcats::fct_rev(foodname_order)), 
                   linetype = 'dotted', 
                   color = 'darkgrey')

# add points
f1l <- f1l + geom_point(size = 4, alpha = 1)

# change color manually
f1l <- f1l + scale_color_manual(labels = c('Basis','Meat','Plant'),
                       values = c('#dbad04', '#ab2c0c', '#027d21'))

# modify theme
f1l <- f1l + theme_classic()

# add x y axis title
f1l <- f1l + labs(
  x = 'Relative change (%)', 
  y = 'Food'
)

# modify layout, text size etc
f1l <- f1l + theme(axis.text = element_text(size = 12), 
                 axis.title = element_text(size =12),
                 plot.title = element_text(size = 15), 
                 axis.title.y = element_blank() # if do not want y axis
)

# legend options
f1l <- f1l + theme(legend.position = 'right', 
                 legend.title = element_blank(),
                 legend.text = element_text(size = 10))


f1l



# fig2 ----

fig2_long <- tidyr::pivot_longer(fig2, 
                                 cols = -c('foodname'), 
                                 names_to = 'type', 
                                 values_to = 'value')
fig2_long

# set color
foodnames <- c('Nuts and seeds', 
               'Legumes',
               'Eggs', 
               'Fish', 
               'Ruminant meat', 
               'Pork', 
               'White meat', 
               'Other meat')

colors_f2 <- c('#a16415', 
            '#929c78', 
            '#f0b146',
            '#7da9db', 
            '#8c120e', 
            '#a84a25',
            '#f7e3c8', 
            '#c9bfbb')


# order text for both foodname and type
type_your_order <- c('obsdiet', 'basis', 'meat', 'plant')

fig2_long$type_order <- factor(fig2_long$type, 
                               levels = type_your_order)


foodname_your_order <- c('Nuts and seeds', 
                         'Legumes',
                         'Eggs', 
                         'Fish', 
                         'Ruminant meat', 
                         'Pork', 
                         'White meat', 
                         'Other meat')

fig2_long$foodname_order <- factor(fig2_long$foodname, 
                               levels = foodname_your_order)


# plot 
# use ordered rather than original
f2 <- ggplot(data = fig2_long, 
             aes(x = type_order, y = value, fill = foodname_order))

# basic barplot
f2 <- f2 + geom_bar(stat = 'identity')

# use customized colors
f2 <- f2 + scale_fill_manual(breaks = foodnames, 
                        values = colors_f2)

# change label name
f2 <- f2 + scale_x_discrete(labels = c('Observed diet', 
                                 '(1) NNR Basis', 
                                 '(2) NNR Meat', 
                                 '(3) NNR Plant-based'))
# y axis breaks: 0, 50, 100, ...
# start y axis from 0
f2 <- f2 + scale_y_continuous(breaks = seq(0, 300, by = 50), 
                              expand = expansion(mult = c(0, 0.1)))

# modify background theme
f2 <- f2 + theme_classic()

# change title, x, y axis (if needed)
# if you do not need, specify axis.title.x(or y) = element_blank() 
# in theme() -> next section

f2 <- f2 + labs(
  x = 'Diet type', 
  y = 'Proteint source (g/10 MJ)'
)

# modify layout, text size etc
f2 <- f2 + theme(axis.text = element_text(size = 12), 
                 axis.title = element_text(size =12),
                 plot.title = element_text(size = 15), 
                 axis.title.x = element_blank() # if do not want x axis
)

# legend options
f2 <- f2 + theme(legend.position = 'right', 
                 legend.title = element_blank(),
                 legend.text = element_text(size = 10))

f2





