# use new data 

library(data.table)
library(ggplot2)


foodgroup <- readxl::read_excel("~/Documents/Data/uio_norkost/norkost3_matvaregrupper.xlsx")
foodgroup <- data.table(foodgroup)

foodgroup

demographics <- read_excel("~/Documents/Data/uio_norkost/norkost3_demographics.xlsx")
demographics <- data.table(demographics)
demographics



# match ID, age 
demographics$Nr |> unique() |> length() # 1767

# same ID? yes
all.equal(demographics$Nr |> unique(), foodgroup$Nr |> unique())


# attach age to foodgroup data
age <- demographics[, .(Nr, Alder)]

d <- merge(age, foodgroup)


# food group ----

d$Nr |> unique() |> length() # 1787 (norkost 3)
d$Løpedag |> table() # which round

d$Kjønn |> table()/2 # 2 is women
# 862 men, 925 women


d$Alder |> hist()


d$Ukedag |> table() |> barplot()
# thursday, friday, saturday much fewer than others

# gender difference?
tb <- table(d$Ukedag, d$Kjønn)
barplot(tb[,1])
barplot(tb[,2])
# not significant

# round difference?
tb <- table(d$Ukedag, d$Løpedag)
barplot(tb[,1])
barplot(tb[,2]) 
tb
# first measurement has fewer on friday, saturday
# probably wont affect results




# treat each round as independent 

# what is total?how can it have such small and large values?
d$TOTALT |> hist()


# bread -----

dbread <- d[, .(Nr, Løpedag, Kjønn, Ukedag, BROD)]
dbread

# how many records above 0? 3423/3574 - 96%
dbread[BROD >0]
dbread[BROD >0]$Nr |> unique() |> length() # 1776/1787

# weekday difference on consumption

dbread[Ukedag == 'Onsdag']$BROD |> hist() 

q <- ggplot(dbread, aes(x = BROD, color = as.factor(Kjønn)))
q <- q + geom_histogram(fill = 'white')
q + facet_wrap(~Ukedag)

# seems to be no difference across gender or week day

# how to identify within person variation?
# 2 measurements only
# difference might make sense 
dbread[, !c('Ukedag')]

try <- tidyr::pivot_wider(dbread[, !c('Ukedag')], 
                          names_from = Løpedag, 
                          values_from = BROD)
try

# compute difference, take absolute value
try$diff <- abs(try$`1` - try$`2`)
try$diff |> hist()
# variation between 2 measurements is quite big 

try$average <- (try$`1` + try$`2`)/2
try$average |> hist()
# what about average

q <- ggplot(try, aes(x = average, y = diff, color = as.factor(Kjønn)))
q <- q + geom_point()
q

# as expected
# the straight line is for those with 0 on one measurement 
