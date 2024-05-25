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


