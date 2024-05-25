# use bread as an example 

library(data.table)
library(ggplot2)


foodgroup <- readxl::read_excel("~/Documents/Data/uio_norkost/norkost3_matvaregrupper.xlsx")
foodgroup <- data.table(foodgroup)

demographics <- readxl::read_excel("~/Documents/Data/uio_norkost/norkost3_demographics.xlsx")
demographics <- data.table(demographics)

# attach age to foodgroup data
age <- demographics[, .(Nr, Alder)]

d <- merge(age, foodgroup)


# take out bread

dbread <- d[, .(Nr, Løpedag, Alder, Kjønn, Ukedag, BROD)]
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



# lm (log transform?) ----
# I use bread because it is a rather complete variable

dbread

setnames(dbread, old = 'Løpedag', new = 'record')
setnames(dbread, old = 'Alder', new = 'age')
setnames(dbread, old = 'Kjønn', new = 'sex')

# sex 1m 2f
dbread[sex == 1, sexc := 'm']
dbread[sex == 2, sexc := 'f']


hist(dbread$BROD)
hist(log(dbread$BROD))
# not normally distributed

boxplot(BROD ~ sexc, data = dbread)
plot(dbread$age, dbread$BROD) # age effect not strong


# both are bad fits due to non-normal data
f1 <- lm(BROD ~ age, data = dbread)
summary(f1)

f2 <- lm(BROD ~ sexc, data = dbread)
summary(f2)

plot(f2)


f3 <- lm(BROD ~ age + sexc, data = dbread)
summary(f3)
# r2 = 0.11
# b0 = 196.38 (se 7.31)
# bage = -1.166 (se 0.14)
# bsexm = 84.825 (se 4.14)



# lmm ----
# (this is not an appropriate model due to skewed response)
# (but as a start to test the model outputs and pi)
# (should probably use bayesian regression)

library(lme4)
library(merTools)

f3m <- lmer(BROD ~ age + sexc + (1|Nr), data = dbread)

summ_f3m <- summary(f3m)
summ_f3m
# fixed:
# b0 = 196.37 (se 8.36)
# bage = -1.166 (0.17)
# bsexm = 84.82 (4.479)
# point estimate similar to lm, se are larger


pred_f3m <- predictInterval(f3m, 
                            newdata = data.frame(age = 60, sexc = 'f', Nr = 11000), 
                            n.sims = 100, 
                            returnSims = T, 
                            level = 0.95)
pred_f3m

?predictInterval
pred_f3m


head(dbread)

dbread$BROD |> summary() # median 157, mean 183




