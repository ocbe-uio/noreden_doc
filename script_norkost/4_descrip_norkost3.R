# descriptive analysis for food from norkost3 

library(data.table)
library(ggplot2)


foodgroup <- readxl::read_excel("~/Documents/Data/uio_norkost/norkost3_matvaregrupper.xlsx")
foodgroup <- data.table(foodgroup)

demographics <- readxl::read_excel("~/Documents/Data/uio_norkost/norkost3_demographics.xlsx")
demographics <- data.table(demographics)

# attach age to foodgroup data
age <- demographics[, .(Nr, Alder)]

d <- merge(age, foodgroup)
colnames(d)

# make it easier
setnames(d, old = 'Løpedag', new = 'record')
setnames(d, old = 'Alder', new = 'age')
setnames(d, old = 'Kjønn', new = 'sex')

# sex 1m 2f
d[sex == 1, sexc := 'm']
d[sex == 2, sexc := 'f']



# intake summary ----
# in total, 25 types
# take out bread


# need information on
# mean, median, 90% quantiles, max, 
# nzero
# mean, median after removing zero 
# stratified by gender


# d[, .(Mean = sapply(.SD, mean)), .SDcols = 'BROD']
# d[, .(Mean = sapply(.SD, mean)), .SDcols = 'BROD', by = sexc]
# d[, .(Mean = sapply(.SD, mean), 
#       Median = sapply(.SD, median),
#       Max = sapply(.SD, max), 
#       q90 = sapply(.SD, function(x){quantile(x, 0.9)})),
#   .SDcols = 'BROD', by = sexc]
# d[, .(nzero = sapply(.SD, function(x){sum(x == 0)})), .SDcols = 'BROD', by = sexc]


# use the function 

colnames(d)
head(d)


intake_summary(dt = d, foodname = 'BROD')
intake_summary(dt = d, foodname = 'POTET')

intake_summary(dt = d, foodname = 'TE')

# use a loop, get all food table

foodname_list <- c(
  'BROD',
  'KORNPR',
  'KAKER',
  'POTET',
  'GRSAK',
  'FRUKTB',
  'JUICE',
  'KJOTT',
  'FISK',
  'EGG',
  'MELKYO',
  'FLOTIS',
  'OST',
  'SMARGO',
  'SUKSOT',
  'DRIKKE', 
  'KAFFE',
  'TE',
  'SAFBRU',
  'VIN',
  'DRVANN',
  'OL',
  'BRVIN',
  'DIVERS',
  'SNACKS'
)


summary_list <- list()

for(i in 1:length(foodname_list)){
  summary_list[[i]] <- intake_summary(dt = d, foodname = foodname_list[i])
  cat('Getting summary for ', foodname_list[i], '\n')
}

summary_list
summary_table <- do.call(rbind, summary_list)
summary_table

# can extract info easily
s1 <- summary_table[sexc == 'total' & label == 'keep_zero'] 
s1[order(pzero, Mean)]

s1zero <- summary_table[sexc == 'total' & label == 'rm_zero'] 
s1zero[order(Mean)]






# visualization -----


vis_distribution(data = d, foodname = 'BROD')

# zero boosted
vis_distribution(data = d, foodname = 'TE')
vis_distribution(data = d, foodname = 'EGG')

# above 300, above 450
vis_distribution(data = d, foodname = 'FISK')
vis_distribution(data = d, foodname = 'OST')

# above 350
vis_distribution(data = d, foodname = 'KJOTT')


# scatter

vis_point(data = d, foodname = 'BROD')
vis_point(data = d, foodname = 'TE')
vis_point(data = d, foodname = 'EGG')


vis_point(data = d, foodname = 'FISK')
vis_point(data = d, foodname = 'OST')
vis_point(data = d, foodname = 'KJOTT')




# SPADE: take out variables ----


head(d)

dtest <- d[, .(
  Nr, age, record, Ukedag, sex, sexc, 
  GRSAK, FRUKTB, KJOTT, FISK
)]


dtest
# write.csv(dtest, file = '~/Documents/Data/uio_norkost/food_test_spade.csv')


# meat
# pzero is 0.12, not so sparse
# might treat as frequently consumed food
intake_summary(dt = dtest, foodname = 'KJOTT') # meat
vis_distribution(data = dtest, foodname = 'KJOTT')
vis_point(data = dtest, foodname = 'KJOTT')



# fish
# pzero is 0.55
# mean 67 (incl zero), max 1443, q90 225
# mean 150 (excl zero), median 120, q90 320
intake_summary(dt = dtest, foodname = 'FISK') # fish
vis_distribution(data = dtest, foodname = 'FISK')
vis_point(data = dtest, foodname = 'FISK')


# vegetables
# pzero 0.09
# mean 154, median 131, max 1284, p90 315
intake_summary(dt = dtest, foodname = 'GRSAK') # veg
vis_distribution(data = dtest, foodname = 'GRSAK')
vis_point(data = dtest, foodname = 'GRSAK')

# fruit
# pzero 0.21
intake_summary(dt = dtest, foodname = 'FRUKTB') # fruit
vis_distribution(data = dtest, foodname = 'FRUKTB')
vis_point(data = dtest, foodname = 'FRUKTB')


# nut?






# utility -----

intake_summary <- function(dt, foodname){
  
  # dt <- d
  # foodname <- 'BROD'
  if(!foodname %in% colnames(dt)){
    stop('Food name not found, check')
  }
  
  
  # overall mean, median
  overall <- dt[, .(N = .N,
                    Mean = sapply(.SD, mean), 
                    Median = sapply(.SD, median),
                    Max = sapply(.SD, max), 
                    q90 = sapply(.SD, function(x){quantile(x, 0.9)})),
                .SDcols = foodname]
  
  
  # zero related: n zero, percentage
  zero <- dt[, .(nzero = sapply(.SD, function(x){sum(x == 0)}), 
                 pzero = sapply(.SD, function(x){round(sum(x == 0)/length(x), 2)})), 
             .SDcols = foodname]
  
  # stratify by sex
  bysex <- dt[, .(N = .N,
                  Mean = sapply(.SD, mean), 
                  Median = sapply(.SD, median),
                  Max = sapply(.SD, max), 
                  q90 = sapply(.SD, function(x){quantile(x, 0.9)}),
                  nzero = sapply(.SD, function(x){sum(x == 0)}), 
                  pzero = sapply(.SD, function(x){round(sum(x == 0)/length(x), 2)})),
              .SDcols = foodname, 
              by = sexc]
  
  
  # remove zero records, some might be very zero skewed
  zero_id <- which(dt[[foodname]] == 0)
  overall_rmzero <- dt[!zero_id, 
                       .(N = .N,
                         Mean = sapply(.SD, mean), 
                         Median = sapply(.SD, median),
                         Max = sapply(.SD, max), 
                         q90 = sapply(.SD, function(x){quantile(x, 0.9)}),
                         nzero = sapply(.SD, function(x){sum(x == 0)}), 
                         pzero = sapply(.SD, function(x){round(sum(x == 0)/length(x), 2)})),
                       .SDcols = foodname]
  
  # also stratify by sex
  bysex_rmzero <- dt[!zero_id, .(N = .N,
                                 Mean = sapply(.SD, mean), 
                                 Median = sapply(.SD, median),
                                 Max = sapply(.SD, max), 
                                 q90 = sapply(.SD, function(x){quantile(x, 0.9)}),
                                 nzero = sapply(.SD, function(x){sum(x == 0)}), 
                                 pzero = sapply(.SD, function(x){round(sum(x == 0)/length(x), 2)})),
                     .SDcols = foodname, 
                     by = sexc]
  
  # put together 
  sexc <- 'total'
  t1 <- cbind(sexc, overall, zero)
  t1p <- rbind(t1, bysex)
  t1p$label <- 'keep_zero'
  
  t2 <- cbind(sexc, overall_rmzero)
  t2p <- rbind(t2, bysex_rmzero)
  t2p$label <- 'rm_zero'
  
  result <- rbind(t1p, t2p)
  result$Mean <- round(result$Mean, 2)
  result$food <- foodname
  
  return(result)
}


vis_distribution <- function(data, foodname){
  
  # take subset
  # data <- d
  # foodname <- 'BROD'
  
  dplot <- data[, .SD, .SDcols = c('Nr', 'age', 'sexc', foodname)]
  setnames(dplot, old = foodname, new = 'food')
  
  q <- ggplot(dplot, aes(x = food, color = sexc))
  q <- q + geom_histogram(fill = 'white')  
  q <- q + theme_bw()
  q <- q + facet_wrap(~sexc, nrow = 2)
  
  return(q)
}


vis_point <- function(data, foodname){
  # take subset
  # data <- d
  # foodname <- 'BROD'
  
  dplot <- data[, .SD, .SDcols = c('Nr', 'age', 'sexc', foodname)]
  setnames(dplot, old = foodname, new = 'food')
  q <- ggplot(dplot, aes(x = age, y = food, color = sexc))
  q <- q + geom_point()
  q <- q + geom_smooth(method = 'lm', se = FALSE, color = 'black')
  q <- q + theme_bw()
  q <- q + facet_wrap(~sexc, nrow = 2)
  
  return(q)
}





