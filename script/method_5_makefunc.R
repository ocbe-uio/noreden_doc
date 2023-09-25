# make functions

library(data.table)
library(readxl)

# load 28 foods
# this is not part of the function
input <- read_excel("data_example/input_0915.xlsx")
input <- setDT(input)
input

# change variable name
setnames(input, 'Mean10MJ', 'intake_mean')
setnames(input, '0.1xmean10MJ', 'intake_lwr')
setnames(input, '95thpercentile10MJ', 'intake_upr')


# f: food selector ----





# f: nutconst selector ----
# contribution per unit 






# f: envconst selector ----
# contribution per unit




# f: const computer ----
# constraint for nutrients, envimpact
# expect arg: 
# a vector that specifies min/max as total contrib *gram (0.9, 1.1 etc)
# option to standardize or not (..., how to use it)
# IF REDUCE
# which variable (ghge), by how much





# f: objective ----
# define qp
# (this should also be flexible, in case we use other)
# does it need a second arg for current diet?




# f: ineq const ----
# define inequality constraints
# should match the items from const computer
# make a check for name





# RUN ----


# f: collect results ----










