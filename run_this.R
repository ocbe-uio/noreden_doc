# specify the location of the data file
d <- readRDS('d.RData')

# check if the data is correctly loaded
d$cvec
d$Amat
d$bvec
d$Qmat
d$lb
d$ub
d$sense


# run the program

library(Rcplex)

problem_1 <- Rcplex(cvec = d$cvec, # use data from the d list
                    Amat = d$Amat, 
                    bvec = d$bvec, 
                    Qmat = d$Qmat, 
                    lb = d$lb, 
                    ub = d$ub, 
                    objsense ="min", 
                    sense = d$sense) 

saveRDS(problem_1, file = 'result.RData')
