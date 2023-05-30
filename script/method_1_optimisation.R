# learn and test different optimization methods 

# objective
# make a diet combination (x1 grams of A, x2 grams of B) s.t.
# nutrition is enough; 
# green house emission is low;
# ... 

# make a fake dataframe 

df <- data.frame(food = c('bread', 'vegetable', 'meat'), 
                 nutri = c(200, 100, 300), 
                 ghe = c(100, 50, 400))

df

# this should be a LP: one objective (find x), s.t. a few constraints
# 1. food_prop %*% nutri > lower threshold of nutri; 
# 2. food_prop %*% ghe < upper threshod of environment



# try constrOptim

# maximise f(x,y) = log(x) + \frac{x^2}{y^2},
# s.t. 
# g1(x,y) = x+y <1 
# g2(x,y) = x>0
# g3(x,y) = y>0


stats::constrOptim()

fr <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  -(log(x1) + x1^2/x2^2) # negative, bec minimisation
}


rbind(c(-1,-1),c(1,0), c(0,1) ) %*% c(0.99,0.001) -c(-1,0, 0)

try <- constrOptim(c(0.25, 0.25), 
                   fr, NULL, 
                   ui=rbind(c(-1,-1),  # the -x-y > -1
                            c(1,0),    # the x > 0
                            c(0,1)),   # the y > 0
                   ci=c(-1,0.0001,0.001)) # the thresholds
try
try$par

?constrOptim

## from optim
fr <- function(x) {   ## Rosenbrock Banana function
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
grr <- function(x) { ## Gradient of 'fr'
  x1 <- x[1]
  x2 <- x[2]
  c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
    200 *      (x2 - x1 * x1))
}

optim(c(-1.2,1), fr, grr)
#Box-constraint, optimum on the boundary
constrOptim(c(-1.2,0.9), fr, grr, ui = rbind(c(-1,0), c(0,-1)), ci = c(-1,-1))
#  x <= 0.9,  y - x > 0.1
constrOptim(c(.5,0), fr, grr, ui = rbind(c(-1,0), c(1,-1)), ci = c(-0.9,0.1))



## Solves linear and quadratic programming problems
## but needs a feasible starting value
#
# from example(solve.QP) in 'quadprog'
# no derivative
fQP <- function(b) {-sum(c(0,5,0)*b)+0.5*sum(b*b)}
Amat       <- matrix(c(-4,-3,0,2,1,0,0,-2,1), 3, 3)
bvec       <- c(-8, 2, 0)
constrOptim(c(2,-1,-1), fQP, NULL, ui = t(Amat), ci = bvec)
# derivative
gQP <- function(b) {-c(0, 5, 0) + b}
constrOptim(c(2,-1,-1), fQP, gQP, ui = t(Amat), ci = bvec)

## Now with maximisation instead of minimisation
hQP <- function(b) {sum(c(0,5,0)*b)-0.5*sum(b*b)}
constrOptim(c(2,-1,-1), hQP, NULL, ui = t(Amat), ci = bvec,
            control = list(fnscale = -1))










