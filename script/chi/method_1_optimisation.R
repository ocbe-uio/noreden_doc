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



# constrOptim ----

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




# ________ ----
# nloptr ----
# install.packages('nloptr')

## Rosenbrock Banana (unconstrained) ----
eval_f <- function(x) {   
  return( 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2 )
}

## Gradient of Rosenbrock Banana function
eval_grad_f <- function(x) { 
  return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
             200 * (x[2] - x[1] * x[1]) ) )
}

# initial values
x0 <- c(-1.2, 1)

opts <- list("algorithm"="NLOPT_LD_LBFGS",
             "xtol_rel"=1.0e-8)

# solve Rosenbrock Banana function
res <- nloptr::nloptr( x0=x0, 
               eval_f=eval_f, 
               eval_grad_f=eval_grad_f,
               opts=opts)
res

eval_f(c(1, 1))
eval_f(c(1, -1))



## minim + inequality const ----

# objective function
eval_f0 <- function( x, a, b ){ 
  return( sqrt(x[2]) )
}

# gradient of objective function
eval_grad_f0 <- function( x, a, b ){ 
  return( c( 0, .5/sqrt(x[2]) ) )
}

# constraint function
eval_g0 <- function( x, a, b ) {
  return( (a*x[1] + b)^3 - x[2] )
}


# jacobian of constraint
eval_jac_g0 <- function( x, a, b ) {
  return( rbind( c( 3*a[1]*(a[1]*x[1] + b[1])^2, -1.0 ), 
                 c( 3*a[2]*(a[2]*x[1] + b[2])^2, -1.0 ) ) )
}

# define parameters
a <- c(2,-1)
b <- c(0, 1)


# Solve using NLOPT_LD_MMA with gradient information supplied in separate function
res0 <- nloptr::nloptr( x0=c(1.234,5.678), 
                eval_f=eval_f0, 
                eval_grad_f=eval_grad_f0,
                lb = c(-Inf,0), 
                ub = c(Inf,Inf), 
                eval_g_ineq = eval_g0,
                eval_jac_g_ineq = eval_jac_g0,                
                opts = list("algorithm" = "NLOPT_LD_MMA",
                            "xtol_rel"=1.0e-8,
                            "print_level" = 2,
                            "check_derivatives" = TRUE,
                            "check_derivatives_print" = "all"),
                a = a, 
                b = b )


res0


# no gradient -----
# Objective Function
eval_f <- function(x)
{
  return (x[1]*x[4]*(x[1] +x[2] + x[3] ) + x[3] )
}
# Inequality constraints
eval_g_ineq <- function(x)
{
  return (25 - x[1]*x[2]*x[3]*x[4])
}
# Equality constraints
eval_g_eq <- function(x)
{
  return ( x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40 )
}
# Lower and upper bounds
lb <- c(1,1,1,1)
ub <- c(5,5,5,5)
#initial values
x0 <- c(1,5,5,1)


# Set optimization options.
local_opts <- list( "algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-15 )
opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 160000,
              "local_opts" = local_opts,
              "print_level" = 0 )

res <- nloptr::nloptr ( x0 = x0,
                eval_f = eval_f,
                lb = lb,
                ub = ub,
                eval_g_ineq = eval_g_ineq,
                eval_g_eq = eval_g_eq,
                opts = opts
)
print(res)

res$objective
res$solution



# multiple inequality ----
# Objective function
eval_f <- function(x)
{
  return ( x[1]^2 + x[2]^2 )
}

# Inequality constraints
eval_g_ineq <- function (x) {
  constr <- c(1 - x[1] - x[2],
              1 - x[1]^2 - x[2]^2,
              9 - 9*x[1]^2 - x[2]^2,
              x[2] - x[1]^2,
              x[1] - x[2]^2)
  return (constr)
}
# Lower and upper bounds
lb <- c(-50, -50)
ub <- c(50, 50)
# Initial values
x0 <- c(3, 1)

opts <- list( "algorithm"
              = "NLOPT_GN_ISRES",
              "xtol_rel"
              = 1.0e-15,
              "maxeval"= 160000,
              "tol_constraints_ineq" = rep( 1.0e-10, 5 ))


res <- nloptr::nloptr(
  x0          = x0,
  eval_f      = eval_f,
  lb          = lb,
  ub          = ub,
  eval_g_ineq = eval_g_ineq,
  opts        = opts )
print(res)


# ________ ----
# quadprog ----
# install.packages('quadprog')

## Assume we want to minimize: -(0 5 0) %*% b + 1/2 b^T b
## under the constraints:      A^T b >= b0
## with b0 = (-8,2,0)^T
## and
##
##
## we can use solve.QP as follows:
##
Dmat       <- matrix(0,3,3)
diag(Dmat) <- 1
dvec       <- c(0,5,0)
Amat       <- matrix(c(-4,-3,0,2,1,0,0,-2,1),3,3)
bvec       <- c(-8,2,0)
quadprog::solve.QP(Dmat,dvec,Amat,bvec=bvec)






