https://cran.r-project.org/web/views/Optimization.html



## Linear programming LP

Objective linear, constraints linear equalities or inequalities

maximise f(x) = cx

s.t. Ax <= b

x>= 0

## Quadratic programming 

some QP are convex, some not

Constraints: linear equality or inequality 

minimize 1/2 x^t Q x + c^t x

s.t. Ax < b



## Nonlinear optimizaion  (more general than QP)

minimize f(x)

s.t. g(x) <= 0

h(x) = 0

xl <= x <= xu





-----

# Option 1: constOptim 

insert function

`stats::constrOptim()` (underneath running `optim`)

- minimize a problem `f`. 
- `theta`: starting values of length p, must be in feasible region
- `f`, `grad`: function and gradient. gradient can be NULL
- `ui`: constraint matrix, k by p
- `ci`: constraint vector, length k

feasible region defined by `ui %*% theta - ci >=0`

objective function f should retutrn a scalar result



# Option 2:  nloptr

https://cran.r-project.org/web/packages/nloptr/vignettes/nloptr.html

insert function (and gradient)

Need to compute the jacobian for constraints (or not?)









# Option 3: quadprog

Insert matrix instead of function, so need to convert the problem by ourselves.  

solves min(-d^b + 0.5 b^t D b) s.t. A^t b >= b0



























