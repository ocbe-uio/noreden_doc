https://cran.r-project.org/web/views/Optimization.html



# Topics

## Convex optimization

- Objective function is convex, constraint set is convex

Linear programming LP

- Obj linear, constraints linear equalities or inequalities



## Quadratic programming 

some QP are convex

Constraints: linear equality or inequality 



## Stochastic programming

dynamic programming 









# Test different options 

`stats::constrOptim()` (underneath running `optim`)

- minimize a problem `f`. 
- `theta`: starting values of length p, must be in feasible region
- `f`, `grad`: function and gradient. gradient can be NULL
- `ui`: constraint matrix, k by p
- `ci`: constraint vector, length k

feasible region defined by `ui %*% theta - ci >=0`

objective function f should retutrn a scalar result



