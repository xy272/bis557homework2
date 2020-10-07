#` @title Grab the ridge regression function
#` @description This is the ridge regression function. There are test data, which has 25 columns, including first two columns are colinear. We will test N times.
#`@export
# build up the test data with first two columns are colinear.
# Ridge regression function
My_ridge_regression <- function(X, y, lambda) {
  n <- dim(X)[2]
  solve( crossprod(X) + lambda * diag(n), crossprod(X, y))
}


