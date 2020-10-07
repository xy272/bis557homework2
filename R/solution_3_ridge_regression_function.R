#` @title Grab the ridge regression function
#` @description This is the ridge regression function. There are test data, which has 25 columns, including first two columns are colinear. We will test N times.
#`@export
# build up the test data with first two columns are colinear.
n1 <- 1000
p1 <- 25
beta1 <- c(1, rep(0, p1 - 1))
X1 <- matrix(rnorm(n1 * p1), ncol = p1)
alpha1 <- 0.001
X1[,1] <- X1[,1] * alpha1 + X1[,2] * (1 - alpha1)

# Ridge regression function
My_ridge_regression <- function(X, y, lambda) {
  n1 <- dim(X1)[2]
  solve( crossprod(X1) + lambda * diag(n1), crossprod(X1, y1))
}

# Test N times
N1 <- 1e4; l2_errors <- rep(0, N1)
for (k in 1:N1) {
  y1 <- X1 %*% beta1 + rnorm(n1)
  betahat1 <- My_ridge_regression(X1, y1, 0.1)
  l2_errors[k] <- sqrt(sum((betahat1 - beta1)^2))
}
mean(l2_errors)

