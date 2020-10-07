#` @title Grab the ridge parameter
#` @description Use grid search to find the best lambda, which makes the best estimation error -- the smallest.
#`@export
library(MASS)
n <- 200
p <- 4
N <- 500
M <- 20
beta <- c(1, -1, 0.5, 0)
mu <- rep(0, p)
Sigma <- matrix(0.9, nrow = p, ncol = p)
diag(Sigma) <- 1
X <- MASS::mvrnorm(n, mu, Sigma)
y <- X %*% beta + rnorm(n, sd = 5)
X_test <- MASS::mvrnorm(n, mu, Sigma)
y_test <- X_test %*% beta + rnorm(n, sd = 5)
y_test <- as.numeric(y_test)
lambda_vals <- seq(0, n*2, length.out = N)
casl_lm_ridge <-function(X, y, lambda_vals){
  svd_obj <- svd(X)
  U<- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  k <- length(lambda_vals)
  ridge_beta <- matrix(NA_real_, nrow = k, ncol = ncol(X))
  for (j in seq_len(k)){
    D <- diag(svals / (svals^2 + lambda_vals[j]))
    ridge_beta[j,] <- V %*% D %*% t(U) %*% y
  }
  ridge_beta
}
beta_mat <- casl_lm_ridge(X, y, lambda_vals)
y_hat <- tcrossprod(X_test, beta_mat)
mse <- apply((y_hat - y_test)^2, 2, mean)
lambda_vals[which.min(mse)]
