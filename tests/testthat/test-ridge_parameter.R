context("Testing ridge parameter")
test_that("You solution_4_ridge_parameter() function works",{
  set.seed(4)
  n <- 200
  p <- 4
  N <- 500
  beta <- c(1, -1, 0.5, 0)
  X <- matrix(rnorm(n*p), ncol = p)
  y <- X %*% beta + rnorm(n, sd = 5)
  X_test <- matrix(rnorm(n*p), ncol = p)
  y_test <- X_test %*% beta + rnorm(n, sd = 5)
  y_test <- as.numeric(y_test)
  lambda_vals <- seq(0, n*2, length.out = N)
  beta_mat <- casl_lm_ridge(X, y, lambda_vals)
  y_hat <- tcrossprod(X_test, beta_mat)
  mse <- apply((y_hat - y_test)^2, 2, mean)
  min_lambda_vals <- lambda_vals[which.min(mse)]
  expect_equivalent(min_lambda_vals, 27.3, tolerance = 0.1)
})

