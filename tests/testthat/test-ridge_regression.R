context("Testing ridge regression function")
test_that("You ridge_regression() function works.",{
  n <- 1000
  p <- 25
  beta1 <- c(1, rep(0, p - 1))
  X1 <- matrix(rnorm(n * p), ncol = p)
  alpha1 <- 0.001
  X1[,1] <- X1[,1] * alpha1 + X1[,2] * (1 - alpha1)
  N1 <- 1e4; l2_errors <- rep(0, N1)
  for (k in 1:N1) {
    y1 <- X1 %*% beta1 + rnorm(n)
    betahat1 <- My_ridge_regression(X1, y1, 0.1)
    l2_errors[k] <- sqrt(sum((betahat1 - beta1)^2))
  }
  mean_l2_error <- mean(l2_errors)
  expect_equivalent(mean_l2_error, 0.7255469,  tolerance = 1)
})


