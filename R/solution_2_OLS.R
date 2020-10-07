#` @title Build up a New Function Fitting the OLS Model.
#` @description We have the database and use all of them getting the error by OLS, which needs to compare with the other one which is got by the GD. In the GD function,
#` we have to use part of the data to train and the others to test.
#`@export

#Build up the OLS
casl_ols_svd <-function(X, y){
  svd_output <- svd(X)
  r <- sum(svd_output$d > .Machine$double.eps)
  U <- svd_output$u[, 1:r]
  V <- svd_output$v[, 1:r]
  beta <- V %*% (t(U) %*% y / svd_output$d[1:r])
  beta
}

# Gradient Descent
cost <- function(X, y, beta) {
  sum( (X %*% beta - y)^2 ) / (2*length(y))
}
gradient <- function(X, y, alpha, iter_nums) {
  cost_history <- double(iter_nums)
  theta <- as.matrix(rep(0, ncol(X)))
  for (i in 1:iter_nums) {
    error <- (X %*% theta - y)
    delta <- t(X) %*% error / length(y)
    theta <- theta - alpha * delta
    cost_history[i] <- cost(X, y, theta)
  }
  return(theta)
}

# Build up the database
set.seed(1)
n <- 1e4; p <- 4
X <- matrix(rnorm(n*p), ncol = p)
beta <- c(1,2,3,4)
epsilon <- rnorm(n)
y <- X %*% beta + epsilon

# OLS on entire database
beta_h_svd <- casl_ols_svd(X, y)
error_ols <- sqrt(sum((y - X %*% beta_h_svd)^2))

#To do the Cross-Validation for Gradient Descent
error_gd_cv <- rep(0, 10)
folds <- cut(seq(1,nrow(X)),breaks=10,labels=FALSE)

# Perform 10 fold cross validation
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  X_train <- X[testIndexes, ]
  y_train <- y[testIndexes, ]
  X_test <- X[-testIndexes, ]
  y_test <- y[-testIndexes, ]
  beta_gd <- gradient(X_train, y_train, 0.5, 10000)
  error_gd_cv[i] <- sqrt(sum((y_test - X_test %*% beta_gd)^2))
}
error_gd <- mean(error_gd_cv)

