#` @title Grab the ridge parameter
#` @description Use grid search to find the best lambda, which makes the best estimation error -- the smallest.
#`@export
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
