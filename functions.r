# Functions to be imported by other scripts

skewness <- function(data) {
  # Given a numeric vector, compute and return the empirical skewness
  mu <- mean(data)
  sigma <- sd(data)
  mu_3 <- mean((data - mu)^3)
  S <- mu_3 / sigma^3
  return(S)
}

kurtosis <- function(data) {
  # Given a numeric vector, compute and return the empirical kurtosis
  mu <- mean(data)
  sigma <- sd(data)
  mu_4 <- mean((data - mu)^4)
  K <- mu_4 / sigma^4
  return(K)
}




# Test for quantile proposed in Kupiec (1995) ---------------------------------------
kupiec_test <- function(mat_bool_exceptions, alpha) {
  # mat_bool_exceptions is a (M x K) matrix filled with 0s (no exception) and 1s (exception occurs)
  # alpha is nominal VaR level (= 1 - coverage probability)
  M <- NROW(mat_bool_exceptions) # Number of predictions
  exceptions <- colSums(mat_bool_exceptions) # Number of exceptions for each time series
  L_obs <- ((1 - exceptions / M)^(M - exceptions)) * ((exceptions / M)^exceptions)
  L_H0 <- ((1 - alpha)^(M - exceptions)) * ((alpha)^exceptions)
  LR <- 2 * (log(L_obs) - log(L_H0))
  return(LR)
}


# Test for quantile proposed in Christoffersen (1998) -------------------------------
christoffersen_test <- function(mat_bool_exceptions, alpha) {
  # mat_bool_exceptions is a (M x K) matrix filled with 0s (no exception) and 1s (exception occurs)
  # alpha is the nominal VaR level (= 1 - coverage probability)
  M <- NROW(mat_bool_exceptions) # Number of predictions

  # Independence step
  LR_ind <- numeric(length = NCOL(mat_bool_exceptions))
  hit <- mat_bool_exceptions[2:M, ]
  hit_lag1 <- mat_bool_exceptions[1:(M - 1), ]
  for (j in 1:NCOL(mat_bool_exceptions)) {
    tab <- table(
      hit_lag1[, j],
      hit[, j]
    ) # Rows: initial state; columns: final state
    n00 <- tab[1, 1] # n_{h,k}: count transitions from h to k
    n01 <- tab[1, 2]
    n10 <- tab[2, 1]
    n11 <- tab[2, 2]
    p_01 <- n01 / (n00 + n01) # Conditional (given previous no exception) probability of exception
    p_11 <- n11 / (n11 + n10) # Conditional (given previous exception) probability of exception
    L_H1 <- ((1 - p_01)^n00) * (p_01^n01) * ((1 - p_11)^n10) * (p_11^n11)
    p <- (n11 + n01) / M # Marginal probability of an exception
    L_H0 <- ((1 - p)^(n00 + n10)) * (p^(n11 + n01))
    LR_ind[j] <- 2 * (log(L_H1) - log(L_H0))
  }

  # Unconditional coverage step
  LR_UC <- kupiec_test(
    mat_bool_exceptions = mat_bool_exceptions,
    alpha = alpha
  )

  # Test congiunto
  LR_CC <- LR_UC + LR_ind

  result <- list(
    LR_UC = LR_UC,
    LR_ind = LR_ind,
    LR_CC = LR_CC
  )
  return(result)
}



# Pseudo R^2 ------------------------------------------------------------------------
# Goodness-of-fit measure for quantile regression
# pseudo_R2 <- function(){







# }





# Select \lambda for l_1 penalized quantile regression -------------------------------
# Two approaches: 
# 1. Cross-validation (CV) approach 
selectLambdaCv <- function(data, lambdas, nFolds, type = c("plain", "dependant")){
  # data is a data frame with the response variable in the first column and the predictors in the remaining columns
  # lambdas is a vector of candidate values for \lambda
  # nFolds is the number of folds for cross-validation
  # type is either
  # - "plain" (for independent folds, i.e. random sampling of observations) 
  # - "dependant" (for dependent folds i.e. block sampling of observations)



}

selectLambdaSim <- function(data, B){
  # data is a data frame with the response variable in the first column and the predictors in the remaining columns
  # B is the number of simulations to perform
  

  
}