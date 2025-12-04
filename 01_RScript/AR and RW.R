# ============================================================================
# BENCHMARK MODELS: RANDOM WALK AND AUTOREGRESSIVE
# ============================================================================

# Load your data
data <- readRDS("02_Input/data_cleaned.rds")

# Setup
target_col <- which(colnames(data_matrix) == "CPIAUCSL")
nprev <- 120  # Out-of-sample period

cat("Target variable:", colnames(data_matrix)[target_col], "\n")
cat("Out-of-sample observations:", nprev, "\n\n")

# ============================================================================
# FUNCTIONS
# ============================================================================

runAR <- function(Y, indice, lag, type="fixed"){
  Y2 <- cbind(Y[, indice])
  aux <- embed(Y2, 4 + lag)  # h=4 forecasting
  y <- aux[, 1]
  X <- aux[, -c(1:(ncol(Y2)*lag))]  
  
  if(lag == 1){
    X.out <- tail(aux, 1)[1:ncol(X)]  
  } else {
    X.out <- aux[, -c(1:(ncol(Y2)*(lag-1)))]
    X.out <- tail(X.out, 1)[1:ncol(X)]
  }
  
  if(type == "fixed"){
    model <- lm(y ~ X)
    coef <- coef(model)
  }
  
  if(type == "bic"){
    bb <- Inf
    for(i in seq(1, ncol(X), 1)){
      m <- lm(y ~ X[, 1:i])
      crit <- BIC(m)
      if(crit < bb){
        bb <- crit
        model <- m
        ar.coef <- coef(model)
      }
    }
    coef <- rep(0, ncol(X) + 1)
    coef[1:length(ar.coef)] <- ar.coef
  }
  
  pred <- c(1, X.out) %*% coef
  return(list("pred" = pred))
}

ar.rolling.window <- function(Y, nprev, indice, lag, type="fixed"){
  save.pred <- matrix(NA, nprev, 1)
  
  for(i in nprev:1){
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), ]
    fact <- runAR(Y.window, indice, lag, type)
    save.pred[(1 + nprev - i), ] <- fact$pred
  }
  
  real <- Y[, indice]
  actual <- tail(real, nprev)
  
  # Calculate MSE
  mse <- mean((actual - save.pred)^2)
  
  return(list("mse" = mse, "pred" = save.pred, "actual" = actual))
}

# ============================================================================
# RUN BENCHMARKS
# ============================================================================

cat("Running AR benchmarks (h=4)...\n\n")

ar1 <- ar.rolling.window(data_matrix, nprev, target_col, lag=1, type="fixed")
ar4 <- ar.rolling.window(data_matrix, nprev, target_col, lag=4, type="fixed")
ar12 <- ar.rolling.window(data_matrix, nprev, target_col, lag=12, type="fixed")
ar_bic <- ar.rolling.window(data_matrix, nprev, target_col, lag=12, type="bic")

# ============================================================================
# RESULTS
# ============================================================================

results <- data.frame(
  Model = c("AR(1)", "AR(4)", "AR(12)", "AR-BIC"),
  MSE = c(ar1$mse, ar4$mse, ar12$mse, ar_bic$mse)
)

cat("=== AR BENCHMARK RESULTS (h=4) ===\n")
print(results, row.names = FALSE, digits = 6)
cat("\nBest model:", results$Model[which.min(results$MSE)], "\n")

