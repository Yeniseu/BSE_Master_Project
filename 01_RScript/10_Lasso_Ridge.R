library(data.table)
library(glmnet)
rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

### Load & prepare
fred <- readRDS("02_Input/data_transformed.rds")
setDT(fred)
setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

fred <- na.omit(fred, cols = "inf")  ## Remove rows if inflation is NA
Y    <- fred$inf
Xdt  <- fred[, -c("date", "inf")]  
Xdt  <- Xdt[, sapply(Xdt, function(x) sum(is.na(x))==0), with = F]  # Drop cols with NA
X    <- as.matrix(Xdt)

### Rolling glmnet with forward TS-CV for lambda
get_best_lambda <- function(y, X, window_size, alpha) {
  #browser()
  n      <- length(y)
  n_out  <- n - window_size
  window <- window_size
  
  # Create lambda grid automatically
  fit0 <- glmnet(X[1:window, , drop = F], y[1:window], 
                 alpha = alpha, standardize = T)
  lambda_grid <- fit0$lambda
  
  err_sum   <- rep(0, times = length(lambda_grid))
  err_count <- 0
  # Create each rolling window, then forecast using every lambda
  for (k in 1:n_out) {
    y_window   <- y[(1:window)+ k-1]
    X_window   <- X[(1:window)+ k-1, , drop = F]
    X_forecast <- X[window + k     , , drop = F]
    
    # TS-CV: expanding 1-step-ahead within the window
    mse <- NA
    for (j in seq_along(lambda_grid)) {
      lam <- lambda_grid[j]
      fit <- glmnet(X_window, y_window, alpha=alpha, lambda=lam, standardize=T)
      yhat <- as.numeric(predict(fit, X_forecast))
      err_sum[j] <- err_sum[j] + (y[window + k] - yhat)^2
    }
    err_count  <- err_count + 1
  }
  mse <- err_sum / err_count
  # best lambda
  best_lam <- lambda_grid[which.min(mse)]
  return(best_lam)
}

rolling_glmnet_ts <- function(y_all, X_all, window_size, train_size, alpha) {
  # y_all vector. X_all matrix.
  if(length(y_all) != nrow(X_all)) stop("Y and X lengths does not match")
  train_index <- 1:train_size
  fcast_index <- (train_size+1):length(y_all)
  y      <- y_all[train_index]
  X      <- X_all[train_index, ]
  best_lam <- get_best_lambda(y, X, window_size, alpha)
  fit_full <- glmnet(X, y, alpha=alpha, lambda=best_lam, standardize=T)
  preds <- as.numeric(predict(fit_full, X_all[fcast_index, , drop=F]))
  preds
}

### Run models
w <- 120  # 10 years of monthly data
t_s <- 240
pred_lasso <- rolling_glmnet_ts(Y, X, window = w, train_size=t_s, alpha = 1)     # Lasso
pred_ridge <- rolling_glmnet_ts(Y, X, window = w, alpha = 0)     # Ridge
pred_elnet <- rolling_glmnet_ts(Y, X, window = w, alpha = 0.5)   # Elastic Net

# Align forecasts with actual Y (they start at t = w+1)
y_actual <- Y[(w + 1):length(Y)]
tail(cbind(
  y_actual   = y_actual,
  lasso_hat  = pred_lasso,
  ridge_hat  = pred_ridge,
  enet_hat   = pred_enet
))