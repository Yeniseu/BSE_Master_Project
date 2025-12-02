library(data.table)
library(glmnet)
library(caret)

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
rolling_window_fcast <- function(y, X, window_size, step=1, alpha_lasso, lambda_lasso, anchored=F) {
  browser()
  n      <- length(y)
  n_out  <- n - window_size
  window <- window_size
  
  # Create each rolling window, then forecast
  roll_index <- createTimeSlices(y, window_size, step, fixedWindow = T)
  data_all <- cbind(y, X)
  train <- lapply(roll_index$train, function(x) data_all[x, ,drop=F])
  test  <- lapply(roll_index$test , function(x) data_all[x, ,drop=F])
  
  y_true_all <- lapply(test, function(m) m[, "y"])
  y_true_all <- as.matrix(list2DF(y_true_all))
  y_hat_all  <- matrix(NA, nrow(y_true_all), ncol(y_true_all))
  for (i in 1:length(train)) {
    all_variables <- train[[i]]
    Y <- all_variables[, "y"]
    X <- all_variables[,  colnames(all_variables) != "y"]
    y_true <- test[[i]][, "y"]
    
    # TS-CV: expanding 1-step-ahead within the window
    fit   <- glmnet(X, Y, alpha=alpha_lasso, lambda=lambda_lasso, standardize=T)
    y_hat <- as.numeric(predict(fit, test[[i]][,  colnames(all_variables) != "y"]))
    y_hat_all[,i] <- y_hat
  }
  
  mse <- err_sum / err_count
  rolling_res <- list("mse" = mse, "forecast" = forecast)
  rolling_res
}

get_best_lambda <- function(y, X, window_size, step, alpha_lasso) {
  #browser()
  # Create lambda grid automatically
  fit0 <- glmnet(X[1:window_size, , drop = F], y[1:window_size], 
                 alpha = alpha_lasso, standardize = T)
  lambda_grid <- fit0$lambda
  rolling_res <- list(NA)
  rolling_mse <- NA
  for (i in 1:length(lambda_grid)) {
    #browser()
    rolling_res[[i]] <- rolling_window_fcast(y, X, window_size, step=1, alpha_lasso, 
                                             lambda_grid[i], anchored=F)
    rolling_mse[i] <- rolling_res[[i]]$mse
  }
  #browser()
  # best lambda
  best_lam <- lambda_grid[which.min(rolling_mse)]
  return(best_lam)
}

rolling_glmnet_ts <- function(y_all, X_all, window_size, train_size, step=1, alpha_lasso) {
  # y_all vector. X_all matrix.
  if(length(y_all) != nrow(X_all)) stop("Y and X lengths does not match")
  train_index <- 1:train_size
  fcast_index <- (train_size+1):length(y_all)
  y      <- y_all[train_index]
  X      <- X_all[train_index, ,drop=F]
  best_lam <- get_best_lambda(y, X, window_size, step, alpha_lasso)
  fit_full <- glmnet(X, y, alpha=alpha_lasso, lambda=best_lam, standardize=T)
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