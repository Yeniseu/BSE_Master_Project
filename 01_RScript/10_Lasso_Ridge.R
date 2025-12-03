library(data.table)
library(glmnet)
library(caret)
rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

### Load & prepare
fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

fred <- na.omit(fred, cols = "inf")  ## Remove rows if inflation is NA
Y    <- fred$inf
Xdt  <- fred[, -c("date", "inf")]  
Xdt  <- Xdt[, sapply(Xdt, function(x) sum(is.na(x))==0), with = F]  # Drop cols with NA
X    <- as.matrix(Xdt)

### Rolling glmnet with forward TS-CV for lambda
roll_wind_fcast <- function(y, X, window_size, step=1, alpha_lasso, lambda_lasso, anchored=F, skip=0) {
  # Create each rolling window, then forecast
  roll_index <- createTimeSlices(y, window_size, step, fixedWindow = T, skip = skip)
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
  bias_mean <- rowMeans(y_true_all - y_hat_all)
  bias_percantage <- abs(bias_mean)/abs(rowMeans(y_true_all))*100
  bias_mean
  bias_percantage
  rmse <- sqrt(rowMeans((y_true_all - y_hat_all)^2))
  rolling_res <- list("rmse" = rmse, "forecast" = y_hat_all, "real" = y_true_all)
  rolling_res
}

get_best_lambda <- function(y, X, window_size, step, alpha_lasso) {
  #browser()
  # Create lambda grid automatically
  fit0 <- glmnet(X[1:window_size, , drop = F], y[1:window_size], 
                 alpha = alpha_lasso, standardize = T, nlambda = 20)
  lambda_grid <- fit0$lambda
  if(alpha_lasso == 1) {  # Remove lambda possibilities that does not change variable count
    lambda_grid <- lambda_grid[fit0$df != shift(fit0$df, fill=-1)]  
  }
  rolling_res <- list(NA)
  rolling_rmse <- NA
  for (i in 1:length(lambda_grid)) {
    #browser()
    rolling_res[[i]] <- roll_wind_fcast(y, X, window_size, step=step, alpha_lasso, 
                                        lambda_grid[i], anchored=F)
    rolling_rmse[i] <- rolling_res[[i]]$rmse[step]
  }

  # best lambda
  best_lam_index <- which.min(rolling_rmse)
  best_lam_and_mse <- c(lambda_grid[best_lam_index], rolling_rmse[best_lam_index])
  return(best_lam_and_mse)
}

rolling_glmnet_ts <- function(y_all, X_all, window_size, train_size, step=1, alpha_lasso) {
  # y_all vector. X_all matrix.
  if(length(y_all) != nrow(X_all)) stop("Y and X lengths does not match")
  y_train <- y_all[1:train_size]
  X_train <- X_all[1:train_size, ,drop=F]
  best_lam_and_rmse <- matrix(NA, length(alpha_lasso), 3) # Store alpha, best lambda and its rmse
  for(i in 1:length(alpha_lasso)) {
    lam_and_rmse <- get_best_lambda(y_train, X_train, window_size, step=step, alpha_lasso[i])
    best_lam_and_rmse[i, ] <- c(alpha_lasso[i], lam_and_rmse)
  }
  best_comb_index <- which.min(best_lam_and_rmse[, 3])
  best_alpha <- best_lam_and_rmse[best_comb_index, 1]
  best_lam   <- best_lam_and_rmse[best_comb_index, 2]

  #fit_full <- glmnet(X, y, alpha=alpha_lasso, lambda=best_lam, standardize=T)
  skip_for_2001_2015 <- train_size-window_size
  skip_for_2016_2025 <- skip_for_2001_2015 + (2016-2001)*12
  
  #browser()
  roll_fcast_1 <- roll_wind_fcast(y_all, X_all, window_size, step=step, best_alpha,
                                 best_lam, anchored=F, skip = skip_for_2001_2015)
  roll_fcast_2 <- roll_wind_fcast(y_all, X_all, window_size, step=step, best_alpha,
                                 best_lam, anchored=F, skip = skip_for_2016_2025)
  all_res <- list(roll_fcast_1, roll_fcast_2)
  all_res
}

### Run models
w <- 360  # 30 years of monthly data
t_s <- fred[, which(date=="2000-12-01")]
pred_lasso <- rolling_glmnet_ts(Y, X, window = w, train_size=t_s, step=1, alpha = 1)     # Lasso
pred_ridge <- rolling_glmnet_ts(Y, X, window = w, train_size=t_s, step=1, alpha = 0)     # Ridge
pred_elnet <- rolling_glmnet_ts(Y, X, window = w, train_size=t_s, step=1, alpha = seq(0, 1, by = 0.1))   # Elastic Net





