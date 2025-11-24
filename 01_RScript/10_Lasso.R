# Author: Orhun Ozel
# Date: 24/11/2025
# Scope: Apply Lasso Ridge Elastic Net 

library(data.table)
library(glmnet)

rm(list=ls())
options(print.max = 300)
options(scipen = 30)
options(digits = 5)

### Load Data
fred <- readRDS("02_Input/data_transformed.rds")
setDT(fred)
setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

### Create Reg Variables
fred <- na.omit(fred, cols="inf")
Y <- fred$inf
X <- fred[, -c("date", "inf")]
X <- X[, sapply(X, function(x) sum(is.na(x))) == 0, with = F]
X <- scale(X) # Standardize predictors


# Rolling Window Lasso/Ridge/Elastic Net (time-series CV for lambda)
rolling_glmnet <- function(Y, X, window, alpha, lambda_grid = 10^seq(-3, 1, length = 20)) {
  n <- length(Y)
  preds <- rep(NA, n - window)
  
  for (i in 1:(n - window)) {
    idx <- i:(i + window - 1)
    y_train <- Y[idx]
    x_train <- as.matrix(X[idx, ])
    x_new   <- as.matrix(X[i + window, , drop = FALSE])
    
    # --- Skip if y_train is constant ---
    if (length(unique(y_train)) <= 1) {
      preds[i] <- y_train[1]  # fallback: repeat value
      next
    }
    
    # --- time-series CV for lambda selection ---
    cv_mse <- sapply(lambda_grid, function(lambda) {
      y_hat <- numeric(length(y_train) - 1)
      for (t in 2:length(y_train)) {
        # skip if the first t-1 observations are constant
        if (length(unique(y_train[1:(t-1)])) <= 1) {
          y_hat[t-1] <- y_train[t-1]
        } else {
          fit <- glmnet(as.matrix(x_train[1:(t-1), , drop = FALSE]), 
                        y_train[1:(t-1)], 
                        alpha = alpha, lambda = lambda)
          y_hat[t-1] <- predict(fit, as.matrix(x_train[t, , drop = FALSE]))
        }
      }
      mean((y_train[2:length(y_train)] - y_hat)^2)
    })
    
    best_lambda <- lambda_grid[which.min(cv_mse)]
    final_fit <- glmnet(x_train, y_train, alpha = alpha, lambda = best_lambda)
    preds[i] <- predict(final_fit, x_new)
  }
  
  preds
}


# Alternative: Rolling Window Lasso Ridge Function
rolling_glmnet <- function(Y, X, window, alpha) {
  browser()
  n <- length(Y)
  preds <- NA
  
  for (i in 1:(n - window)) {
    idx <- i:(i + window - 1)
    y_train <- Y[idx]
    x_train <- X[idx, ]
    x_new   <- X[i + window, , drop = FALSE]
    
    fit <- cv.glmnet(x_train, y_train, alpha = alpha)
    preds[i] <- predict(fit, x_new)
  }
  preds
}


# Run Models
w <- 12    # rolling window = 1 years

pred_lasso <- rolling_glmnet(Y, X, w, alpha = 1)
pred_ridge <- rolling_glmnet(Y, X, w, alpha = 0)
pred_enet  <- rolling_glmnet(Y, X, w, alpha = 0.5)

# Show forecasts 
tail(cbind(pred_lasso, pred_ridge, pred_enet))
