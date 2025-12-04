# Author: ...
# Date: 26/11/2025
# Scope: Apply Random Forest
rm(list = ls())
source("01_RScript/00_Functions.R")
library(data.table)
library(glmnet)
library(caret)
options(print.max = 300, scipen = 30, digits = 5)

### Load & prepare
fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

fred <- fred[!is.na(inf),]  ## Remove rows if inflation is NA
dt <- fred[, -c("date")]  
dt <- dt[, sapply(dt, function(x) sum(is.na(x))==0), with = F]  # Drop cols with NA
dt <- as.matrix(dt)

fred[, which(date=="2000-12-01")]
fred[, which(date=="2015-12-01")]
dt_s1 <- dt[1:fred[, which(date=="2015-12-01")], ]
dt_s2 <- dt

### Run for different lags and samples
nprev <- 180
best_lam_all <- get_best_lambda(dt, nprev, 1, lag=1, alpha=1, nlambda=25)  #0.020048
best_lam <- best_lam_all$best_lam
#best_lam <- 0.020048
lasso_s2_l1 <- lasso_roll_win(dt_s1, nprev, 1, lag=1, alpha=1, lambda=best_lam)
lasso_s2_l3 <- lasso_roll_win(dt_s1, nprev, 1, lag=3, alpha=1, lambda=best_lam)
lasso_s2_l6 <- lasso_roll_win(dt_s1, nprev, 1, lag=6, alpha=1, lambda=best_lam)

lasso_s2_l1 <- lasso_roll_win(dt_s2, nprev, 1, lag=1, alpha=1, lambda=best_lam)
lasso_s2_l3 <- lasso_roll_win(dt_s2, nprev, 1, lag=3, alpha=1, lambda=best_lam)
lasso_s2_l6 <- lasso_roll_win(dt_s2, nprev, 1, lag=6, alpha=1, lambda=best_lam)

Y[1:(nrow(Y)-nprev),]


