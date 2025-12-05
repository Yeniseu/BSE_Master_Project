# Author: Orhun Ozel
# Date: 26/11/2025
# Scope: Apply Random Forest
rm(list = ls())
source("01_RScript/00_Functions_Lasso.R")
library(data.table)
library(glmnet)
library(ggplot2)
options(print.max = 300, scipen = 50, digits = 5)

### Load & prepare
fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

fred <- fred[!is.na(inf),]  ## Remove rows if inflation is NA
data <- fred[, -c("date")]  
data <- data[, sapply(data, function(x) sum(is.na(x))==0), with = F]  # Drop cols with NA
data <- as.matrix(data)

fred[, which(date=="2000-12-01")]
fred[, which(date=="2015-12-01")]
dt_s1 <- data[1:fred[, which(date=="2015-12-01")], ]
dt_s2 <- copy(data)

### Run for different lags and samples
## Sample 1: Train: 1960-01-01:2000-12-01.  Test: 2001-01-01:2015-12-01 
npred1 <- nrow(dt_s1) - fred[, which(date=="2000-12-01")]  # 180
Y_train_val1 <- dt_s1[1:(nrow(dt_s1)-npred1),]
#
best_lam_lasso_all_1 <- get_best_lambda(Y_train_val1, npred1, 1, lag=1, alpha=1, nlambda=25)  #0.016912
blam_l1 <- best_lam_lasso_all_1$best_lam  # best_lam_lasso_1
#blam_l1 <- 0.016912  
best_lam_ridge_all_1 <- get_best_lambda(Y_train_val1, npred1, 1, lag=1, alpha=0, nlambda=25)  #0.016912
blam_r1 <- best_lam_ridge_all_1$best_lam  # best_lam_ridge_1
#blam_r1 <- 2.0489
best_alp_all_1 <- get_best_alpha(Y_train_val1, npred1, 1, lag=1, alpha_grid="el", lambda="auto")
balp1 <- best_alp_all_1$best_alp # 0.6
blam_e1 <- best_alp_all_1$best_lam # 0.6
#balp_e1 <- 0.4
#blam_e1 <- 0.049153 


lasso_s1_l1 <- lasso_roll_win(dt_s1, npred1, 1, lag=1, alpha=1    , lambda=blam_l1)
lasso_s1_l3 <- lasso_roll_win(dt_s1, npred1, 1, lag=3, alpha=1    , lambda=blam_l1)
ridge_s1_l1 <- lasso_roll_win(dt_s1, npred1, 1, lag=1, alpha=0    , lambda=blam_r1)
ridge_s1_l3 <- lasso_roll_win(dt_s1, npred1, 1, lag=3, alpha=0    , lambda=blam_r1)
elnet_s1_l1 <- lasso_roll_win(dt_s1, npred1, 1, lag=1, alpha=balp1, lambda=blam_e1)
elnet_s1_l3 <- lasso_roll_win(dt_s1, npred1, 1, lag=3, alpha=balp1, lambda=blam_e1)

## Sample 2: Train: 1960-01-01:2015-12-01.  Test: 2016-01-01:2024-12-01
npred2 <- nrow(fred) - fred[, which(date=="2015-12-01")]  # 108 as of 2024-12-01
blam_l2 <- blam_l1
blam_r2 <- blam_r1
blam_e2 <- blam_e1
balp2   <- balp1
lasso_s2_l1 <- lasso_roll_win(dt_s2, npred2, 1, lag=1, alpha=1    , lambda=blam_l2)
lasso_s2_l3 <- lasso_roll_win(dt_s2, npred2, 1, lag=3, alpha=1    , lambda=blam_l2)
ridge_s2_l1 <- lasso_roll_win(dt_s2, npred2, 1, lag=1, alpha=0    , lambda=blam_r2)
ridge_s2_l3 <- lasso_roll_win(dt_s2, npred2, 1, lag=3, alpha=0    , lambda=blam_r2)
elnet_s2_l1 <- lasso_roll_win(dt_s2, npred2, 1, lag=1, alpha=balp2, lambda=blam_e2)
elnet_s2_l3 <- lasso_roll_win(dt_s2, npred2, 1, lag=3, alpha=balp2, lambda=blam_e2)


### Create Charts
## Lambda Grid Search
lambda_search <- sapply(best_lam_all_1$all_res, function(x) c("lambda"=x$lambda, x$errors, "n"=sum(x$coef[1,] != 0)))
lambda_search <- as.data.table(t(lambda_search))

best_lam    <- lambda_search[rmse==min(rmse), lambda]
ggplot(lambda_search, aes(x=lambda, y=rmse)) + geom_line() + theme_light() + 
  geom_vline(xintercept=best_lam   , linetype="dashed", color="red") 
ggplot(lambda_search) + aes(x=lambda, y =n   ) + geom_line() + theme_light() + 
  geom_vline(xintercept=best_lam, linetype="dashed", color="red")

## Alpha Grid Search for Elastic Net
alpha_search <- sapply(best_alp_all_1$all_res, function(x) c(x$alpha, x$lambda, x$errors))
alpha_search <- as.data.table(t(alpha_search))
alpha_search

best_alpha <- alpha_search[rmse==min(rmse), alpha]
ggplot(alpha_search_line1, aes(x=alpha, y=rmse)) + geom_line() + theme_light() + 
  geom_vline(xintercept=best_alpha, linetype="dashed", color="red") 

# TODO Add barchart for variable coefficients
# TODO Forecast chart 
