# Author: Ece Tasan
# Date: 5/12/2025
# Scope: Rolling RMSE for Model Comparison

#### Comparison for First Out of Sample Period ####

lasso1 <- readRDS("03_Output/lasso_pred_s1.rds")

lasso1_1 <- lasso1[, c("real", "lasso_l1", "ridge_l1", "elnet_l1", "rw_l1")]
setnames(lasso1_1, c("real", "lasso_l1", "ridge_l1", "elnet_l1", "rw_l1"),
         c("real", "LASSO", "Ridge", "ElNet", "RW"))

lasso1_3 <- lasso1[, c("real", "lasso_l3", "ridge_l3", "elnet_l3", "rw_l3")]
setnames(lasso1_3, c("real", "lasso_l3", "ridge_l3", "elnet_l3", "rw_l3"),
         c("real", "LASSO", "Ridge", "ElNet", "RW"))

mean1_h1 <- readRDS("03_Output/p1_h1_mean.rds")
mean1_h1 <- mean1_h1$pred
mean1_h1 <- as.data.table(mean1_h1)
setnames(mean1_h1, "V1", "RSM")

mean1_h3 <- readRDS("03_Output/p1_h3_mean.rds")
mean1_h3 <- mean1_h3$pred
mean1_h3 <- as.data.table(mean1_h3)
setnames(mean1_h3, "V1", "RSM")

p1_h1_ar4 <- readRDS("03_Output/p1_h1_ar4.rds")
p1_h1_ar4 <- p1_h1_ar4$pred
p1_h1_ar4 <- as.data.table(p1_h1_ar4)
setnames(p1_h1_ar4, "V1", "AR")

p1_h3_ar4 <- readRDS("03_Output/p1_h3_ar4.rds")
p1_h3_ar4 <- p1_h3_ar4$pred
p1_h3_ar4 <- as.data.table(p1_h3_ar4)
setnames(p1_h3_ar4, "V1", "AR")

rf1_1 <- readRDS("03_Output/rf1_1.rds")
rf1_1 <- rf1_1$pred
rf1_1 <- as.data.table(rf1_1)
setnames(rf1_1, "V1", "RF")

rf1_3 <- readRDS("03_Output/rf1_3.rds")
rf1_3 <- rf1_3$pred
rf1_3 <- as.data.table(rf1_3)
setnames(rf1_3, "V1", "RF")

all1_1 <- cbind(lasso1_1, mean1_h1, p1_h1_ar4, rf1_1)
all1_3 <- cbind(lasso1_3, mean1_h3, p1_h3_ar4, rf1_3)

# Calculate Cumulative Absolute Errors
all1_1_err <- copy(all1_1)
cols <- setdiff(names(all1_1_err), "real")
all1_1_err[, (cols) := lapply(.SD, function(x) abs(x - real)), .SDcols = cols]
all1_1_err[, real := NULL]

all1_1_cum <- copy(all1_1_err)
all1_1_cum[, (names(all1_1_cum)) := lapply(.SD, cumsum)]





#### Comparison for Second Out of Sample Period ####

lasso2 <- readRDS("03_Output/lasso_pred_s2.rds")

lasso2_1 <- lasso2[, c("real", "lasso_l1", "ridge_l1", "elnet_l1", "rw_l1")]
setnames(lasso2_1, c("real", "lasso_l1", "ridge_l1", "elnet_l1", "rw_l1"),
         c("real", "LASSO", "Ridge", "ElNet", "RW"))

lasso2_3 <- lasso2[, c("real", "lasso_l3", "ridge_l3", "elnet_l3", "rw_l3")]
setnames(lasso2_3, c("real", "lasso_l3", "ridge_l3", "elnet_l3", "rw_l3"),
         c("real", "LASSO", "Ridge", "ElNet", "RW"))

mean2_h1 <- readRDS("03_Output/p2_h1_mean.rds")
mean2_h1 <- mean2_h1$pred
mean2_h1 <- as.data.table(mean2_h1)
setnames(mean2_h1, "V1", "RSM")

mean2_h3 <- readRDS("03_Output/p2_h3_mean.rds")
mean2_h3 <- mean2_h3$pred
mean2_h3 <- as.data.table(mean2_h3)
setnames(mean2_h3, "V1", "RSM")

p2_h1_ar4 <- readRDS("03_Output/p2_h1_ar4.rds")
p2_h1_ar4 <- p2_h1_ar4$pred
p2_h1_ar4 <- as.data.table(p2_h1_ar4)
setnames(p2_h1_ar4, "V1", "AR")

p2_h3_ar4 <- readRDS("03_Output/p2_h3_ar4.rds")
p2_h3_ar4 <- p2_h3_ar4$pred
p2_h3_ar4 <- as.data.table(p2_h3_ar4)
setnames(p2_h3_ar4, "V1", "AR")

rf2_1 <- readRDS("03_Output/rf2_1.rds")
rf2_1 <- rf2_1$pred
rf2_1 <- as.data.table(rf2_1)
setnames(rf2_1, "V1", "RF")

rf2_3 <- readRDS("03_Output/rf2_3.rds")
rf2_3 <- rf2_3$pred
rf2_3 <- as.data.table(rf2_3)
setnames(rf2_3, "V1", "RF")

all2_1 <- cbind(lasso2_1, mean2_h1, p2_h1_ar4, rf2_1)
all2_3 <- cbind(lasso2_3, mean2_h3, p2_h3_ar4, rf2_3)
