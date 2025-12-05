# Author: Ece Tasan
# Date: 3/12/2025
# Scope: Apply Random Forest

library(data.table)
library(randomForest)
library(ggplot2)

rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

source("01_RScript/00_Functions_RF.R")

#### Load & prepare ####

fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
dim(fred)

setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

#### TUNING ####

Y <- fred[date < "2001-01-01"]
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
dim(Y)

# Validation Data Length = 120 (between years 1991-2000)
nprev <- 120

# mtry grid
p = 520 # number of features
mtry_grid <- c(2, 3, 5, 8, 10, 15, 25, round(p/10), round(p/8), round(p/6), round(p/4),
               round(p/3), round(p/2))

results_mtry <- data.frame(
  mtry = mtry_grid,
  rmse = NA_real_,
  mae  = NA_real_
)

# Grid search
for (k in seq_along(mtry_grid)) {
  cat("\n==== Testing mtry =", mtry_grid[k], "====\n")
  
  set.seed(123)
  out_k <- rf.rolling.window_tune_mtry(Y, nprev, 1, 1, nfeature = mtry_grid[k])
  results_mtry$rmse[k] <- out_k$errors["rmse"]
  results_mtry$mae[k]  <- out_k$errors["mae"]
}

results_mtry

# best mtry
best_idx  <- which.min(results_mtry$rmse)
best_mtry <- results_mtry$mtry[best_idx]

best_mtry

saveRDS(results_mtry, file = "03_Output/rfres_mtry.rds")

plot_mtry <- ggplot(results_mtry, aes(x = mtry, y = rmse)) +
      geom_line() +
      geom_point(size = 2) +
      geom_vline(xintercept = best_mtry, linetype = "dashed", color = "red") +
      scale_x_continuous(breaks = results_mtry$mtry) +
      theme_light() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )

ggsave(
  filename = "03_Output/Graphs/plots_rf/mtry_tuning.png",
  plot     = plot_mtry,
  width    = 12,     
  height   = 6,     
  dpi      = 300      
)

#results_mtry <- readRDS("03_Output/rfres_mtry.rds")

#### PREDICTIONS ####

# Tuning Result: Best mtry result
best_mtry <- 52

# FIRST Out of Sample Predictions: 2001-2015

Y <- fred[date < "2016-01-01"]
Y[inf == min(inf), date] # "2008-11-01"
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
dim(Y)

# Dummy for 
dum=rep(0,nrow(Y))
dum[which.min(Y[,1])]=1
Y=cbind(Y,dum=dum)

# Out of Sample Length = 180 (between years 2001-2015)
nprev <- 180

set.seed(123)

rf1_1 <- rf.rolling.window(Y,nprev,1,1)
saveRDS(rf1_1, file= "03_Output/rf1_1.rds")


rf1_1$errors
#rf1_1$pred


rf1_3 <- rf.rolling.window(Y,nprev,1,3)
saveRDS(rf1_3, file= "03_Output/rf1_3.rds")


rf1_3$errors
#rf1_3$pred



# SECOND Out of Sample Predictions: 2016-2024

# Tuning Result: Best mtry result
best_mtry <- 52

Y <- fred
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
dim(Y)

# Out of Sample Length = 108 (between years 2001-2015)
nprev <- 108

set.seed(123)
rf2_1 <- rf.rolling.window(Y,nprev,1,1)
saveRDS(rf2_1, file= "03_Output/rf2_1.rds")

rf2_1$errors
#rf2_1$pred

rf2_3 <- rf.rolling.window(Y,nprev,1,3)
saveRDS(rf2_3, file= "03_Output/rf2_3.rds")

rf2_3$errors
#rf2_3$pred
