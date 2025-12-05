
#Plots

plot_dir <- "03_Output/Graphs/plots_rf"

if (!dir.exists("03_Output"))          dir.create("03_Output")
if (!dir.exists("03_Output/Graphs"))   dir.create("03_Output/Graphs")
if (!dir.exists(plot_dir))             dir.create(plot_dir)

pred_full <- c(rep(NA, length(real) - nprev), save.pred)

plot_file_full <- file.path(plot_dir, sprintf("rf_lag%d_full.png", lag))

png(plot_file_full,
    width  = 1920,
    height = 1080,
    res    = 300,
    type   = "cairo-png")

plot(real, type = "l",
     main = sprintf("Random Forest Forecast – Lag %d (Full Series)", lag),
     xlab = "Time",
     ylab = "Inflation")
lines(pred_full, col = "red", lwd = 2)
legend("topleft",
       legend = c("Actual", "RF Forecast"),
       col    = c("black", "red"),
       lty    = 1,
       bty    = "n")

dev.off()

#Plot 2: Shorter period
start_idx <- max(1, length(real) - 2 * nprev + 1)

real_zoom <- real[start_idx:length(real)]
pred_zoom <- pred_full[start_idx:length(real)]

plot_file_zoom <- file.path(plot_dir, sprintf("rf_lag%d_last2nprev.png", lag))

png(plot_file_zoom,
    width  = 1920,
    height = 1080,
    res    = 300,
    type   = "cairo-png")

plot(real_zoom, type = "l",
     main = sprintf("Random Forest Forecast – Lag %d (Last %d Periods)", lag, 2 * nprev),
     xlab = "Time",
     ylab = "Inflation")
lines(pred_zoom, col = "red", lwd = 2)
legend("topleft",
       legend = c("Actual", "RF Forecast"),
       col    = c("black", "red"),
       lty    = 1,
       bty    = "n")

dev.off()
