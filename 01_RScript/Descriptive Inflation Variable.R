# ============================================================================
# DESCRIPTIVE STATISTICS AND EXPLORATORY DATA ANALYSIS
# Inflation Forecasting Using Machine Learning Methods
# Author: Emilia Infante
# Date: 28.11.2025
# ============================================================================

# ----------------------------------------------------------------------------
# Summary Statistics
# ----------------------------------------------------------------------------
summary(data_transformed)

# ----------------------------------------------------------------------------
# Figure 1: Temporal Evolution of US Monthly Inflation Rate
# ----------------------------------------------------------------------------

plot(data_transformed$date, data_transformed$CPIAUCSL, 
     type = "l", 
     main = "US Monthly Inflation Rate (1960-2025)",
     xlab = "Date", 
     ylab = "Inflation Rate (%)",
     col = "steelblue",
     lwd = 1.5,
     las = 1)
abline(h = 0, col = "red", lty = 2, lwd = 1.2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray90")

# ----------------------------------------------------------------------------
# Figure 2: Distribution of Monthly Inflation Rates
# ----------------------------------------------------------------------------

hist(data_transformed$CPIAUCSL, 
     breaks = 50,
     main = "Distribution of US Monthly Inflation Rates",
     xlab = "Inflation Rate (%)",
     ylab = "Frequency",
     col = "lightblue",
     border = "white",
     las = 1)
abline(v = mean(data_transformed$CPIAUCSL, na.rm = TRUE), 
       col = "red", lwd = 2, lty = 2)
legend("topright", 
       legend = c("Mean"), 
       col = c("red"), 
       lty = 2, 
       lwd = 2,
       bty = "n")

# ----------------------------------------------------------------------------
# Variable Selection for Correlation Analysis
# ----------------------------------------------------------------------------
# Selected key macroeconomic and financial indicators based on economic theory
selected_vars <- c(
  "CPIAUCSL",       # Consumer Price Index (All Urban Consumers)
  "WPSFD49207",     # Producer Price Index: Finished Goods
  "WPSID61",        # Producer Price Index: Intermediate Materials
  "OILPRICEx",      # Crude Oil Prices (WTI)
  "ACOGNO",         # Orders for Consumer Goods
  "UNRATE",         # Unemployment Rate
  "M2REAL",         # Real M2 Money Stock
  "INDPRO",         # Industrial Production Index
  "PAYEMS",         # Total Nonfarm Payrolls
  "FEDFUNDS",       # Federal Funds Rate
  "GS10",           # 10-Year Treasury Constant Maturity Rate
  "HOUST"           # Housing Starts
)

# ----------------------------------------------------------------------------
# Correlation Matrix Computation
# ----------------------------------------------------------------------------
# Compute pairwise correlations using complete observations to handle 
# potential missing values in the dataset
cor_matrix <- cor(data_transformed[, selected_vars], 
                  use = "pairwise.complete.obs")

# Display correlation matrix (rounded to 2 decimal places)
cat("\nCorrelation Matrix:\n")
print(round(cor_matrix, 2))

# ----------------------------------------------------------------------------
# Figure 3: Correlation Matrix Heatmap
# ----------------------------------------------------------------------------

corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.7,
         addCoef.col = "black",
         number.cex = 0.5,
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
         title = "Correlation Matrix: Inflation and Key Macroeconomic Predictors",
         mar = c(0,0,2,0))
