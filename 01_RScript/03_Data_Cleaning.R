# Author: Silvia Ianeselli
# Date: 24/11/2025
# Scope: This script uses data_transformed created in 01_Data_Transformation.R and cleans it by 
  # 1. keeping only the data range of interest (1960-01-01 - 2024-12-31)
  # 2. Investigating the NAs in the data and dealing with them

rm(list = ls())

# load data
folder_path <- "C:/Users/sianeselli/OneDrive - Charles River Associates International/A. BSE/Data science-LONSIANESELLI2/Group Project/BSE_DataProject/03_Output"
data_transformed <- readRDS(file.path(folder_path, "data_transformed.rds"))

# data_transformed has 800 obs and 127 variables

# keep data from January 1960 to Dec 2024 only - 780 observations
data_cleaned <- data_transformed[
  data_transformed$date >= as.Date("1960-01-01") &
    data_transformed$date <= as.Date("2024-12-31"),
]
# Data now has 780 obs and 127 variables


# Investigate columns with NAs to decide how to deal with them


# ACOGNO is missing in the dataset until 1992.
# Therefore, the only way to fill in the missing values would be to apply Next Observation Carried Backward (NOCB) method
# that is, to fill all previous missing values with the latest available value.
# However, this would result in issues like data leakage and increased variance.
# Therefore, the only way to deal with this is by removing the variable ACOGNO.


# UMCSENTx - Consumer Sentiment Index
# TWEXAFEGSMTHx - TWEXMMTH Trade Weighted U.S. Dollar Index: Major Currencies


# Count NAs per variable
na_counts <- sapply(data_cleaned, function(x) sum(is.na(x)))

# Keep only variables with >0 NAs and order by decreasing NA count
vars_to_check <- names(sort(na_counts[na_counts > 0], decreasing = TRUE))
print(vars_to_check)


# Initialize list to store results
na_summary_list <- list()

for (varname in vars_to_check) {
  
  # Subset rows where variable is NA
  na_rows <- data_cleaned[is.na(data_cleaned[[varname]]), ]
  
  # Min and max date for missing values
  min_date <- if(nrow(na_rows) > 0) min(na_rows$date, na.rm = TRUE) else NA
  max_date <- if(nrow(na_rows) > 0) max(na_rows$date, na.rm = TRUE) else NA
  
  # Count missing values per year
  missing_years <- format(na_rows$date, "%Y")
  year_table <- table(missing_years)
  
  # Store in list
  na_summary_list[[varname]] <- list(
    min_date = min_date,
    max_date = max_date,
    missing_per_year = year_table
  )
  
}

# View results
na_summary_list

# Drop variables with NAs for which we cannot extrapolate a value
data_cleaned <- data_cleaned[, setdiff(names(data_cleaned), 
                                       c("ACOGNO", "UMCSENTx", "TWEXAFEGSMTHx", "ANDENOx", "VIXCLSx"))]

# now the data has 780 obs and 122 variables

# TO DO: deal with variables CP3Mx and COMPAPFFx which only have 2 and 1 missing values respectively in 2020
