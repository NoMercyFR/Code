library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)

data <- read.csv("C:/Users/cleme/Documents/Master_Allemagne/Topic_Economic_History/Dataset_code/final_data/GDL_2010_merged_common_rows.csv")

##### mean, variance, std################"


cat("Variance Comprehensive Subnational Corruption Index:", var(data$fullsci, na.rm = TRUE), "\n")
cat("Variance Mean years of schooling:", var(data$msch, na.rm = TRUE), "\n")
cat("Variance Population in millions:", var(data$popshare, na.rm = TRUE), "\n")
cat("Variance Household size:", var(data$hhsize, na.rm = TRUE), "\n")
cat("Variance International Wealth Index:", var(data$iwi, na.rm = TRUE), "\n")
cat("Variance Percentage of People living urban:", var(data$urban, na.rm = TRUE), "\n")
cat("Variance Gini coefficient:", var(data$gini, na.rm = TRUE), "\n")
cat("Variance Infant Mortality:", var(data$infmort, na.rm = TRUE), "\n")
cat("Variance Piped water:", var(data$pipedwater, na.rm = TRUE), "\n")
cat("Variance Electricity:", var(data$electr, na.rm = TRUE), "\n")

####### Covariance matrix ##############

columns <- c("fullsci", "msch", "popshare","hhsize","iwi", "urban" , "gini" , "infmort" ,"pipedwater" ,"electr")

covariance_matrix <- cov(data[columns], use = "complete.obs")
correlation_matrix <- cor(data[columns], use = "complete.obs")

print(covariance_matrix)
print(correlation_matrix)
