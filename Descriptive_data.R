library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)

data <- read.csv("C:/Users/cleme/Documents/Master_Allemagne/Topic_Economic_History/Dataset_code/data_set_2010_allvar.csv")

##### mean, variance, std################"
"""
cat("GDL Vulnerability Index:", mean(data$gvi, na.rm = TRUE), "\n")
cat("Comprehensive Subnational Corruption Index:", mean(data$fullsci, na.rm = TRUE), "\n")
cat("Mean years of education of adults aged 25+:", mean(data$edyr25, na.rm = TRUE), "\n")
cat("Population in millions:", mean(data$regpopm, na.rm = TRUE), "\n")
cat("Household size:", mean(data$hhsize, na.rm = TRUE), "\n")
cat("International Wealth Index:", mean(data$iwi, na.rm = TRUE), "\n")

cat(" Variance GDL Vulnerability Index:", var(data$gvi, na.rm = TRUE), "\n")
cat(" VarianceComprehensive Subnational Corruption Index:", var(data$fullsci, na.rm = TRUE), "\n")
cat(" Variance Mean years of education of adults aged 25+:", var(data$edyr25, na.rm = TRUE), "\n")
cat(" Variance Population in millions:", var(data$regpopm, na.rm = TRUE), "\n")
cat(" Variance Household size:", var(data$hhsize, na.rm = TRUE), "\n")
cat(" Variance International Wealth Index:", var(data$iwi, na.rm = TRUE), "\n")
"""
#data %>%
#  summarise(across(where(is.numeric), list(mean = ~mean(., na.rm = TRUE), 
#                                           var = ~var(., na.rm = TRUE))))

####### Covariance matrix ##############

columns <- c("gvi","fullsci","edyr25", "regpopm","iwi","hhsize")

covariance_matrix <- cov(data[columns], use = "complete.obs")
correlation_matrix <- cor(data[columns], use = "complete.obs")

print(covariance_matrix)
print(correlation_matrix)
