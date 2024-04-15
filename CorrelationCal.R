
# Load the libraries
library(readxl)
library(dplyr)
library(mice)
library(ggplot2)
library(corrplot)

# Read in the the Excel file that contains the data
survey_data <- read_excel("/Users/r0hak/Downloads/4th Year/CS Final Year Project/R Correlation.xlsx")

# Convert survey questions to numeric and apply multiple imputation
survey_data <- survey_data %>%
  mutate(across(everything(), as.numeric))
imputed_data <- mice(survey_data, m=5, method='pmm', seed=500)
completed_data <- complete(imputed_data, 1)  # Using the first imputation

# Correlation Analysis using Spearman method
correlation_matrix <- cor(completed_data, use = "complete.obs", method = "spearman")





