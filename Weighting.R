library(readxl)
library(anesrake)

# Read in Excel data from file
survey_data <- read_excel("/Users/r0hak/Downloads/4th Year/CS Final Year Project/Weighting Analysis.xlsx", na = "#NULL!")

# Convert #NULL! values explicitly to NA to avoid potential errors
survey_data[survey_data == "#NULL!"] <- NA

# Define HEA Demogrpahic proportions
inputter <- list(
  Gender = c(Male = 0.4716, Female = 0.5281, Undeclared = 0.0003),
  AgeGroup = c(Under23 = 0.6345, Over24 = 0.3654, Unknown = 0.0001),
  StudyMode = c(Fulltime = 0.7803, Parttime = 0.181, Other = 0.0387),
  InstituteType = c(University = 0.5464, TU = 0.3956, Other = 0.058)
)

# Check if InitialWeight exists and has the correct number of cases
# If it does not exist, create it with equal weights
if("InitialWeight" %in% names(survey_data)) {
  # Ensure that the InitialWeight column is numeric and has no NA values
  survey_data$InitialWeight <- as.numeric(as.character(survey_data$InitialWeight))
  # Replace any NAs with the mean weight, ensuring there are no NAs
  survey_data$InitialWeight[is.na(survey_data$InitialWeight)] <- mean(survey_data$InitialWeight, na.rm = TRUE)
} else {
  # If InitialWeight is not present, create it with equal weights
  survey_data$InitialWeight <- rep(1, nrow(survey_data))
}

# Confirm that the number of weights matches the number of cases
if(length(survey_data$InitialWeight) != nrow(survey_data)) {
  stop("The weight vector does not contain the same number of cases as the data frame.")
}

# Apply the anesrake function
raking_result <- anesrake(
  inputter = inputter,
  dataframe = survey_data,
  caseid = NULL, # If you don't have a caseid, you can set this to NULL
  weightvec = survey_data$InitialWeight,
  cap = 5,
  verbose = TRUE
)

# Attach the final weights to the survey_data dataframe
survey_data$final_weight <- raking_result$weights

# Check the first few rows of the dataframe to see the final weights
head(survey_data)
