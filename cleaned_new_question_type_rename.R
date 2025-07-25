# Load necessary libraries
library(readr)
library(dplyr)
library(stringr)

# Read the dataset
#raw_data <- read_csv("Riskwise_data/New_Question_type_raw.csv")
raw_data <- read_csv("Riskwise_data/sharepoint_data_2025-07-25.csv", show_col_types = FALSE)
# Get the original column names
original_colnames <- colnames(raw_data)

# Clean the column names
# 1. Convert to lowercase
# 2. Replace all non-alphanumeric characters with an underscore
# 3. Replace multiple consecutive underscores with a single one
# 4. Remove any trailing underscores
cleaned_colnames <- original_colnames %>%
  str_to_lower() %>%
  str_replace_all("[^a-z0-9]+", "_") %>%
  str_replace_all("_+", "_") %>%
  str_remove("_$")

# Assign the new, cleaned column names to the data frame
colnames(raw_data) <- cleaned_colnames

# --- Verification ---
# Print the new column names to verify the changes
print(colnames(raw_data))

# Display the first few rows of the data with the new column names
head(raw_data)

# --- Save the result ---
# Save the data with renamed columns to a new CSV file
write_csv(raw_data, "Riskwise_data/cleaned_sharepoint_data_2025-07-25.csv")
