```{r}
# Install and load necessary R packages
install.packages(c("readr", "dplyr"))
library(readr)
library(dplyr)
```
```{r}
# 1. Read the file as a single column
data <- readLines("IMPC_parameter_description.txt")

# 3. Remove the first two rows (because they contain column titles and other unnecessary content)
data_clean <- data[-c(1, 2)]

# 4. Check and remove any empty lines
data_clean <- data_clean[data_clean != ""]

# 5. Use regular expressions to split each line's content
# For each line, use a regular expression to separate "line_number" from the following fields
data_split <- strsplit(data_clean, '" "', perl = TRUE)

# 6. Further process the split data: remove quotes and split comma-separated fields
data_split <- lapply(data_split, function(x) {
  # Remove leading and trailing quotes
  x <- gsub('"', '', x)
  # Split fields by comma
  strsplit(x, ",\\s*")
})

# Flatten each list element into a vector
result_lapply <- lapply(data_split, function(x) {
  unlist(x)
})

# Ensure that each character vector is transposed into one row
result_as_df <- lapply(result_lapply, function(x) {
  data.frame(t(x))  # Transpose each vector to make it one row
})

# Use bind_rows to merge by rows
data_split_df <- bind_rows(result_as_df)

# Remove leading and trailing spaces for each column
data_split_df <- apply(data_split_df, 2, function(x) trimws(x))

# 9. Convert the data into a data frame and name each column
final_data <- as.data.frame(data_split_df, stringsAsFactors = FALSE)

# 10. Name the columns of the data frame
colnames(final_data) <- c("line_number", "impcParameterOrigId", "name", "description", "parameterId")

# 11. View the results
# View rows 1000 to 2000 in final_data
# Display the extracted data

# Assuming final_data is your data frame
# View rows where the 'name' column is empty
empty_rows <- final_data[is.na(final_data$parameterId), ]

# View rows where the sixth column has data
non_empty_rows <- final_data[!is.na(final_data[[6]]), ]
final_data <- final_data[!(final_data$parameterId %in% empty_rows$parameterId) & !(final_data[[6]] %in% non_empty_rows[[6]]), ]

# Step 3: Store empty_rows and non_empty_rows into a new CSV file
all_abnormal_data <- rbind(empty_rows, non_empty_rows)
write.csv(all_abnormal_data, "final_data_2.csv", row.names = FALSE)

# Step 4: View the final_data after deletion
read.csv("final_data_2.csv")
final_data <- final_data[, -((ncol(final_data)-2):ncol(final_data))]
head(final_data)

```
```{r}
# 1. Read the Modified final_data_2.csv and convert it to a data.frame
modified_data <- read.csv("Modified final_data_2.csv")  # Load the CSV file into a data.frame

# 2. Check the type of modified_data
class(modified_data)  # It should output "data.frame"

head(modified_data)
head(final_data)
write.csv(final_data, "final_data.csv", row.names = FALSE)
```
```{r}
# Read the final_data.csv and convert it to a data.frame, saved to the final_data variable
final_data <- read.csv("final_data.csv")
tail(final_data)

library(tools)

# Target folder path, assuming there is a subfolder called '10'
folder_path <- "10"  # Target subfolder
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# List of parameters to check
keywords <- c("gene_accession_id", "gene_symbol", "mouse_strain", "mouse_life_stage", 
              "parameter_id", "pvalue", "parameter_name", "analysis_id")

# Data frame to store files with missing parameters
missing_parameters <- data.frame(
  file_name = character(),  # CSV file name
  missing_parameters = character(),  # Missing parameters
  stringsAsFactors = FALSE
)

# Iterate over each CSV file in the folder
for (csv_file in csv_files) {
  # Read the current CSV file, assuming the first column is A
  data <- read.csv(csv_file, header = FALSE)
  
  # Extract the file name (without the path)
  file_name_value <- basename(csv_file)
  
  # Get the contents of column A from the file
  a_column_values <- data[, 1]
  
  # Find the missing parameters
  missing_keywords <- setdiff(keywords, a_column_values)
  
  if (length(missing_keywords) > 0) {
    # If there are missing parameters, record the file and the missing parameters
    missing_parameters <- rbind(missing_parameters, data.frame(
      file_name = file_name_value,
      missing_parameters = paste(missing_keywords, collapse = ", "),  # Missing parameters separated by commas
      stringsAsFactors = FALSE
    ))
  }
}

# If there are missing parameters, save them to a new CSV file
if (nrow(missing_parameters) > 0) {
  write.csv(missing_parameters, "missing_parameters.csv", row.names = FALSE)
} else {
  print("All files contain all parameters.")
}

# Print the missing parameters (optional)
print(missing_parameters)

```
```{r}
library(tools)
library(stringr)  # For string manipulation (e.g., title case formatting)

# Get the list of CSV files in the target folder
csv_files <- list.files(path = "10", pattern = "*.csv", full.names = TRUE)

# Assuming `final_data` already exists and contains 'parameterId' and 'parameter_name' columns
# final_data <- read.csv("final_data.csv")  # Uncomment if `final_data` needs to be loaded

# Set the list of keywords to check in each CSV file
keywords <- c("gene_accession_id", "gene_symbol", "mouse_strain", "mouse_life_stage", 
              "parameter_id", "pvalue", "parameter_name", "analysis_id")

# Initialize an empty data frame to store exception data
exception_data <- data.frame(
  file_name = character(),  # File name
  variable_name = character(),  # Variable name (e.g., gene_accession_id)
  abnormal_value = character(),  # Abnormal value found
  stringsAsFactors = FALSE
)

# Iterate through each CSV file in the folder
for (csv_file in csv_files) {
  # Read the current CSV file
  data <- read.csv(csv_file, header = FALSE)
  
  # Extract the file name (excluding path)
  file_name_value <- basename(csv_file)
  
  # Initialize a list to store parameters for the current file
  file_parameters <- list()
  
  # Check for each keyword in the CSV file and store the corresponding value
  for (keyword in keywords) {
    # Find the value in column B (second column) corresponding to the keyword in column A (first column)
    matching_value <- data[data[, 1] == keyword, 2]
    
    # If the keyword has a corresponding value, store it in `file_parameters`
    if (length(matching_value) > 0) {
      file_parameters[[keyword]] <- matching_value
    }
    
    # If the value is missing or NA, log the issue
    if (length(matching_value) == 0 || any(is.na(matching_value))) {
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value,
        variable_name = keyword,
        abnormal_value = "Missing or NA",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Clean and check each keyword's value for specific conditions
  
  # Check the length of 'analysis_id' (should be 15 characters)
  if ("analysis_id" %in% names(file_parameters)) {
    analysis_id <- file_parameters[["analysis_id"]]
    if (nchar(analysis_id) != 15) {
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "analysis_id",
        abnormal_value = analysis_id,
        stringsAsFactors = FALSE
      ))
      file_parameters[["analysis_id"]] <- NA  # Set to NA
    }
  }
  
  # Check 'gene_accession_id' length (should be between 9 and 11 characters)
  if ("gene_accession_id" %in% names(file_parameters)) {
    gene_accession_id <- file_parameters[["gene_accession_id"]]
    if (nchar(gene_accession_id) < 9 | nchar(gene_accession_id) > 11) {
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "gene_accession_id",
        abnormal_value = gene_accession_id,
        stringsAsFactors = FALSE
      ))
      file_parameters[["gene_accession_id"]] <- NA  # Set to NA
    }
  }
  
  # Check 'gene_symbol' length (should be between 1 and 13 characters)
  if ("gene_symbol" %in% names(file_parameters)) {
    gene_symbol <- file_parameters[["gene_symbol"]]
    if (nchar(gene_symbol) < 1 | nchar(gene_symbol) > 13) {
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "gene_symbol",
        abnormal_value = gene_symbol,
        stringsAsFactors = FALSE
      ))
      file_parameters[["gene_symbol"]] <- NA  # Set to NA
    } else {
      # If the 'gene_symbol' is valid, convert to title case
      file_parameters[["gene_symbol"]] <- str_to_title(gene_symbol)
    }
  }
  
  # Check if 'mouse_strain' is valid (should be one of the predefined valid strains)
  if ("mouse_strain" %in% names(file_parameters)) {
    mouse_strain <- file_parameters[["mouse_strain"]]
    valid_strains <- c("C57BL", "B6J", "C3H", "129SV")
    if (!(mouse_strain %in% valid_strains)) {
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "mouse_strain",
        abnormal_value = mouse_strain,
        stringsAsFactors = FALSE
      ))
      file_parameters[["mouse_strain"]] <- NA  # Set to NA
    }
  }
  
  # Check if 'mouse_life_stage' is valid (should be one of the predefined stages)
  if ("mouse_life_stage" %in% names(file_parameters)) {
    mouse_life_stage <- file_parameters[["mouse_life_stage"]]
    valid_stages <- c("E12.5", "E15.5", "E18.5", "E9.5", "Early adult", "Late adult", "Middle aged adult")
    if (!(mouse_life_stage %in% valid_stages)) {
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "mouse_life_stage",
        abnormal_value = mouse_life_stage,
        stringsAsFactors = FALSE
      ))
      file_parameters[["mouse_life_stage"]] <- NA  # Set to NA
    }
  }
  
  # Check if 'parameter_id' exists in `final_data` and its corresponding 'parameter_name' matches
  if ("parameter_id" %in% names(file_parameters)) {
    parameter_id <- file_parameters[["parameter_id"]]
    if (!(parameter_id %in% final_data$parameterId)) {
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "parameter_id",
        abnormal_value = parameter_id,
        stringsAsFactors = FALSE
      ))
      file_parameters[["parameter_id"]] <- NA  # Set to NA
    } else {
      # If 'parameter_id' matches, check 'parameter_name' consistency
      matching_row <- final_data[final_data$parameterId == parameter_id, ]
      final_parameter_name <- NA  # Initialize the final parameter name

      # Check for the first valid value in the matched rows
      for (name_value in matching_row$name) {
        if (!is.na(name_value)) {
          final_parameter_name <- name_value
          break  # Exit loop once a valid name is found
        }
      }
      if (final_parameter_name != file_parameters[["parameter_name"]]) {
        exception_data <- rbind(exception_data, data.frame(
          file_name = file_name_value, 
          variable_name = "parameter_name",
          abnormal_value = file_parameters[["parameter_name"]],
          stringsAsFactors = FALSE
        ))
        file_parameters[["parameter_name"]] <- NA  # Set to NA
      }
    }
  }
  
  # Check if 'pvalue' is within the valid range (0 to 1)
  if ("pvalue" %in% names(file_parameters)) {
    pvalue <- file_parameters[["pvalue"]]
    if (pvalue < 0 | pvalue > 1) {
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "pvalue",
        abnormal_value = pvalue,
        stringsAsFactors = FALSE
      ))
      file_parameters[["pvalue"]] <- NA  # Set to NA
    }
  }
}

# Save the exception data to a CSV file
write.csv(exception_data, "exception_data.csv", row.names = FALSE)

# Print the exception data (optional)
print(exception_data)

```
```{r}
# Get the list of all CSV files in the data/10 folder
csv_files <- list.files(path = "data/10", pattern = "*.csv", full.names = TRUE)

# Create an empty data frame to store all extracted data
combined_data <- data.frame()

# Define the keywords to search for (corresponding to column A)
keywords <- c("gene_accession_id", "gene_symbol", "mouse_strain", "mouse_life_stage", 
              "parameter_id", "pvalue", "parameter_name", "analysis_id")

# Iterate through each CSV file and extract the relevant data
for (file in csv_files) {
  
  # Read the CSV file
  sheet_data <- read.csv(file, header = FALSE)  # Ensure no column names since data is in fixed rows
  
  # Create an empty data frame to store the extracted data for the current file
  extracted_data <- data.frame(matrix(NA, ncol = length(keywords), nrow = 1))
  colnames(extracted_data) <- keywords  # Set the column names to the keywords

  # Iterate through each keyword to find the corresponding value in column A
  for (keyword in keywords) {
    
    # Find the row where the keyword is located
    matching_row <- which(grepl(keyword, sheet_data$V1, ignore.case = TRUE))  # V1 is column A
    
    # If a matching row is found, extract the corresponding value from column B
    if (length(matching_row) > 0) {
      b_value <- sheet_data[matching_row, 2]  # Extract the corresponding value from column B
      extracted_data[[keyword]] <- b_value  # Place the value in the corresponding column
    } else {
      # If no matching row is found, set the value as an empty string
      extracted_data[[keyword]] <- ""  # Set as an empty string instead of NA
    }
  }

  # Append the extracted data from the current file to the combined data frame
  combined_data <- bind_rows(combined_data, extracted_data)
}

# Add an Index column to the data frame
combined_data$Index <- 1:nrow(combined_data)

# Write the combined data to a CSV file
write.csv(combined_data, "combined_data.csv", row.names = FALSE)


```
```{r}
# Read the exception data from the CSV file
data <- read.csv('exception_data.csv')

# Sort the data by the 'variable' column
data_sorted <- data[order(data$variable), ]

# Display the first few rows of the sorted data
head(data_sorted)

# Extract data where the 'variable' column is "mouse_life_stage"
mouse_life_stage_data <- subset(data_sorted, variable == "mouse_life_stage")

# Display the extracted data
print(mouse_life_stage_data)


```
```{r}
# Define the list of valid abnormal values
valid_abnormal_values <- c("E12.5", "E15.5", "E18.5", "E9.5", "Early adult", "Late adult", "Middle aged adult")

# Convert the abnormal_value column to lowercase to avoid case sensitivity issues
mouse_life_stage_data$abnormal_value_lower <- tolower(mouse_life_stage_data$abnormal_value)

# Convert valid abnormal values to lowercase
valid_abnormal_values_lower <- tolower(valid_abnormal_values)

# Filter the data to get rows where abnormal_value matches any of the valid values
matched_data <- mouse_life_stage_data[mouse_life_stage_data$abnormal_value_lower %in% valid_abnormal_values_lower, ]

# Filter the data to get rows where abnormal_value does not match any of the valid values
unmatched_data <- mouse_life_stage_data[!mouse_life_stage_data$abnormal_value_lower %in% valid_abnormal_values_lower, ]

# Print the matched data
cat("Matched Data:\n")
print(head(matched_data))

# Print the unmatched data
cat("Unmatched Data:\n")
print(head(unmatched_data))

```
```{r}
# Read combined_data.csv
combined_data <- read.csv('combined_data.csv')

# Check if required columns exist in mouse_life_stage_data and combined_data
if ("file_name" %in% colnames(mouse_life_stage_data) && "analysis_id" %in% colnames(combined_data)) {

  # Define a list of valid mouse_life_stage values
  valid_values <- c("E12.5", "E15.5", "E18.5", "E9.5", "Early adult", "Late adult", "Middle aged adult")

  # Clean up the file names to remove ".csv" and ensure they match case-insensitively
  mouse_life_stage_data$file_name_clean <- tolower(sub("\\.csv$", "", mouse_life_stage_data$file_name))
  combined_data$analysis_id_clean <- tolower(sub("\\.csv$", "", combined_data$analysis_id))

  # Map each file_name_clean in mouse_life_stage_data to the corresponding analysis_id_clean in combined_data
  for (i in 1:nrow(mouse_life_stage_data)) {
    current_file <- mouse_life_stage_data$file_name_clean[i]
    
    # Find matching rows in combined_data
    matched_row_index <- which(combined_data$analysis_id_clean == current_file)

    if (length(matched_row_index) > 0) {
      current_value <- combined_data$mouse_life_stage[matched_row_index]
      
      # If the current value is not in the valid list, correct it
      if (!(current_value %in% valid_values)) {
        # Update the value based on custom rules for specific values
        if (current_value == "early adult") {
          combined_data$mouse_life_stage[matched_row_index] <- "Early adult"
        } else if (current_value == "late adult") {
          combined_data$mouse_life_stage[matched_row_index] <- "Late adult"
        } else if (current_value == "middle aged adult") {
          combined_data$mouse_life_stage[matched_row_index] <- "Middle aged adult"
        } else {
          # Apply title case for other valid values (like E12.5, E9.5, etc.)
          combined_data$mouse_life_stage[matched_row_index] <- tools::toTitleCase(current_value)
        }
      }
    }
  }

  # Save the updated data back to CSV
  write.csv(combined_data, 'combined_data.csv', row.names = FALSE)

  # View the updated data
  head(combined_data)

} else {
  print("Columns 'file_name' or 'analysis_id' are missing in the data")
}


```
```{r}
# Read the original CSV file
combined_data <- read.csv("combined_data.csv")

# Remove the "I" column
combined_data <- combined_data[, !(names(combined_data) == "I")]

# Save the modified data back to the original file
write.csv(combined_data, "combined_data.csv", row.names = FALSE)


```
```{r}
gene_symbol_data <- data_sorted[data_sorted$variable_name == "gene_symbol", ]

# Merge data
# Here, merging means assigning the filtered data to the gene_symbol data frame

# View the filtered data
print(gene_symbol_data)


analysis_id_data <- data_sorted[data_sorted$variable_name == "analysis_id", ]

# Merge data
# Here, merging means assigning the filtered data to the analysis_id data frame

# View the filtered data
print(analysis_id_data)

gene_accsion_id_data <- data_sorted[data_sorted$variable_name == "gene_accsion_id", ]

# Merge data
# Here, merging means assigning the filtered data to the gene_accsion_id data frame

# View the filtered data
print(gene_accsion_id_data)

```
```{r}
pvalue_data <- data_sorted[data_sorted$variable_name == "pvalue", ]

# Merge data
# Here, merging means assigning the filtered data to the pvalue data frame

# View the filtered data
print(pvalue_data)

```
```{r}
# Assume your data frame is pvalue_data

# Convert the abnormal_value column from character type to numeric
pvalue_data$abnormal_value <- as.numeric(pvalue_data$abnormal_value)

# Filter rows where abnormal_value is not between 0 and 1
out_of_range_values <- subset(pvalue_data, abnormal_value <= 0 | abnormal_value >= 1)

# Output the filtered data
print(out_of_range_values)

```
```{r}
# Install and load the dplyr package
library(dplyr)

# Extract the file_name from out_of_range_values and remove the ".csv" suffix
file_names <- gsub("\\.csv$", "", out_of_range_values$file_name)

# Read the original combined_data.csv file
combined_data <- read.csv("combined_data.csv")

# Match the analysis_id column in combined_data with file_names, and delete the matching rows
combined_data <- combined_data %>%
  filter(!analysis_id %in% file_names)

# Save the modified data back to the original file
write.csv(combined_data, "combined_data.csv", row.names = FALSE)

```
```{r}
mouse_strain_data <- data_sorted[data_sorted$variable_name == "mouse_strain", ]

# Merge data
# Here, merging means assigning the filtered data to the mouse_strain data frame

# View the filtered data
print(mouse_strain_data)

```
```{r}
# Assume you have already filtered mouse_strain_data from data_sorted
# Expected values list
expected_values <- c("C57BL", "B6J", "C3H", "129SV")

# Convert the abnormal_value column to lowercase
mouse_strain_data$abnormal_value_lower <- tolower(mouse_strain_data$abnormal_value)

# Filter data that does not match the expected values due to case sensitivity
case_mismatch_data <- mouse_strain_data[mouse_strain_data$abnormal_value_lower %in% tolower(expected_values) & 
                                        !(mouse_strain_data$abnormal_value %in% expected_values), ]

# Filter data that does not match the expected values for other reasons
other_reasons_data <- mouse_strain_data[!(mouse_strain_data$abnormal_value_lower %in% tolower(expected_values)), ]

# Output both datasets
print("Case mismatch data due to case sensitivity:")
print(case_mismatch_data)

print("Other reasons data that does not match expected values:")
print(other_reasons_data)

```
```{r}
# 1. Read the combined_data.csv file
combined_data <- read.csv("combined_data.csv")

# 2. Assume other_reasons_data is already a data frame
# Remove the '.csv' suffix from 'file_name'
other_reasons_data$file_name_without_csv <- gsub("\\.csv$", "", other_reasons_data$file_name)

# 3. Match 'file_name_without_csv' with 'analysis_id' and find the matching rows
# Use %in% to check if 'analysis_id' is present in 'file_name_without_csv'
to_remove <- combined_data$analysis_id %in% other_reasons_data$file_name_without_csv

# 4. Remove the matching rows
combined_data_cleaned <- combined_data[!to_remove, ]

# 5. Save to the original file
write.csv(combined_data_cleaned, "combined_data.csv", row.names = FALSE)


```
```{r}
parameter_id_data <- data_sorted[data_sorted$variable_name == "parameter_id", ]

# Merge data
# Here, merging means assigning the filtered data to the parameter_id data frame

# View the filtered data
print(parameter_id_data)


```
```{r}
# 1. Load the original data
combined_data <- read.csv("combined_data.csv")

# 2. Assume parameter_id_data is already a dataframe
# Remove the '.csv' suffix from the 'file_name' column
parameter_id_data$file_name_without_csv <- gsub("\\.csv$", "", parameter_id_data$file_name)

# 3. Match 'file_name_without_csv' and 'analysis_id', and find the matching rows
to_remove <- combined_data$analysis_id %in% parameter_id_data$file_name_without_csv

# 4. Extract the matching rows and save them to the newtype.csv file
matched_data <- combined_data[to_remove, c("gene_accession_id", "gene_symbol", "mouse_strain", 
                                           "mouse_life_stage", "parameter_id", "pvalue", 
                                           "parameter_name", "analysis_id")]
write.csv(matched_data, "newtype.csv", row.names = FALSE)

# 5. Remove the matching rows and save the cleaned data back to the original file
combined_data_cleaned <- combined_data[!to_remove, ]
write.csv(combined_data_cleaned, "combined_data.csv", row.names = FALSE)


```

```{r}
parameter_name_data <- data_sorted[data_sorted$variable_name == "parameter_name", ]

# Merge data
# Here, merging means assigning the filtered data to the parameter_name data frame

# View the filtered data
print(parameter_name_data)


```
```{r}
# Assume your data frame is parameter_name_data
unique_parameters <- unique(parameter_name_data$abnormal_value)

# View the result
print(unique_parameters)
# After comparing with the reference file, no cleaning is required

```
```{r}
# 1. Read the combined_data.csv file
combined_data <- read.csv("combined_data.csv")

# 2. Find identical rows and remove duplicates, keeping the first occurrence
combined_data_unique <- combined_data[!duplicated(combined_data[c("gene_accession_id", 
                                                                  "gene_symbol", 
                                                                  "mouse_strain", 
                                                                  "mouse_life_stage", 
                                                                  "parameter_id", 
                                                                  "pvalue", 
                                                                  "parameter_name")]), ]

# 3. View the deduplicated data
head(combined_data_unique)

# 4. Save the deduplicated data back to the original file
write.csv(combined_data_unique, "combined_data.csv", row.names = FALSE)


```
