# Install and load necessary R packages
install.packages(c("readr", "dplyr"))
library(readr)
library(dplyr)

# Step 1: Read the file as a single column (lines of text)
data <- readLines("IMPC_parameter_description.txt")

# Step 2: Remove the first two rows, as they contain column headers and unnecessary info
data_clean <- data[-c(1, 2)]

# Step 3: Remove any empty rows from the dataset
data_clean <- data_clean[data_clean != ""]

# Step 4: Use regular expressions to split the content of each line.
# Split each line using double quotes and space between fields
data_split <- strsplit(data_clean, '" "', perl = TRUE)

# Step 5: Further processing the split data: remove quotation marks and split by commas
data_split <- lapply(data_split, function(x) {
  # Remove leading and trailing quotation marks
  x <- gsub('"', '', x)
  # Split by commas and remove any extra spaces
  strsplit(x, ",\\s*")
})

# Step 6: Flatten each list element to a vector
result_lapply <- lapply(data_split, function(x) {
  unlist(x)
})

# Step 7: Convert the data into a dataframe by transposing each vector into a row
result_as_df <- lapply(result_lapply, function(x) {
  data.frame(t(x))  # Transpose each vector to become a row
})

# Step 8: Bind all rows together into a single dataframe
data_split_df <- bind_rows(result_as_df)

# Step 9: Trim white spaces from each column in the dataframe
data_split_df <- apply(data_split_df, 2, function(x) trimws(x))

# Step 10: Convert the data into a dataframe and set column names
final_data <- as.data.frame(data_split_df, stringsAsFactors = FALSE)
colnames(final_data) <- c("line_number", "impcParameterOrigId", "name", "description", "parameterId")

# Step 11: Check if there are any missing data in the parameterId column
empty_rows <- final_data[is.na(final_data$parameterId), ]

# Step 12: Filter out rows with missing 'parameterId'
final_data <- final_data[!(final_data$parameterId %in% empty_rows$parameterId), ]

# Step 13: Save the filtered data into a CSV file for further analysis
write.csv(final_data, "final_data.csv", row.names = FALSE)
