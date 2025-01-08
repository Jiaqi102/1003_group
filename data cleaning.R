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
