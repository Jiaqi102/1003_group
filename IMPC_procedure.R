setwd('D:/1003_data/Group10/Group10')
getwd()

```

```{r}
# Read the file 
data <- read.table("IMPC_procedure.txt", header = FALSE, stringsAsFactors = FALSE)

# Extract the valid data portion.
data <- data$V2  # Suppose the data is in the first column.

# Continue to operate according to the above logic.
data <- data[-1]  # Remove the table header.
split_data <- strsplit(data, ",\\s*")

result <- data.frame(
  procedureId = sapply(split_data, function(x) x[1]),
  impcParameterOrigId = sapply(split_data, function(x) x[length(x)]),
  stringsAsFactors = FALSE
)

head(result)
```
```{r}
write.csv(result, file = "IMPC_procedure.csv", row.names = FALSE)
```

```{r}
install.packages(c("readr", "dplyr"))
library(readr)
library(dplyr)
```
```{r}
# 1. Read the file as a single column
data <- readLines("Disease_information.txt")

# 2. Remove the first two rows (based on the description you provided, if there are column headers or other unnecessary content)
data_clean <- data[-c(1, 2)]

# 3. Check and remove any empty lines
data_clean <- data_clean[data_clean != ""]

# 4. Split the content of each line using a regular expression
# For each line, use a regular expression to separate the data fields
data_split <- strsplit(data_clean, '" "', perl = TRUE)

# 5. Further process the split data: remove quotes and split by comma
data_split <- lapply(data_split, function(x) {
  # Remove the surrounding quotes
  x <- gsub('"', '', x)
  # Split by commas
  strsplit(x, ",\\s*")
})

# 6. Flatten the split results
result_lapply <- lapply(data_split, function(x) {
  unlist(x)
})

# 7. Ensure each character vector is transposed into a row
result_as_df <- lapply(result_lapply, function(x) {
  data.frame(t(x))  # Transpose each vector so it becomes a row
})

# 8. Use bind_rows to combine the data, binding by rows
library(dplyr)
data_split_df <- bind_rows(result_as_df)

# 9. Remove any leading/trailing white spaces
data_split_df <- apply(data_split_df, 2, function(x) trimws(x))  

# 10. Convert the data into a data frame and name each column
final_data <- as.data.frame(data_split_df, stringsAsFactors = FALSE)

# 11. Name the columns of the data frame
colnames(final_data) <- c("number", "disease_id", "disease_term", "gene_accession_id", "phenodigm_score")

# 12. View the result
head(final_data)

# 13. If needed, export to CSV file
write.csv(final_data, "Disease_information_output.csv", row.names = FALSE)

```
```{r}
# Read the Disease_information_output.csv file
disease_info <- read.csv("Disease_information_output.csv")

# Create an empty data frame to store the results
disease_result <- data.frame()

# Loop through columns D to K to extract MGI data and the corresponding phenodigm_score
for (i in 4:11) {
  # Find rows containing data starting with "MGI:" and extract the corresponding data from the next column
  mgi_rows <- grep("^MGI:", disease_info[, i])
  
  # If MGI data is found, extract the corresponding phenodigm_score
  for (row in mgi_rows) {
    mgi_value <- disease_info[row, i]  # Extract MGI value
    phenodigm_score <- disease_info[row, i + 1]  # Extract the corresponding phenodigm_score (next column)
    
    # Store the data in the disease_result data frame
    disease_result <- rbind(disease_result, data.frame(disease_id = disease_info[row, 2], 
                                                      gene_accession_id = mgi_value, 
                                                      phenodigm_score = phenodigm_score))
  }
}

# Write the result into a CSV file
write.csv(disease_result, "Disease.csv", row.names = FALSE)

```
