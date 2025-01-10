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

