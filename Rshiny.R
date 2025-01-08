# Load necessary R packages
library(shiny)
library(ggplot2)
library(reshape2)
library(dendextend)

# Set path for data to be read(change accordingly)
csv_file_path <- "C:/Users/10600/Desktop/database.csv"
disease_file_path <- "C:/Users/10600/Desktop/Group10/Group10/Disease_information.txt"

# Read mainly used dataset
data <- read.csv(csv_file_path)

# Read Disease_information.txt line by line using readLines
lines <- readLines(disease_file_path)

# Extract column names
col_names <- strsplit(gsub('"', '', lines[2]), ", ")[[1]]

# Extract data
data_lines <- lines[3:length(lines)]

# Clean data lines
data_lines_clean <- gsub('^"|"$', '', data_lines)  # Remove the double quotes at the beginning and end of the sentence
data_lines_clean <- gsub(',\\s+', ',', data_lines_clean)  # Remove redundant spaces
data_lines_clean <- gsub('([a-zA-Z0-9]), ([a-zA-Z])', '\\1|\\2', data_lines_clean)  # Replace redundant commas

# Forcefully set column names and read the cleaned data
disease_data <- read.table(
  text = data_lines_clean,
  sep = ",",
  col.names = c("disease_id", "disease_term", "gene_accession_id", "phenodigm_score"),
  fill = TRUE,  # Automatically fill in missing values
  stringsAsFactors = FALSE
)

# Repair commas in the field
disease_data$disease_term <- gsub('|', ', ', disease_data$disease_term, fixed = TRUE)

# Check the reading results
print(head(disease_data))

# Merge Disease_information data into the main dataset
analysis_results <- merge(
  data,
  disease_data[, c("gene_accession_id", "phenodigm_score")],
  by = "gene_accession_id",
  all.x = TRUE
)

# Ensure the column types are correct.
analysis_results$pvalue <- as.numeric(analysis_results$pvalue)
analysis_results$phenodigm_score <- as.numeric(analysis_results$phenodigm_score)

# Replace NA values
analysis_results[is.na(analysis_results)] <- 0

# Check the merge results
print(head(analysis_results))

# Split the data into genes, phenotypes and analysis results
gene_info <- unique(analysis_results[c("gene_accession_id", "gene_symbol")])
parameter_info <- unique(analysis_results[c("parameter_id", "parameter_name")])

# Define UI part
ui <- fluidPage(
  titlePanel("IMPC data visualization dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("view_type", "Select View Type:", 
                  choices = c("View phenotypes by gene" = "gene_view",
                              "View genes by phenotype" = "phenotype_view",
                              "Gene clustering analysis" = "cluster_view")),
      
      uiOutput("dynamic_ui")  # Dynamically generate input box
    ),
    
    mainPanel(
      plotOutput("main_plot"),
      tableOutput("data_table")  # Display the original data table
    )
  )
)

# Define the Server section
server <- function(input, output, session) {
  
  # Dynamically generate input box
  output$dynamic_ui <- renderUI({
    if (input$view_type == "gene_view") {
      selectInput("gene", "Select gene symbols:", 
                  choices = unique(gene_info$gene_symbol))
    } else if (input$view_type == "phenotype_view") {
      selectInput("phenotype", "Select phenotypic parameters:", 
                  choices = unique(parameter_info$parameter_name))
    } else if (input$view_type == "cluster_view") {
      sliderInput("cluster_number", "Select the number of clusters:", min = 2, max = 10, value = 3)
    }
  })
  
  # Main image generation
  output$main_plot <- renderPlot({
    req(input$view_type)
    
    if (input$view_type == "gene_view") {
      filtered_data <- merge(analysis_results, gene_info[gene_info$gene_symbol == input$gene, ], by = "gene_accession_id")
      
      ggplot(filtered_data, aes(x = parameter_id, y = -log10(pvalue))) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Phenotypic statistical significance (gene:", input$gene, ")"), 
             x = "phenotypic parameter", y = "-log10(p value)")
      
    } else if (input$view_type == "phenotype_view") {
      filtered_data <- merge(analysis_results, parameter_info[parameter_info$parameter_name == input$phenotype, ], by = "parameter_id")
      
      ggplot(filtered_data, aes(x = gene_accession_id, y = -log10(pvalue))) +
        geom_bar(stat = "identity", fill = "coral") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Genetic statistical significance（phenotype:", input$phenotype, "）"), 
             x = "Gene symbol", y = "-log10(p value)")
      
    } else if (input$view_type == "cluster_view") {
      # Convert the data into a matrix, including pvalue and phenodigm_score
      matrix_data <- dcast(
        analysis_results, 
        gene_accession_id ~ parameter_id, 
        value.var = "pvalue", 
        fun.aggregate = mean,  # Specified Aggregate Function
        fill = 0  # Replace missing values with 0
      )
      rownames(matrix_data) <- matrix_data$gene_accession_id
      matrix_data <- matrix_data[,-1]  # Remove the first column (gene symbols)
      
      # Add phenodigm_score column
      phenodigm_scores <- analysis_results[, c("gene_accession_id", "phenodigm_score")]
      phenodigm_scores <- phenodigm_scores[!duplicated(phenodigm_scores$gene_accession_id), ]
      rownames(phenodigm_scores) <- phenodigm_scores$gene_accession_id
      phenodigm_scores <- phenodigm_scores$phenodigm_score[rownames(matrix_data)]
      matrix_data <- cbind(matrix_data, phenodigm_score = phenodigm_scores)
      
      # Convert to numerical type and handle missing values
      matrix_data <- apply(matrix_data, 2, as.numeric)
      matrix_data[is.na(matrix_data)] <- 0  # Replace NA with 0
      
      # Calculate the distance matrix and hierarchical clustering
      dist_matrix <- dist(matrix_data, method = "euclidean")
      hc <- hclust(dist_matrix, method = "ward.D2")
      
      # Draw a tree diagram
      dend <- as.dendrogram(hc)
      dend <- color_branches(dend, k = input$cluster_number)
      plot(dend, main = "Gene clustering analysis", xlab = "Gene symbol", ylab = "Distance")
    }
  })
  
  # Data table output
  output$data_table <- renderTable({
    req(input$view_type)
    
    if (input$view_type == "gene_view") {
      filtered_data <- merge(analysis_results, gene_info[gene_info$gene_symbol == input$gene, ], by = "gene_accession_id")
      filtered_data[, c("parameter_id", "pvalue")]
      
    } else if (input$view_type == "phenotype_view") {
      filtered_data <- merge(analysis_results, parameter_info[parameter_info$parameter_name == input$phenotype, ], by = "parameter_id")
      filtered_data[, c("gene_accession_id", "pvalue")]
    }
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
