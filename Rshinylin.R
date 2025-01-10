```{r}
# Install necessary R packages
install.packages("shiny")        # The Shiny package is used for building interactive web applications
install.packages("ggplot2")      # The ggplot2 package is used for data visualization
install.packages("reshape2")     # The reshape2 package is used for data transformation (reshape)
install.packages("dendextend")   # The dendextend package is used for visualizing hierarchical clustering
```
```{r}
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Read the data
data <- read_csv("combined_data.csv")

# Define the UI interface
ui <- fluidPage(
  titlePanel("Gene and Phenotype Analysis"),
  
  # New UI section for selecting view type
  sidebarLayout(
    sidebarPanel(
      radioButtons("view_type", "Select View Type:",
                   choices = c("View phenotype by genes", "View genes by phenotype"),
                   selected = "View phenotype by genes"),
      
      # Conditional UI for "View phenotype by genes"
      conditionalPanel(
        condition = "input.view_type == 'View phenotype by genes'",
        textInput("gene_symbol", "Enter Gene Symbol:", value = "BRCA1"),
        numericInput("n", "Select number (pvalue < 0.05):", value = 5, min = 1),
        numericInput("a", "Select number (pvalue >= 0.05):", value = 5, min = 1),
        actionButton("submit1", "Submit")
      ),
      
      # Conditional UI for "View genes by phenotype"
      conditionalPanel(
        condition = "input.view_type == 'View genes by phenotype'",
        textInput("parameter_name", "Enter Parameter Name:", value = "parameter1"),
        numericInput("n2", "Select number (pvalue < 0.05):", value = 5, min = 1),
        numericInput("a2", "Select number (pvalue >= 0.05):", value = 5, min = 1),
        actionButton("submit2", "Submit")
      )
    ),
    
    mainPanel(
      plotOutput("barplot")
    )
  )
)

# Define the Server logic
server <- function(input, output) {
  
  # Define reactive data processing for "View phenotype by genes"
  observeEvent(input$submit1, {
    gene_symbol_input <- input$gene_symbol
    n <- input$n
    a <- input$a
    
    # Filter the data based on the input gene_symbol
    filtered_data <- data %>%
      filter(gene_symbol %in% strsplit(gene_symbol_input, ",")[[1]]) %>%
      mutate(log_pvalue = -log10(pvalue))
    
    # Group the data by pvalue
    group1 <- filtered_data %>% filter(pvalue < 0.05)
    group2 <- filtered_data %>% filter(pvalue >= 0.05)
    
    # Select top n and a values from group1 and group2 respectively
    selected_group1 <- group1 %>% top_n(n, wt = pvalue)
    selected_group2 <- group2 %>% top_n(a, wt = -pvalue)
    
    # Combine the selected data
    selected_data <- bind_rows(selected_group1, selected_group2)
    
    # Render the bar plot for "View phenotype by genes"
    output$barplot <- renderPlot({
      ggplot(selected_data, aes(x = parameter_name, y = log_pvalue, fill = pvalue < 0.05)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("red", "blue")) +
        labs(x = "Parameter Name", y = "-log10(p-value)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
  
  # Define reactive data processing for "View genes by phenotype"
  observeEvent(input$submit2, {
    parameter_name_input <- input$parameter_name
    n2 <- input$n2
    a2 <- input$a2
    
    # Filter the data to match the input parameter_name
    filtered_data <- data %>%
      filter(parameter_name == parameter_name_input) %>%
      mutate(log_pvalue = -log10(pvalue))
    
    # Group the data based on pvalue
    group1 <- filtered_data %>% filter(pvalue < 0.05)
    group2 <- filtered_data %>% filter(pvalue >= 0.05)
    
    # Select n2 and a2 values from group1 and group2 respectively
    selected_group1 <- group1 %>% top_n(n2, wt = pvalue)
    selected_group2 <- group2 %>% top_n(a2, wt = -pvalue)
    
    # Combine the selected data
    selected_data <- bind_rows(selected_group1, selected_group2)
    
    # Render the bar plot for "View genes by phenotype"
    output$barplot <- renderPlot({
      ggplot(selected_data, aes(x = gene_symbol, y = log_pvalue, fill = pvalue < 0.05)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("red", "blue")) +
        labs(x = "Gene Symbol", y = "-log10(p-value)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

```


