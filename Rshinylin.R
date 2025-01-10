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

# Read data
data <- read_csv("combined_data.csv")

# Define UI layout
ui <- fluidPage(
  titlePanel("Gene Symbol Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("gene_symbol", "Enter Gene Symbol:", value = "BRCA1"),
      numericInput("n", "Select number of red bars (pvalue < 0.05):", value = 5, min = 1),
      numericInput("a", "Select number of blue bars (pvalue >= 0.05):", value = 5, min = 1),
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      plotOutput("barplot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Define reactive data processing
  observeEvent(input$submit, {
    gene_symbol_input <- input$gene_symbol
    n <- input$n
    a <- input$a
    
    # Filter data based on the input gene_symbol
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
    
    # Render the bar plot
    output$barplot <- renderPlot({
      ggplot(selected_data, aes(x = parameter_name, y = log_pvalue, fill = pvalue < 0.05)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("red", "blue")) +
        labs(x = "Parameter Name", y = "-log10(p-value)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
```
```{r}
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Read data
data <- read_csv("combined_data.csv")

# Define UI layout
ui <- fluidPage(
  titlePanel("Gene Symbol Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("gene_symbol", "Enter Gene Symbol:", value = "BRCA1"),
      numericInput("n", "Select number of red bars (pvalue < 0.05):", value = 5, min = 1),
      numericInput("a", "Select number of blue bars (pvalue >= 0.05):", value = 5, min = 1),
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      plotOutput("barplot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Define reactive data processing
  observeEvent(input$submit, {
    gene_symbol_input <- input$gene_symbol
    n <- input$n
    a <- input$a
    
    # Filter data based on the input gene_symbol
    filtered_data <- data %>%
      filter(gene_symbol %in% strsplit(gene_symbol_input, ",")[[1]]) %>%
      mutate(log_pvalue = -log10(pvalue))
    
    # Group data by pvalue
    group1 <- filtered_data %>% filter(pvalue < 0.05)
    group2 <- filtered_data %>% filter(pvalue >= 0.05)
    
    # Select top n and a values from group1 and group2 respectively
    selected_group1 <- group1 %>% top_n(n, wt = pvalue)
    selected_group2 <- group2 %>% top_n(a, wt = -pvalue)
    
    # Combine selected data
    selected_data <- bind_rows(selected_group1, selected_group2)
    
    # Render bar plot
    output$barplot <- renderPlot({
      ggplot(selected_data, aes(x = parameter_name, y = log_pvalue, fill = pvalue < 0.05)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("red", "blue")) +
        labs(x = "Parameter Name", y = "-log10(p-value)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

```
