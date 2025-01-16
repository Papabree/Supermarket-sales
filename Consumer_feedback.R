supermarket_sales <- read.csv("C:/Users/mcdon/Desktop/Data and path/supermarket_sales.csv")
attach(supermarket_sales)
View(supermarket_sales)
#Libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(rsconnect)

# Load the dataset
df <- supermarket_sales





# Define UI
ui <- fluidPage(
  titlePanel("Sales Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "factor",
        label = "Select a Factor:",
        choices = c("Branch", "City", "Customer.type", "Gender", "Product.line", "Payment"),
        selected = "Branch"
      )
    ),
    mainPanel(
      plotlyOutput("lineGraph", height = "400px"),
      fluidRow(
        column(6, plotlyOutput("ratingGraph", height = "400px")),
        column(6, plotlyOutput("grossIncomeGraph", height = "400px"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  # Scatter Plot for Rating vs Gross Income
  output$lineGraph <- renderPlotly({
    data <- df  # Use the original data for this scatter plot
    
    plot_ly(data, x = ~Rating, y = ~gross.income, type = "scatter", mode = "markers",
            color = ~as.factor(.data[[input$factor]]), marker = list(size = 10)) %>%
      layout(
        title = "Scatter Plot: Rating vs Gross Income by Selected Factor",
        xaxis = list(title = "Rating"),
        yaxis = list(title = "Gross Income")
      )
  })
  
  # Rating by Selected Factor
  output$ratingGraph <- renderPlotly({
    df %>%
      group_by(.data[[input$factor]]) %>%
      summarise(Mean_Rating = mean(Rating)) %>%
      plot_ly(x = ~.data[[input$factor]], y = ~Mean_Rating, type = "bar",
              color = ~.data[[input$factor]]) %>%
      layout(
        title = "Average Rating by Selected Factor",
        xaxis = list(title = input$factor),
        yaxis = list(title = "Average Rating")
      )
  })
  
  # Gross Income by Selected Factor
  output$grossIncomeGraph <- renderPlotly({
    df %>%
      group_by(.data[[input$factor]]) %>%
      summarise(Total_Income = sum(gross.income)) %>%
      plot_ly(x = ~.data[[input$factor]], y = ~Total_Income, type = "bar",
              color = ~.data[[input$factor]]) %>%
      layout(
        title = "Total Gross Income by Selected Factor",
        xaxis = list(title = input$factor),
        yaxis = list(title = "Total Gross Income")
      )
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)