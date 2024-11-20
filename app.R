# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(forecast)
library(readxl)

# Load the dataset
weather_data <- read_xlsx("C:\\Users\\PC\\Desktop\\PortoAlegre.xlsx", skip = 6)
colnames(weather_data) <- c("Date", "Cloudiness", "Rainfall", "Max_Temperature", 
                            "Min_Temperature", "Humidity", "Mean_Temperature")

# Convert Date column to Date type
weather_data$Date <- as.Date(weather_data$Date)

# Define UI for application
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {font-family: Arial, sans-serif; background-color: #f4f4f9; color: #333;}
      .title {text-align: center; padding: 20px; color: #4a4a4a;}
      .sidebar {background-color: #e9ecef; padding: 15px; border-radius: 5px;}
      .main-panel {background-color: #ffffff; padding: 15px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.1);}
      .plot {margin-top: 20px;}
    "))
  ),
  
  titlePanel("Porto Alegre Weather Data"),
  
  tabsetPanel(
    tabPanel("Data Visualization",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar",
                 selectInput("variable", "Select a Variable:", 
                             choices = colnames(weather_data)[-1]),
                 dateRangeInput("dateRange", "Select Date Range:",
                                start = min(weather_data$Date),
                                end = max(weather_data$Date)),
                 selectInput("plotType", "Select Plot Type:", 
                             choices = c("Line Plot", "Histogram", "Box Plot", "Scatter Plot"))
               ),
               
               mainPanel(
                 class = "main-panel",
                 plotOutput("weatherPlot"),
                 tableOutput("summaryTable"),
                 plotOutput("acfPlot"),
                 plotOutput("pacfPlot")
               )
             )
    ),
    
    tabPanel("Time Series Modeling",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar",
                 selectInput("tsVariable", "Select a Variable for Time Series:", 
                             choices = colnames(weather_data)[-1]),
                 numericInput("forecastPeriod", "Forecast Period (months):", 12, min = 1),
                 numericInput("p", "AR Order (p):", 1, min = 0),
                 numericInput("d", "Differencing Order (d):", 0, min = 0),
                 numericInput("q", "MA Order (q):", 1, min = 0),
                 numericInput("P", "Seasonal AR Order (P):", 1, min = 0),
                 numericInput("D", "Seasonal Differencing Order (D):", 0, min = 0),
                 numericInput("Q", "Seasonal MA Order (Q):", 1, min = 0),
                 numericInput("S", "Seasonal Period (S):", 12, min = 1)
               ),
               
               mainPanel(
                 class = "main-panel",
                 plotOutput("tsPlot"),
                 verbatimTextOutput("modelSummary"),
                 verbatimTextOutput("forecastStats")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to filter data based on user input
  filtered_data <- reactive({
    weather_data %>%
      filter(Date >= input$dateRange[1],
             Date <= input$dateRange[2])
  })
  
  # Generate plot based on filtered data and selected plot type
  output$weatherPlot <- renderPlot({
    data <- filtered_data()
    variable <- input$variable
    
    if (input$plotType == "Line Plot") {
      ggplot(data, aes_string(x = "Date", y = variable)) +
        geom_line(color = "#007bff") +
        labs(title = paste("Line Plot of", variable),
             x = "Date", y = variable)
    } else if (input$plotType == "Histogram") {
      ggplot(data, aes_string(x = variable)) +
        geom_histogram(binwidth = 1, fill = "#007bff", color = "black") +
        labs(title = paste("Histogram of", variable),
             x = variable, y = "Frequency")
    } else if (input$plotType == "Box Plot") {
      ggplot(data, aes_string(y = variable)) +
        geom_boxplot(fill = "#ff7f0e") +
        labs(title = paste("Box Plot of", variable),
             y = variable)
    } else if (input$plotType == "Scatter Plot") {
      ggplot(data, aes_string(x = "Date", y = variable)) +
        geom_point(color = "#2ca02c") +
        labs(title = paste("Scatter Plot of", variable),
             x = "Date", y = variable)
    }
  })
  
  output$summaryTable <- renderTable({
    filtered_data() %>%
      summarise(Mean = mean(get(input$variable), na.rm = TRUE),
                Min = min(get(input$variable), na.rm = TRUE),
                Max = max(get(input$variable), na.rm = TRUE))
  })
  
  # ACF plot
  output$acfPlot <- renderPlot({
    data <- filtered_data()
    variable <- input$variable
    ts_data <- ts(data[[variable]], frequency = 12)
    Acf(ts_data, main = paste("ACF for", variable))
  })
  
  # PACF plot
  output$pacfPlot <- renderPlot({
    data <- filtered_data()
    variable <- input$variable
    ts_data <- ts(data[[variable]], frequency = 12)
    Pacf(ts_data, main = paste("PACF for", variable))
  })
  
  # Time series modeling with SARIMA
  output$tsPlot <- renderPlot({
    ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q), 
                 seasonal = c(input$P, input$D, input$Q))
    forecast_data <- forecast(fit, h = input$forecastPeriod)
    
    # Plot observed data
    plot(ts_data, main = paste("SARIMA Forecast for", input$tsVariable), xlab = "Time", ylab = input$tsVariable, col = "blue")
    # Add forecast data
    lines(forecast_data$mean, col = "red")
    # Add confidence intervals
    lines(forecast_data$lower[,2], col = "red", lty = 2)
    lines(forecast_data$upper[,2], col = "red", lty = 2)
    legend("topright", legend = c("Observed", "Forecast", "95% CI"), col = c("blue", "red", "red"), lty = c(1, 1, 2))
  })
  
  output$modelSummary <- renderPrint({
    ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q), 
                 seasonal = c(input$P, input$D, input$Q))
    summary(fit)
  })
  
  # Calculate forecast statistics
  output$forecastStats <- renderPrint({
    ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q), 
                 seasonal = c(input$P, input$D, input$Q))
    forecast_data <- forecast(fit, h = input$forecastPeriod)
    
    # Calculate MSE and MAE
    actuals <- window(ts_data, start = end(ts_data) - input$forecastPeriod + 1)
    predictions <- forecast_data$mean[1:length(actuals)]
    
    mse <- mean((actuals - predictions)^2, na.rm = TRUE)
    mae <- mean(abs(actuals - predictions), na.rm = TRUE)
    
    cat("Forecast Statistics:\n")
    cat("Mean Squared Error (MSE):", mse, "\n")
    cat("Mean Absolute Error (MAE):", mae, "\n")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)