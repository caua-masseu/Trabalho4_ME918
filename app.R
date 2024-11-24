# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(forecast)
library(readxl)
library(DT)
library(openxlsx)

# Define UI for application
ui <- fluidPage(
  tags$head(tags$style(HTML("
body {
font-family: 'Arial', sans-serif;
background-color: #a7dfe9;
color: #333;
margin: 0;
padding: 0;
border-radius: 20px;
}
.title {
text-align: center;
padding: 15px;
color: #2c3e50;
background-color: #ecf0f1;
border-bottom: solid #bdc3c7;
border-radius: 20px;
margin: 5px 7px 5px 7px;
}
.sidebar {
background-color: #ecf0f1;
padding: 10px;
}
.main-panel {
background-color: #ffffff;
padding: 20px;
border-radius: 5px;
box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
margin: 5px 5px 5px 5px;
}
.plot {
margin-top: 20px;
}
.btn {
margin-top: 10px;
}
.shiny-input-container {
margin-bottom: 15px;
}
"))
  ),
  
  tags$div(
    class = "title",
    titlePanel("Time Series Data Analysis")
  ),
  
  tabsetPanel(
    tabPanel(title =  "Data Visualization",
             class = "title",
             fluidRow(class = "sidebar",
                      column(width = 4,
                             fileInput("fileUpload", "Upload a File", accept = c(".csv", ".xlsx")),
                             selectInput("variable", "Select a Variable:", choices = NULL),
                             checkboxInput("applyLog", "Apply Logarithm", value = FALSE),
                             numericInput("numDiff", "Number of Differences:", value = 0, min = 0)
                      ),
                      column(width = 4,
                             dateRangeInput("dateRange", "Select Date Range:", start = NULL, end = NULL)
                      ),
                      column(width = 4,
                             selectInput("plotType", "Select Plot Type:", choices = c("Line Plot", "Histogram", "Box Plot"))
                      )
             ),
             
             fluidRow(
               class = "main-panel",
               plotOutput("weatherPlot"),
               tableOutput("summaryTable"),
               column(width = 6, plotOutput("acfPlot")),
               column(width = 6, plotOutput("pacfPlot")),
               column(width = 12,
                      downloadButton("downloadWeatherPlot", "Download Weather Plot", class = "btn"),
                      downloadButton("downloadACFPlot", "Download ACF Plot", class = "btn"),
                      downloadButton("downloadPACFPlot", "Download PACF Plot", class = "btn")
               )
             ),
             fluidRow(class = "main-panel",
                      DTOutput("dadosTable"),
                      column(width = 12,
                             downloadButton("downloadCSV", "Download CSV", class = "btn"),
                             downloadButton("downloadExcel", "Download Excel", class = "btn")
                      )
             )
    ),
    
    tabPanel(title = "Model Adjustment",
             column(width = 3,
                    class = "sidebar",
                    selectInput("tsVariable", "Select a Variable:", choices = NULL),
                    checkboxInput("applyLogModel", "Apply Logarithm", value = FALSE),
                    numericInput("forecastPeriod", "Forecast Period (months):", 12, min = 1),
                    numericInput("p", "AR Order (p):", 1, min = 0),
                    numericInput("d", "Differencing Order (d):", 0, min = 0),
                    numericInput("q", "MA Order (q):", 1, min = 0),
                    numericInput("P", "Seasonal AR Order (P):", 1, min = 0),
                    numericInput("D", "Seasonal Differencing Order (D):", 0, min = 0),
                    numericInput("Q", "Seasonal MA Order (Q):", 1, min = 0),
                    numericInput("S", "Seasonal Period (S):", 12, min = 1)
             ),
             column(width = 9,
                    class = "main-panel",
                    plotOutput("tsPlot"),
                    verbatimTextOutput("modelSummary"),
                    verbatimTextOutput("forecastStats")
             )
    ),
    
    tabPanel(title = "Diagnostics",
             fluidRow(
               column(6, plotOutput("residualsPlot")),
               column(6, plotOutput("qqPlot")),
               column(6, plotOutput("acf_res")),
               column(6, plotOutput("hist_res")),
               verbatimTextOutput("residualsSummary")
             ),
             fluidRow(downloadButton("downloadResiduals", "Download Residuals Plot", class = "btn"),
                      downloadButton("downloadQQPlot", "Download Q-Q Plot", class = "btn"),
                      downloadButton("downloadACFRes", "Download ACF Residuals Plot", class = "btn"),
                      downloadButton("downloadHistRes", "Download Histogram Residuals Plot", class = "btn")
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store the dataset
  dataset <- reactiveVal(NULL)
  
  # Load initial dataset
  observe({
    initial_data <- read_xlsx("C:\\Users\\PC\\Desktop\\PortoAlegre.xlsx", skip = 6)
    colnames(initial_data) <- c("Date", "Cloudiness", "Rainfall", "Max_Temperature",
                                "Min_Temperature", "Humidity", "Mean_Temperature")
    initial_data$Date <- as.Date(initial_data$Date)
    dataset(initial_data)
    
    # Update UI elements based on the dataset
    updateSelectInput(session, "variable", choices = colnames(initial_data)[-1])
    updateSelectInput(session, "tsVariable", choices = colnames(initial_data)[-1])
    updateDateRangeInput(session, "dateRange", start = min(initial_data$Date), end = max(initial_data$Date))
  })
  
  # Handle file upload
  observeEvent(input$fileUpload, {
    inFile <- input$fileUpload
    if (is.null(inFile)) return(NULL)
    
    # Determine file type and read accordingly
    if (grepl("\\.csv$", inFile$name)) {
      uploaded_data <- read.csv(inFile$datapath)
    } else if (grepl("\\.xlsx$", inFile$name)) {
      uploaded_data <- read_xlsx(inFile$datapath)
    } else {
      return(NULL)
    }
    
    # Assume the first column is the date
    uploaded_data[[1]] <- as.Date(uploaded_data[[1]])
    dataset(uploaded_data)
    
    # Update UI elements based on the new dataset
    updateSelectInput(session, "variable", choices = colnames(uploaded_data)[-1])
    updateSelectInput(session, "tsVariable", choices = colnames(uploaded_data)[-1])
    updateDateRangeInput(session, "dateRange", start = min(uploaded_data[[1]]), end = max(uploaded_data[[1]]))
  })
  
  # Reactive expression to filter and transform data based on user input
  transformed_data <- reactive({
    req(dataset(), input$variable)
    data <- dataset() %>%
      filter(dataset()[[1]] >= input$dateRange[1],
             dataset()[[1]] <= input$dateRange[2])
    
    variable_data <- data[[input$variable]]
    
    # Apply logarithm if selected
    if (input$applyLog) {
      variable_data <- log(variable_data)
    }
    
    # Apply differencing if specified
    if (input$numDiff > 0) {
      variable_data <- diff(variable_data, differences = input$numDiff)
    }
    
    data[[input$variable]] <- variable_data
    data
  })
  
  # Generate plot based on transformed data and selected plot type
  output$weatherPlot <- renderPlot({
    validate(
      need(input$variable != "", "Please select a variable."),
      need(nrow(transformed_data()) > 0, "No data available for the selected date range.")
    )
    
    data <- transformed_data()
    variable <- input$variable
    
    if (input$plotType == "Line Plot") {
      ggplot(data, aes_string(x = colnames(data)[1], y = variable)) +
        geom_line(color = "#007bff") +
        labs(x = "Date", y = variable) + theme_minimal()
    } else if (input$plotType == "Histogram") {
      ggplot(data, aes_string(x = variable)) +
        geom_histogram(binwidth = 1, fill = "#007bff", color = "black") +
        labs(x = variable, y = "Frequency")  + theme_minimal()
    } else if (input$plotType == "Box Plot") {
      ggplot(data, aes_string(x = variable)) +
        geom_boxplot(fill = "#ff7f0e") + labs(x = variable) + theme_minimal()
    }
  })
  
  output$summaryTable <- renderPrint({
    req(input$variable)
    summary(transformed_data()[[input$variable]])
  })
  
  # ACF plot
  output$acfPlot <- renderPlot({
    data <- transformed_data()
    variable <- input$variable
    ts_data <- ts(data[[variable]], frequency = 12)
    acf(ts_data, main = paste("ACF for", variable))
  })
  
  # PACF plot
  output$pacfPlot <- renderPlot({
    data <- transformed_data()
    variable <- input$variable
    ts_data <- ts(data[[variable]], frequency = 12)
    pacf(ts_data, main = paste("PACF for", variable))
  })
  
  output$dadosTable <- renderDT({
    datatable(
      dataset(),
      options = list(
        pageLength = 10,  # Default number of rows per page
        lengthMenu = c(10, 25, 50, 100, nrow(dataset())), # Options for number of rows per page, including "All"
        autoWidth = TRUE, # Automatically adjust column widths
        scrollX = TRUE,   # Enable horizontal scrolling
        searchHighlight = TRUE, # Highlight search terms
        dom = 'frtip' # Elements to show (filter, table, info, pagination)
      ),
      extensions = c('Scroller') # Enable scroller extension only
    )
  })
  
  # Time series modeling with SARIMA
  output$tsPlot <- renderPlot({
    req(input$tsVariable)
    ts_data <- dataset()[[input$tsVariable]]
    
    # Apply logarithm if selected in the model adjustment tab
    if (input$applyLogModel) {
      ts_data <- log(ts_data)
    }
    
    ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    forecast_data <- forecast(fit, h = input$forecastPeriod)
    
    plot(forecast_data, main = paste("SARIMA Forecast for", input$tsVariable))
  })
  
  output$modelSummary <- renderPrint({
    req(input$tsVariable)
    ts_data <- dataset()[[input$tsVariable]]
    
    # Apply logarithm if selected in the model adjustment tab
    if (input$applyLogModel) {
      ts_data <- log(ts_data)
    }
    
    ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    summary(fit)
  })
  
  # Calculate forecast statistics
  output$forecastStats <- renderPrint({
    req(input$tsVariable)
    ts_data <- dataset()[[input$tsVariable]]
    
    # Apply logarithm if selected in the model adjustment tab
    if (input$applyLogModel) {
      ts_data <- log(ts_data)
    }
    
    ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
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
  
  output$residualsPlot <- renderPlot({
    req(input$tsVariable)
    ts_data <- dataset()[[input$tsVariable]]
    
    # Apply logarithm if selected in the model adjustment tab
    if (input$applyLogModel) {
      ts_data <- log(ts_data)
    }
    
    ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    residuals <- residuals(fit)
    plot(residuals, main = "Residuals of the Model", ylab = "Residuals", xlab = "Time")
    abline(h = 0, col = "red", lty = 2)
  })
  
  output$qqPlot <- renderPlot({
    req(input$tsVariable)
    ts_data <- dataset()[[input$tsVariable]]
    
    # Apply logarithm if selected in the model adjustment tab
    if (input$applyLogModel) {
      ts_data <- log(ts_data)
    }
    
    ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    residuals <- residuals(fit)
    qqnorm(residuals)
    qqline(residuals, col = "red")
  })
  
  output$hist_res <- renderPlot({
    req(input$tsVariable)
    ts_data <- dataset()[[input$tsVariable]]
    
    # Apply logarithm if selected in the model adjustment tab
    if (input$applyLogModel) {
      ts_data <- log(ts_data)
    }
    
    ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    residuals <- residuals(fit)
    hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")
  })
  
  output$acf_res <- renderPlot({
    req(input$tsVariable)
    ts_data <- dataset()[[input$tsVariable]]
    
    # Apply logarithm if selected in the model adjustment tab
    if (input$applyLogModel) {
      ts_data <- log(ts_data)
    }
    
    ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    residuals <- residuals(fit)
    acf(residuals, main = "ACF of Residuals")
  })
  
  # Diagnostics: Residuals summary
  output$residualsSummary <- renderPrint({
    req(input$tsVariable)
    ts_data <- dataset()[[input$tsVariable]]
    
    # Apply logarithm if selected in the model adjustment tab
    if (input$applyLogModel) {
      ts_data <- log(ts_data)
    }
    
    ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    residuals <- residuals(fit)
    summary(residuals)
  })
  
  # Handle file download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataset(), file)
    }
  )
  
  # Download CSV
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataset(), file, row.names = FALSE)
    }
  )
  
  # Download Excel
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(dataset(), file)
    }
  )
  
  # Download handlers for plots
  output$downloadWeatherPlot <- downloadHandler(
    filename = function() {
      paste("weather_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      data <- transformed_data()
      variable <- input$variable
      if (input$plotType == "Line Plot") {
        ggplot(data, aes_string(x = colnames(data)[1], y = variable)) +
          geom_line(color = "#007bff") +
          labs(x = "Date", y = variable) + theme_minimal()
      } else if (input$plotType == "Histogram") {
        ggplot(data, aes_string(x = variable)) +
          geom_histogram(binwidth = 1, fill = "#007bff", color = "black") +
          labs(x = variable, y = "Frequency")  + theme_minimal()
      } else if (input$plotType == "Box Plot") {
        ggplot(data, aes_string(x = variable)) +
          geom_boxplot(fill = "#ff7f0e") + labs(x = variable) + theme_minimal()
      }
      dev.off()
    }
  )
  
  output$downloadACFPlot <- downloadHandler(
    filename = function() {
      paste("acf_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      data <- transformed_data()
      variable <- input$variable
      ts_data <- ts(data[[variable]], frequency = 12)
      acf(ts_data, main = paste("ACF for", variable))
      dev.off()
    }
  )
  
  output$downloadPACFPlot <- downloadHandler(
    filename = function() {
      paste("pacf_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      data <- transformed_data()
      variable <- input$variable
      ts_data <- ts(data[[variable]], frequency = 12)
      pacf(ts_data, main = paste("PACF for", variable))
      dev.off()
    }
  )
  
  # Bookmarking
  observeEvent(input$bookmarkBtn, {
    session$doBookmark()
  })
  
  # Download handlers for diagnostic plots
  output$downloadResiduals <- downloadHandler(
    filename = function() {
      paste("residuals_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      ts_data <- dataset()[[input$tsVariable]]
      
      # Apply logarithm if selected in the model adjustment tab
      if (input$applyLogModel) {
        ts_data <- log(ts_data)
      }
      
      ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
      fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                   seasonal = c(input$P, input$D, input$Q))
      residuals <- residuals(fit)
      plot(residuals, ylab = "Residuals", xlab = "Time")
      abline(h = 0, col = "red", lty = 2)
      dev.off()
    }
  )
  
  output$downloadQQPlot <- downloadHandler(
    filename = function() {
      paste("qq_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      ts_data <- dataset()[[input$tsVariable]]
      
      # Apply logarithm if selected in the model adjustment tab
      if (input$applyLogModel) {
        ts_data <- log(ts_data)
      }
      
      ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
      fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                   seasonal = c(input$P, input$D, input$Q))
      residuals <- residuals(fit)
      qqnorm(residuals)
      qqline(residuals, col = "red")
      dev.off()
    }
  )
  
  output$downloadACFRes <- downloadHandler(
    filename = function() {
      paste("acf_residuals_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      ts_data <- dataset()[[input$tsVariable]]
      
      # Apply logarithm if selected in the model adjustment tab
      if (input$applyLogModel) {
        ts_data <- log(ts_data)
      }
      
      ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
      fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                   seasonal = c(input$P, input$D, input$Q))
      residuals <- residuals(fit)
      acf(residuals, main = "ACF of Residuals")
      dev.off()
    }
  )
  
  output$downloadHistRes <- downloadHandler(
    filename = function() {
      paste("hist_residuals_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      ts_data <- dataset()[[input$tsVariable]]
      
      # Apply logarithm if selected in the model adjustment tab
      if (input$applyLogModel) {
        ts_data <- log(ts_data)
      }
      
      ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
      fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                   seasonal = c(input$P, input$D, input$Q))
      residuals <- residuals(fit)
      hist(residuals, xlab = "Residuals")
      dev.off()
    }
  )
}

# Enable bookmarking
enableBookmarking(store = "url")

# Run the application
shinyApp(ui = ui, server = server)