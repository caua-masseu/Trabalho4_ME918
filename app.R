# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(forecast)
library(readxl)
library(gridExtra)
library(DT)

# Load the dataset
weather_data <- read_xlsx("C:\\Users\\PC\\Desktop\\PortoAlegre.xlsx", skip = 6)
colnames(weather_data) <- c("Date", "Cloudiness", "Rainfall", "Max_Temperature",
                            "Min_Temperature", "Humidity", "Mean_Temperature")

weather_data$Date <- as.Date(weather_data$Date)

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
    padding: 20px;
    color: #2c3e50;
    background-color: #ecf0f1;
    border-bottom: solid #bdc3c7;
    border-radius: 20px;
    margin: 5px 7px 5px 7px;
  }
  .sidebar {
    background-color: #ecf0f1;
    padding: 12px;
  }
  .main-panel {
    background-color: #ffffff;
    padding: 20px;
    border-radius: 5px;
    box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
    margin: 5px 7px 5px 7px;
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
    titlePanel("Weather Data")
  ),
  
  tabsetPanel(
    tabPanel(title =  "Visualização dos dados",
             class = "title",
             fluidRow(class = "sidebar",
                      column(width = 3,
                             selectInput("variable", "Select a Variable:",
                                         choices = colnames(weather_data)[-1]),
                             uiOutput("dynamicUI")
                      ),
                      column(width = 3,
                             dateRangeInput("dateRange", "Select Date Range:", start = min(weather_data$Date), end = max(weather_data$Date))
                      ),
                      column(width = 3,
                             selectInput("plotType", "Select Plot Type:", choices = c("Line Plot", "Histogram", "Box Plot"))
                      )
             ),
             
             fluidRow(
               class = "main-panel",
               plotOutput("weatherPlot"),
               tableOutput("summaryTable"),
               column(width = 6, plotOutput("acfPlot")),
               column(width = 6, plotOutput("pacfPlot")),                     
               column(width = 5, fileInput("fileUpload", "Upload a File")),
               column(width = 4, downloadButton("downloadData", "Download Data", class = "btn")),
               column(width = 3, actionButton("bookmarkBtn", "Bookmark", class = "btn"))
             ),
             fluidRow(class = "main-panel",
               DTOutput("dadosTable")  # Use DTOutput instead of tableOutput
             )
    ),
    tabPanel(title = "Time Series Modeling",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar",
                 selectInput("tsVariable", "Selecione uma variável:",
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
    ),
    
    tabPanel(title = "Diagnóstico",
             fluidRow(
               column(6, plotOutput("residualsPlot"), downloadButton("downloadResiduals", "Download Residuals Plot", class = "btn")),
               column(6, plotOutput("qqPlot"), downloadButton("downloadQQPlot", "Download Q-Q Plot", class = "btn")),
               column(6, plotOutput("acf_res"), downloadButton("downloadACFRes", "Download ACF Residuals Plot", class = "btn")),
               column(6, plotOutput("hist_res"), downloadButton("downloadHistRes", "Download Histogram Residuals Plot", class = "btn")),
               verbatimTextOutput("residualsSummary")
             ))
  )
)

# Define server logic
server <- function(input, output, session) {
  # Dynamic UI for additional inputs
  output$dynamicUI <- renderUI({
    if (input$variable == "Rainfall") {
      sliderInput("rainfallThreshold", "Rainfall Threshold:", min = 0, max = 100, value = 50)
    } else {
      NULL
    }
  })
  
  # Reactive expression to filter data based on user input
  filtered_data <- reactive({
    weather_data %>%
      filter(Date >= input$dateRange[1],
             Date <= input$dateRange[2])
  })
  
  # Generate plot based on filtered data and selected plot type
  output$weatherPlot <- renderPlot({
    validate(
      need(input$variable != "", "Please select a variable."),
      need(nrow(filtered_data()) > 0, "No data available for the selected date range.")
    )
    
    data <- filtered_data()
    variable <- input$variable
    
    if (input$plotType == "Line Plot") {
      ggplot(data, aes_string(x = "Date", y = variable)) +
        geom_line(color = "#007bff") +
        labs(x = "Date", y = variable) + theme_minimal()
    } else if (input$plotType == "Histogram") {
      ggplot(data, aes_string(x = variable)) +
        geom_histogram(binwidth = 1, fill = "#007bff", color = "black") +
        labs(x = variable, y = "Frequência")  + theme_minimal()
    } else if (input$plotType == "Box Plot") {
      ggplot(data, aes_string(x = variable)) +
        geom_boxplot(fill = "#ff7f0e") + labs(x = variable) + theme_minimal()
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
    Acf(ts_data, main = paste("FAC para", variable))
  })
  
  # PACF plot
  output$pacfPlot <- renderPlot({
    data <- filtered_data()
    variable <- input$variable
    ts_data <- ts(data[[variable]], frequency = 12)
    Pacf(ts_data, main = paste("FACP para", variable))
  })
  
  output$dadosTable <- renderDT({
    datatable(
      weather_data,
      options = list(
        pageLength = 10,  # Default number of rows per page
        lengthMenu = c(10, 25, 50, 100, nrow(weather_data)), # Options for number of rows per page, including "All"
        autoWidth = TRUE, # Automatically adjust column widths
        scrollX = TRUE,   # Enable horizontal scrolling
        searchHighlight = TRUE, # Highlight search terms
        dom = 'Bfrtip', # Elements to show (buttons, filter, etc.)
        buttons = list(
          list(
            extend = 'copy',
            text = 'Copiar',
            exportOptions = list(
              modifier = list(page = 'all') # Export all data
            )
          ),
          list(
            extend = 'csv',
            text = 'CSV',
            exportOptions = list(
              modifier = list(page = 'all') # Export all data
            )
          ),
          list(
            extend = 'excel',
            text = 'Excel',
            exportOptions = list(
              modifier = list(page = 'all') # Export all data
            )
          )
        ),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json'
        )
      ),
      extensions = c('Buttons', 'Scroller') # Enable buttons and scroller extensions
    )
  })
  # Time series modeling with SARIMA
  output$tsPlot <- renderPlot({
    ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    forecast_data <- forecast(fit, h = input$forecastPeriod)
    
    plot(forecast_data, main = paste("SARIMA Forecast for", input$tsVariable))
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
  
  # Diagnostics: Residuals plot
  output$residualsPlot <- renderPlot({
    ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    residuals <- residuals(fit)
    plot(residuals, main = "Residuals of the Model", ylab = "Residuals", xlab = "Time")
    abline(h = 0, col = "red", lty = 2)
  })
  
  # Diagnostics: Q-Q plot
  output$qqPlot <- renderPlot({
    ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    residuals <- residuals(fit)
    qqnorm(residuals)
    qqline(residuals, col = "red")
  })
  
  output$hist_res <- renderPlot({
    ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    residuals <- residuals(fit)
    hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")
  })
  
  output$acf_res <- renderPlot({
    ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    residuals <- residuals(fit)
    acf(residuals, main = "ACF of Residuals")
  })
  
  # Diagnostics: Residuals summary
  output$residualsSummary <- renderPrint({
    ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                 seasonal = c(input$P, input$D, input$Q))
    residuals <- residuals(fit)
    summary(residuals)
  })
  
  # Handle file download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("weather_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(weather_data, file)
    }
  )
  
  # Handle file upload
  observeEvent(input$fileUpload, {
    inFile <- input$fileUpload
    if (is.null(inFile)) return(NULL)
    uploaded_data <- read.csv(inFile$datapath)
    # Process uploaded_data as needed
  })
  
  # Bookmarking
  observeEvent(input$bookmarkBtn, {
    session$doBookmark()
  })
  
  onRestore(function(state) {
    # Custom logic to restore state if needed
  })
  
  # Download handlers for diagnostic plots
  output$downloadResiduals <- downloadHandler(
    filename = function() {
      paste("residuals_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
      fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                   seasonal = c(input$P, input$D, input$Q))
      residuals <- residuals(fit)
      plot(residuals, ylab = "Residuos", xlab = "Time")
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
      ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
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
      ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
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
      ts_data <- ts(weather_data[[input$tsVariable]], frequency = input$S, start = c(1990, 1))
      fit <- Arima(ts_data, order = c(input$p, input$d, input$q),
                   seasonal = c(input$P, input$D, input$Q))
      residuals <- residuals(fit)
      hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")
      dev.off()
    }
  )
}

# Enable bookmarking
enableBookmarking(store = "url")

# Run the application
shinyApp(ui = ui, server = server)