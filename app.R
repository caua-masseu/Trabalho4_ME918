library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(readxl)
library(DT)
library(yaml)

source("function_aux.R")

config <- yaml::read_yaml("config.yaml")

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
padding: 13px;
color: #2c3e50;
background-color: #ecf0f1;
border-bottom: solid #bdc3c7;
border-radius: 20px;
margin: 5px 7px 5px 7px;
}
.sidebar {
background-color: #ecf0f1;
padding: 10px;
border-radius: 5px;
box-shadow: 10px 10px 10px rgba(0, 0, 0, 0.1);
margin: 5px 5px 5px 5px;
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
.tab-content {
margin-top: 20px;
}
"))),
  
  tags$div(
    class = "title",
    titlePanel("Análise de Séries Temporais")
  ),
  
  tabsetPanel(
    tabPanel(title = "Visualização dos Dados",
             fluidRow(class = "sidebar",
                      column(width = 4,
                             selectInput("variable", "Selecione uma Variável:", choices = c("")),
                             checkboxInput("applyLog", "Aplicar Logaritmo", value = FALSE),
                             fileInput("fileUpload", "Upload um Arquivo", accept = c(".csv", ".xlsx"))
                      ),
                      column(width = 4,
                             selectInput("plotType", "Selecione o tipo de gráfico:", choices = c("Line Plot", "Histograma", "Box Plot"))
                      ),
                      column(width = 4,
                             numericInput("numDiff", "Número de Diferenças (d):", value = 0, min = 0)
                      )
             ),
             
             fluidRow(
               class = "main-panel",
               plotlyOutput("weatherPlot"),
               column(width = 6, plotlyOutput("acfPlot")),
               column(width = 6, plotlyOutput("pacfPlot")),
               column(width = 12, DTOutput("summaryTable"))
             ),
             fluidRow(class = "main-panel",
                      DTOutput("dadosTable"),
                      column(width = 12,
                             downloadButton("downloadCSV", "Download CSV", class = "btn"),
                             downloadButton("downloadExcel", "Download Excel", class = "btn"),
                             bookmarkButton()
                      )
             )
    ),
    tabPanel(title = "Ajuste do Modelo",
             fluidRow(class = "sidebar",
                      column(width = 3,
                             selectInput("tsVariable", "Selecione uma variável:", choices = c("")),
                             checkboxInput("applyLogModel", "Aplicar Logaritmo", value = FALSE),
                             numericInput("forecastPeriod", "Período de Previsão:", 12, min = 1),
                             numericInput("p", "AR Order (p):", 1, min = 0),
                             numericInput("d", "Differencing Order (d):", 0, min = 0),
                             numericInput("q", "MA Order (q):", 1, min = 0),
                             numericInput("P", "Seasonal AR Order (P):", 1, min = 0),
                             numericInput("D", "Seasonal Differencing Order (D):", 0, min = 0),
                             numericInput("Q", "Seasonal MA Order (Q):", 1, min = 0),
                             numericInput("S", "Seasonal Period (S):", 12, min = 1)
                      ),
                      column(width = 9,
                             class = "main-panel2",
                             plotlyOutput("tsPlot"),
                             plotlyOutput("observedVsPredictedPlot"),
                             verbatimTextOutput("modelSummary"),
                             verbatimTextOutput("forecastStats")
                      )
             )
    ),
    tabPanel(title = "Diagnóstico",
             fluidRow(class = "main-panel",
                      column(6, plotlyOutput("residualsPlot")),
                      column(6, plotlyOutput("qqPlot")),
                      column(6, plotlyOutput("acf_res")),
                      column(6, plotlyOutput("hist_res")),
                      column(width = 12, DTOutput("residualsSummaryTable"))
             )
    )
  )
)

server <- function(input, output, session) {
  
  dataset <- reactiveVal(NULL)
  
  # Carrega dataset inicial
  observe({
    initial_data <- load_initial_data("PortoAlegre.xlsx")
    dataset(initial_data)
    
    updateSelectInput(session, "variable", choices = colnames(initial_data)[-1])
    updateSelectInput(session, "tsVariable", choices = colnames(initial_data)[-1])
  })
  
  observeEvent(input$fileUpload, {
    inFile <- input$fileUpload
    if (is.null(inFile)) return(NULL)
    
    # Determina tipo do arquivo upload
    if (grepl("\\.csv$", inFile$name)) {
      uploaded_data <- read.csv(inFile$datapath)
    } else if (grepl("\\.xlsx$", inFile$name)) {
      uploaded_data <- read_xlsx(inFile$datapath)
    } else {
      return(NULL)
    }
    
    # Verifique o formato da data e ajuste conforme necessário
    # Supondo que o formato seja "DD/MM/YYYY"
    uploaded_data[[1]] <- as.Date(uploaded_data[[1]], format = "%d/%m/%Y")
    
    dataset(uploaded_data)
    
    # Limpar seleções antes de adicionar novas opções
    updateSelectInput(session, "variable", choices = c("", colnames(uploaded_data)[-1]))
    updateSelectInput(session, "tsVariable", choices = c("", colnames(uploaded_data)[-1]))
  })
  
  transformed_data <- reactive({
    req(dataset(), input$variable)
    data <- dataset()
    
    variable_data <- data[[input$variable]]
    
    # Verifique se a variável é numérica
    validate(
      need(is.numeric(variable_data), "A variável selecionada não é numérica. Por favor, selecione uma variável numérica.")
    )
    
    if (input$applyLog) {
      variable_data <- log(variable_data)
    }
    
    if (input$numDiff > 0) {
      variable_data <- diff(variable_data, differences = input$numDiff)
      variable_data <- c(rep(NA, input$numDiff), variable_data)
    }
    data[[input$variable]] <- variable_data
    data
  })
  
  output$weatherPlot <- renderPlotly({
    validate(
      need(input$variable != "", "Por favor, selecione uma variável"),
      need(nrow(transformed_data()) > 0, "No data available.")
    )
    
    data <- transformed_data()
    variable <- input$variable
    
    if (input$plotType == "Line Plot") {
      p <- ggplot(data, aes_string(x = colnames(data)[1], y = variable)) +
        geom_line(color = "#007bff") +
        labs(x = "Date", y = variable, title = paste("Plot de", variable)) +
        theme_minimal()
    } else if (input$plotType == "Histograma") {
      p <- ggplot(data, aes_string(x = variable)) +
        geom_histogram(fill = "#009bff", color = "black", bins = 30) +
        labs(x = variable, y = "Frequency", title = paste("Histograma de", variable)) +
        theme_minimal()
    } else if (input$plotType == "Box Plot") {
      p <- ggplot(data, aes_string(y = variable)) +
        geom_boxplot(fill = "#009bff") +
        labs(y = variable, title = paste("Box Plot de", variable)) +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  output$acfPlot <- renderPlotly({
    data <- transformed_data()
    variable <- input$variable
    
    ts_data <- na.omit(ts(data[[variable]], frequency = 12))
    
    acf_data <- acf(ts_data, plot = FALSE)
    bartlett_limit <- 1.96 / sqrt(length(ts_data))
    
    acf_df <- data.frame(lag = acf_data$lag, acf = acf_data$acf)
    
    p <- ggplot(acf_df, aes(x = lag, y = acf)) +
      geom_bar(stat = "identity", fill = "#007bff") +
      geom_hline(yintercept = c(-bartlett_limit, bartlett_limit), linetype = "dashed", color = "red") +
      labs(title = paste("FAC de", variable), x = "Lag", y = "ACF") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$pacfPlot <- renderPlotly({
    data <- transformed_data()
    variable <- input$variable
    
    ts_data <- na.omit(ts(data[[variable]], frequency = 12))
    
    pacf_data <- pacf(ts_data, plot = FALSE)
    bartlett_limit <- 1.96 / sqrt(length(ts_data))
    
    pacf_df <- data.frame(lag = pacf_data$lag, pacf = pacf_data$acf)
    
    p <- ggplot(pacf_df, aes(x = lag, y = pacf)) +
      geom_bar(stat = "identity", fill = "#007bff") +
      geom_hline(yintercept = c(-bartlett_limit, bartlett_limit), linetype = "dashed", color = "red") +
      labs(title = paste("FACP de", variable), x = "Lag", y = "PACF") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$summaryTable <- renderDT({
    req(input$variable)
    
    summary_data <- summary(transformed_data()[[input$variable]])
    
    summary_df <- data.frame(
      Statistic = names(summary_data),
      Value = as.vector(summary_data)
    )
    
    transposed_df <- as.data.frame(t(summary_df))
    
    colnames(transposed_df) <- transposed_df[1, ]
    transposed_df <- transposed_df[-1, , drop = FALSE]
    
    datatable(
      transposed_df,
      extensions = 'Buttons',
      options = list(
        dom = 'B',
        buttons = c('csv', 'excel'),
        paging = FALSE,  
        info = FALSE,    
        autoWidth = TRUE,
        ordering = FALSE,
        scrollX = TRUE,
        searchHighlight = TRUE
      )
    )
  })
  
  output$dadosTable <- renderDT({
    datatable(
      dataset(),
      options = list(
        pageLength = config$datatable$pageLength,
        autoWidth = TRUE,
        scrollX = TRUE,
        searchHighlight = TRUE,
        dom = 'frtip'
      ),
      extensions = c('Scroller')
    )
  })
  
  fitted_model <- reactiveVal(NULL)
  
  observeEvent({
    list(input$tsVariable, input$applyLogModel, input$p, input$d, input$q, input$P, input$D, input$Q, input$S)
  }, {
    req(input$tsVariable)
    ts_data <- dataset()[[input$tsVariable]]
    
    # Verifique se a variável é numérica
    validate(
      need(is.numeric(ts_data), "A variável selecionada para o modelo não é numérica. Por favor, selecione uma variável numérica.")
    )
    
    if (input$applyLogModel) {
      ts_data <- log(ts_data)
    }
    
    ts_data <- ts(ts_data, frequency = input$S)
    fit <- Arima(ts_data, order = c(input$p, input$d, input$q), seasonal = c(input$P, input$D, input$Q))
    fitted_model(fit)
  })
  
  # MODELANDO SARIMA
  output$tsPlot <- renderPlotly({
    req(fitted_model())
    forecast_data <- forecast(fitted_model(), h = input$forecastPeriod)
    
    plot_data <- data.frame(
      Time = as.numeric(time(forecast_data$mean)),
      Forecast = as.numeric(forecast_data$mean),
      Lower = as.numeric(forecast_data$lower[, 2]),
      Upper = as.numeric(forecast_data$upper[, 2])
    )
    
    original_data <- dataset()[[input$tsVariable]]
    if (input$applyLogModel) {
      original_data <- log(original_data)
    }
    original_data <- ts(original_data, frequency = input$S)
    original_plot_data <- data.frame(
      Time = as.numeric(time(original_data)),
      Value = as.numeric(original_data)
    )
    
    p <- ggplot() +
      geom_line(data = original_plot_data, aes(x = Time, y = Value), color = "blue", size = 1, alpha = 0.7) +
      geom_line(data = plot_data, aes(x = Time, y = Forecast), color = "red", size = 1) +
      geom_ribbon(data = plot_data, aes(x = Time, ymin = Lower, ymax = Upper), fill = "grey80", alpha = 0.5) +
      labs(title = paste("Previsão para", input$tsVariable), x = "Time", y = input$tsVariable) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$modelSummary <- renderPrint({
    req(fitted_model())
    summary(fitted_model())
  })
  
  output$observedVsPredictedPlot <- renderPlotly({
    req(fitted_model())
    ts_data <- dataset()[[input$tsVariable]]
    
    if (input$applyLogModel) {
      ts_data <- log(ts_data)
    }
    
    ts_data <- ts(ts_data, frequency = input$S, start = c(1990, 1))
    fit <- fitted_model()
    
    actuals <- window(ts_data, start = start(ts_data), end = end(ts_data))
    predictions <- fitted(fit)
    
    min_length <- min(length(actuals), length(predictions))
    actuals <- actuals[1:min_length]
    predictions <- predictions[1:min_length]
    
    plot_data <- data.frame(
      Time = as.numeric(time(actuals)),
      Observed = as.numeric(actuals),
      Predicted = as.numeric(predictions)
    )
    
    p <- ggplot(plot_data, aes(x = Time)) +
      geom_line(aes(y = Observed, color = "Observado"), size = 1) +
      geom_line(aes(y = Predicted, color = "Predito"), linetype = "dashed", size = 1) +
      labs(title = "Valores Observados vs Preditos", x = "Time", y = "Values") +
      scale_color_manual(values = c("Observado" = "blue", "Predito" = "red")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Calcular estatistica de previsao
  output$forecastStats <- renderPrint({
    req(fitted_model())
    ts_data <- dataset()[[input$tsVariable]]
    
    if (input$applyLogModel) {
      ts_data <- log(ts_data)
    }
    
    ts_data <- ts(ts_data, frequency = input$S)
    forecast_data <- forecast(fitted_model(), h = input$forecastPeriod)
    
    actuals <- window(ts_data, start = end(ts_data) - input$forecastPeriod + 1)
    predictions <- forecast_data$mean[1:length(actuals)]
    
    mse <- mean((actuals - predictions)^2, na.rm = TRUE)
    mae <- mean(abs(actuals - predictions), na.rm = TRUE)
    
    cat("Estatísticas de Previsão:\n")
    cat("Erro Quadrático Médio (EQM):", mse, "\n")
    cat("Erro Absoluto Médio (EAM):", mae, "\n")
  })
  
  # Gráficos no Tab Diagnóstico
  output$residualsPlot <- renderPlotly({
    req(fitted_model())
    residuals <- residuals(fitted_model())
    residuals_df <- data.frame(Time = seq_along(residuals), Residuals = residuals)
    p <- ggplot(residuals_df, aes(x = Time, y = Residuals)) +
      geom_line(color = "blue") +
      labs(title = "Resíduos ao longo do tempo", x = "Tempo", y = "Resíduos") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$qqPlot <- renderPlotly({
    req(fitted_model())
    residuals <- residuals(fitted_model())
    qq_data <- qqnorm(residuals, plot.it = FALSE)
    qq_df <- data.frame(Theoretical = qq_data$x, Sample = qq_data$y)
    p <- ggplot(qq_df, aes(x = Theoretical, y = Sample)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      labs(title = "Q-Q Plot dos Resíduos", x = "Quantis Teóricos", y = "Quantis Amostrais") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$acf_res <- renderPlotly({
    req(fitted_model())
    residuals <- residuals(fitted_model())
    acf_data <- acf(residuals, plot = FALSE)
    acf_df <- data.frame(Lag = acf_data$lag, ACF = acf_data$acf)
    p <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = "Autocorrelação dos Resíduos", x = "Lag", y = "ACF") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$hist_res <- renderPlotly({
    req(fitted_model())
    residuals <- residuals(fitted_model())
    p <- ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
      geom_histogram(fill = "blue", color = "black", bins = 30) +
      labs(title = "Histograma dos Resíduos", x = "Resíduos", y = "Frequência") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Diagnostics: Residuals summary table
  output$residualsSummaryTable <- renderDT({
    req(fitted_model())
    residuals <- residuals(fitted_model())
    
    summary_data <- summary(residuals)
    
    summary_df <- data.frame(
      Statistic = names(summary_data),
      Value = as.vector(summary_data)
    )
    
    transposed_df <- as.data.frame(t(summary_df))
    
    colnames(transposed_df) <- transposed_df[1, ]
    transposed_df <- transposed_df[-1, , drop = FALSE]
    
    datatable(
      transposed_df,
      extensions = 'Buttons',
      options = list(
        dom = 'B',
        buttons = c('csv', 'excel'),
        paging = FALSE,
        info = FALSE,
        autoWidth = TRUE,
        ordering = FALSE,
        scrollX = TRUE,
        searchHighlight = TRUE
      )
    )
  })
  
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
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")