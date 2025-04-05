library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(stringr)
library(DT)

#UI Definition
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Airline Ticket Price Prediction Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Model Selection"),
      selectInput("model_select", "Choose Model:",
                  choices = c("Linear Regression", "ARIMA", "Holt-Winters"),
                  selected = "Linear Regression"),
      
      hr(),
      
      conditionalPanel(
        condition = "input.model_select == 'Linear Regression'",
        h4("Linear Regression Features"),
        checkboxGroupInput("lm_features", "Select Predictors:",
                           choices = c("Airline", "Source", "Destination",
                                       "Total_Stops_Num", "Duration_Minutes",
                                       "Journey_Month", "Journey_DayOfWeek"),
                           selected = c("Airline", "Source", "Destination", "Total_Stops_Num", "Duration_Minutes"))
      ),
      
      conditionalPanel(
        condition = "input.model_select == 'ARIMA'",
        h4("ARIMA Parameters"),
        p("Predicts based on the time series of average daily price."),
        numericInput("arima_p", "ARIMA Order (p):", value = 1, min = 0, step = 1),
        numericInput("arima_d", "ARIMA Order (d):", value = 1, min = 0, step = 1),
        numericInput("arima_q", "ARIMA Order (q):", value = 0, min = 0, step = 1)
      ),
      
      conditionalPanel(
        condition = "input.model_select == 'Holt-Winters'",
        h4("Holt-Winters Parameters"),
        p("Predicts based on the time series of average daily price."),
        sliderInput("hw_alpha", "Alpha (Level Smoothing):", min = 0, max = 1, value = 0.2, step = 0.05),
        sliderInput("hw_beta", "Beta (Trend Smoothing):", min = 0, max = 1, value = 0.1, step = 0.05),
        sliderInput("hw_gamma", "Gamma (Seasonal Smoothing):", min = 0, max = 1, value = 0.1, step = 0.05),
        selectInput("hw_seasonal", "Seasonal Component Type:",
                    choices = c("additive", "multiplicative"), selected = "additive")
      ),
      
      hr(),
      actionButton("run_model", "Fit Selected Model", icon = icon("cogs")),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Prediction Plot",
                 plotOutput("prediction_plot", height = "500px"),
                 verbatimTextOutput("plot_info")),
        tabPanel("Model Summary",
                 verbatimTextOutput("model_summary")),
        tabPanel("Processed Data Table",
                 h4("Data Used for Models"),
                 p("Note: Time series models use daily averages derived from this."),
                 DTOutput("data_table")),
        tabPanel("Raw Data Sample",
                 h4("Original Loaded Data (First 100 Rows)"),
                 tableOutput("raw_table"))
      ),
      width = 9
    )
  )
)

#Server Logic
server <- function(input, output, session) {
  
  # Reactive value to store the loaded and processed data
  data_loaded <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    daily_avg_price = NULL,
    price_ts = NULL
  )
  
  observe({
    file_path <- "Data_Train.xlsx"
    if (file.exists(file_path)) {
      data_loaded$raw_data <- read_excel(file_path)
    } else {
      showNotification("Excel file not found at specified path!", type = "error")
    }
    
    if (!is.null(data_loaded$raw_data)) {
      # Preprocessing function
      preprocess_data <- function(df) {
        df <- df %>%
          mutate(
            Date_of_Journey = as.Date(Date_of_Journey, format = "%d/%m/%Y"),
            Duration_Minutes = {
              hours <- as.numeric(str_extract(Duration, "\\d+(?=h)"))
              mins <- as.numeric(str_extract(Duration, "\\d+(?=m)"))
              hours[is.na(hours)] <- 0
              mins[is.na(mins)] <- 0
              hours * 60 + mins
            },
            Total_Stops_Num = case_when(
              tolower(Total_Stops) == "non-stop" ~ 0,
              grepl("\\d+", Total_Stops) ~ as.numeric(str_extract(Total_Stops, "\\d+")),
              TRUE ~ NA_real_
            ),
            Journey_Month = month(Date_of_Journey),
            Journey_DayOfWeek = wday(Date_of_Journey, label = TRUE, week_start = 1),
            Airline = factor(Airline),
            Source = factor(Source),
            Destination = factor(Destination)
          ) %>%
          filter(!is.na(Date_of_Journey), !is.na(Price))
        
        return(df)
      }
      
      # Apply preprocessing
      data_loaded$processed_data <- preprocess_data(data_loaded$raw_data)
      
      # Prepare time series data
      data_loaded$daily_avg_price <- data_loaded$processed_data %>%
        group_by(Date_of_Journey) %>%
        summarise(Avg_Price = mean(Price, na.rm = TRUE)) %>%
        arrange(Date_of_Journey) %>%
        filter(!is.na(Avg_Price))
      
      # Create time series object
      if (nrow(data_loaded$daily_avg_price) > 0) {
        time_diff_days <- as.numeric(max(data_loaded$daily_avg_price$Date_of_Journey) - 
                                       min(data_loaded$daily_avg_price$Date_of_Journey))
        ts_freq <- if(time_diff_days >= 730) 365 else if (time_diff_days >= 60) 7 else 1
        
        if(nrow(data_loaded$daily_avg_price) > ts_freq) {
          data_loaded$price_ts <- ts(data_loaded$daily_avg_price$Avg_Price,
                                     start = c(year(min(data_loaded$daily_avg_price$Date_of_Journey)),
                                               yday(min(data_loaded$daily_avg_price$Date_of_Journey))),
                                     frequency = ts_freq)
        }
      }
    }
  })

  # Reactive expression for fitted model
  fitted_model <- eventReactive(input$run_model, {
    model_type <- input$model_select
    req(model_type)
    
    model_obj <- NULL
    error_msg <- NULL
    
    tryCatch({
      if (model_type == "Linear Regression") {
        req(input$lm_features)
        req(data_loaded$processed_data)
        
        formula_str <- paste("Price ~", paste(input$lm_features, collapse = " + "))
        model_formula <- as.formula(formula_str)
        model_obj <- lm(model_formula, data = data_loaded$processed_data)
        
      } else if (model_type == "ARIMA") {
        req(data_loaded$price_ts)
        req(input$arima_p, input$arima_d, input$arima_q)
        
        order_vec <- c(input$arima_p, input$arima_d, input$arima_q)
        model_obj <- Arima(data_loaded$price_ts, order = order_vec)
        
      } else if (model_type == "Holt-Winters") {
        req(data_loaded$price_ts)
        req(input$hw_alpha, input$hw_beta, input$hw_gamma, input$hw_seasonal)
        
        model_obj <- HoltWinters(data_loaded$price_ts,
                                 alpha = input$hw_alpha,
                                 beta = if(input$hw_beta > 0) input$hw_beta else FALSE,
                                 gamma = if(input$hw_gamma > 0) input$hw_gamma else FALSE,
                                 seasonal = input$hw_seasonal)
      }
    }, error = function(e) {
      error_msg <<- paste("Error fitting", model_type, ":", e$message)
      print(error_msg)
    })
    
    list(model = model_obj, error = error_msg, type = model_type)
  })
  
  # Outputs 
  output$model_summary <- renderPrint({
    result <- fitted_model()
    req(result)
    
    if (!is.null(result$error)) {
      cat("Model Fitting Error:\n")
      cat(result$error)
    } else if (is.null(result$model)) {
      cat("Model could not be fitted. Check parameters and data.")
    } else {
      cat(paste("Summary for:", result$type, "Model\n\n"))
      if (result$type == "Linear Regression") {
        summary(result$model)
      } else if (result$type == "ARIMA") {
        summary(result$model)
      } else if (result$type == "Holt-Winters") {
        print(result$model)
        cat("\nFitted Values (first 10):\n")
        print(head(result$model$fitted, 10))
      }
    }
  })
  
  output$prediction_plot <- renderPlot({
    result <- fitted_model()
    req(result)
    
    if (!is.null(result$error) || is.null(result$model)) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      title(main = "Model Fitting Failed", sub = result$error %||% "Check parameters/data")
      return()
    }
    
    model <- result$model
    model_type <- result$type
    
    if (model_type == "Linear Regression") {
      req(data_loaded$processed_data)
      preds <- predict(model, newdata = data_loaded$processed_data)
      plot_data <- data.frame(Actual = data_loaded$processed_data$Price, Predicted = preds)
      ggplot(plot_data, aes(x = Actual, y = Predicted)) +
        geom_point(alpha = 0.5) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = "Linear Regression: Actual vs. Predicted Prices",
             x = "Actual Price", y = "Predicted Price") +
        theme_minimal() +
        coord_fixed(ratio = 1, xlim = range(plot_data$Actual, na.rm=T), ylim = range(plot_data$Predicted, na.rm=T))
      
    } else if (model_type %in% c("ARIMA", "Holt-Winters")) {
      req(data_loaded$price_ts)
      autoplot(data_loaded$price_ts, series = "Original Avg Daily Price") +
        autolayer(fitted(model), series = "Fitted Values") +
        labs(title = paste(model_type, "Model Fit"),
             y = "Average Daily Price", x = "Time") +
        theme_minimal() +
        scale_colour_manual(values = c("Original Avg Daily Price" = "black", "Fitted Values" = "blue")) +
        theme(legend.position = "bottom")
    }
  })
  
  output$plot_info <- renderText({
    result <- fitted_model()
    req(result)
    
    if (!is.null(result$error)) {
      return(paste("Plot cannot be generated due to model error:", result$error))
    }
    if (is.null(result$model)) {
      return("Plot cannot be generated, model not fitted.")
    }
    
    if (result$type == "Linear Regression") {
      "The plot shows predicted prices (from the linear model) against the actual prices in the dataset. A perfect model would have all points on the dashed red line (y=x)."
    } else if (result$type %in% c("ARIMA", "Holt-Winters")) {
      if (is.null(data_loaded$price_ts)) return("Time series data was not suitable for plotting.")
      "The plot shows the original time series of average daily prices (black) and the values fitted by the selected time series model (blue)."
    } else {
      ""
    }
  })
  
  output$data_table <- renderDT({
    req(data_loaded$processed_data)
    datatable(data_loaded$processed_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$raw_table <- renderTable({
    req(data_loaded$raw_data)
    head(data_loaded$raw_data, 100)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
