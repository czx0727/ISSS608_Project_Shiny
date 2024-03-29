library(shiny)
library(tidyverse)
library(tmap)
library(dplyr)
library(fable)
library(tsibble)
library(ggplot2)
library(lubridate)  # For year() and month() functions
library(feasts)
library(plotly)
library(xts)
library(ggthemes)
library(hrbrthemes)
library(MASS)
library(mgcv)

mpsz_SES_filtered <- read_rds("data/mpsz_SES_filtered_1.rds")
data <- readRDS("data/SEO_TS.rds")
data1 <- readRDS("data/helectricity.rds")
conversion_long <- read_rds("data/conversion_long.rds")

# Define UI for application that includes tabs
ui <- fluidPage(
  titlePanel("Electricity Consumption In Singapore"),
  
  tags$head(
    tags$style(HTML("
      /* Add color to the title panel */
      .titlePanel {
        background-color: #3366CC; /* You can use any color code or name */
        color: white; /* Text color */
      }

      /* Add color to the tabset panel */
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        background-color: #3366CC; /* Background color of active tab */
        color: white; /* Text color of active tab */
      }

      /* Add color to the content area */
      .tab-content {
        background-color: #F0F0F0; /* Background color of content area */
      }
    "))
  ),
  
  tabsetPanel(
    tabPanel("Overview",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Generation",
                             label = "Electricity Generation:",
                             choices=list("Inputs"="Inputs",
                                          "Outputs"="Outputs",
                                          "Conversion_rate"="Conversion_rate")),
                 selectInput(inputId = "fitting_method",
                             label = "Fitting Method:",
                             choices=list("lm"="lm",
                                          "glm"="glm",
                                          "loess"="loess",
                                          "gam"="gam",
                                          "rlm"="rlm"))
               ),
               mainPanel(
                 plotlyOutput("generation_plot")
               )
             )
    ),
    tabPanel("By Dwelling Type",
             sidebarLayout(
               sidebarPanel(
                 selectInput("dwelling", "Dwelling Type",
                             choices = unique(data1$dwellingtype))
               ),
               mainPanel(
                 tabPanel("Monthly Plot", plotlyOutput("monthlyPlot"))
               )
             )
    ),
    tabPanel("By Region",
             sidebarLayout(
               sidebarPanel(
                 selectInput("dwelling", "Dwelling Type:", choices = unique(mpsz_SES_filtered$dwelling_type)),
                 selectInput("year", "Year:", choices = unique(mpsz_SES_filtered$year)),
                 selectInput("month", "Month:", choices = unique(mpsz_SES_filtered$month)),
                 selectInput(inputId = "classification",
                             label = "Classification method:",
                             choices = list("sd" = "sd", 
                                            "equal" = "equal", 
                                            "pretty" = "pretty", 
                                            "quantile" = "quantile", 
                                            "kmeans" = "kmeans", 
                                            "hclust" = "hclust", 
                                            "bclust" = "bclust", 
                                            "fisher" = "fisher", 
                                            "jenks" = "jenks"),
                             selected = "pretty"),
                 selectInput(inputId = "colour",
                             label = "Colour scheme:",
                             choices = list("blues" = "Blues", 
                                            "reds" = "Reds", 
                                            "greens" = "Greens",
                                            "Yellow-Orange-Red" = "YlOrRd",
                                            "Yellow-Orange-Brown" = "YlOrBr",
                                            "Yellow-Green" = "YlGn",
                                            "Orange-Red" = "OrRd"),
                             selected = "YlOrRd")
               ),
               mainPanel(
                 uiOutput("map")
               )
             )
    ),
    tabPanel("Time Series",
             sidebarLayout(
               sidebarPanel(
                 selectInput("dwelling", "Region:", choices = unique(data$DWELLING_TYPE)),
                 numericInput("forecast_months", "Number of Months to Forecast:", min = 1, max = 120, value = 12),
                 selectInput("selected_model", "Select Model:", choices = c("ETS", "ARIMA")),
                 actionButton("submit", "Submit")
               ),
               mainPanel(
                 plotOutput("consumption_plot"),
                 textOutput("mape_output"),
                 textOutput("mae_output")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    filtered <- mpsz_SES_filtered
    if (!is.null(input$dwelling)) {
      filtered <- filtered %>% filter(dwelling_type == input$dwelling)
    }
    if (!is.null(input$year)) {
      filtered <- filtered %>% filter(year == input$year)
    }
    if (!is.null(input$month)) {
      filtered <- filtered %>% filter(month == input$month)
    }
    return(filtered)
  })
  
  #Geospatial_Analysis
  
  output$map <- renderUI({
    req(filtered_data())
    if (nrow(filtered_data()) == 0) {
      return(NULL)
    }
    
    tmap_mode("view")
    tmap_options(check.and.fix = TRUE)
    
    elecmap <- tm_shape(filtered_data()) +
      tm_fill("kwh_per_acc", 
              style = input$classification,  # Use the selected classification method
              palette = input$colour,  # Use the selected color scheme
              title = "Electricity Consumption by Percentile") +
      tm_facets(by = c("year", "month"), ncol = 4) +
      tm_layout(main.title = "Total Household Electricity Consumption by Percentile",
                main.title.position = "center",
                main.title.size = 1.2,
                legend.height = 0.45, 
                legend.width = 0.35,
                frame = TRUE) +
      tm_borders(alpha = 0.5) +
      tm_scale_bar() +
      tm_grid(alpha = 0.2)
    
    print(elecmap)
  })
  
  #Time_Series
  
  # Initialize reactiveValues to store filtered data
  data_filtered <- reactiveValues()
  
  # Update filtered data when submit button is clicked
  observeEvent(input$submit, {
    data_filtered$filtered_data <- data %>%
      filter(DWELLING_TYPE == input$dwelling)
  })
  
  # Function to calculate forecasts
  calculate_forecasts <- function(data, method, h) {
    data_ts <- tsibble(
      Month_Year = yearmonth(paste(data$year, data$month)),
      DWELLING_TYPE = data$DWELLING_TYPE,
      Consumption_GWh = data$consumption_GWh,
      index = Month_Year
    )
    
    fit <- data_ts %>%
      model(method = method)
    fc <- fit %>%
      forecast(h = h)
    return(fc)
  }
  
  # Reactive expression to generate forecasts
  forecasts <- reactive({
    # Check if filtered data is available
    if (is.null(data_filtered$filtered_data)) {
      return(NULL)
    }
    
    # Calculate forecasts using selected method
    method <- switch(input$selected_model,
                     "ETS" = ETS(Consumption_GWh ~ error("A") + trend("N") + season("N")),
                     "ARIMA" = ARIMA(Consumption_GWh))
    calculate_forecasts(data_filtered$filtered_data, method, input$forecast_months)
  })
  
  # Output plot
  output$consumption_plot <- renderPlot({
    if (!is.null(forecasts())) {
      autoplot(forecasts())
    } else {
      plot(NULL, xlim = c(1, input$forecast_months), ylim = c(0, 1), 
           xlab = "Month", ylab = "Consumption (GWh)", 
           main = "Please click submit to generate forecast")
    }
  })
  
  # Output MAPE
  output$mape_output <- renderText({
    if (!is.null(forecasts())) {
      actual_data <- na.omit(data_filtered$filtered_data$consumption_GWh)
      forecast_data <- as.numeric(na.omit(forecasts()$point_forecast))
      if (length(actual_data) == 0 || length(forecast_data) == 0) {
        return("MAPE: N/A")
      }
      mape <- mean(abs((actual_data - forecast_data) / actual_data)) * 100
      paste("MAPE:", round(mape, 2), "%")
    } else {
      "MAPE: N/A"
    }
  })
  
  # Output MAE
  output$mae_output <- renderText({
    if (!is.null(forecasts())) {
      actual_data <- na.omit(data_filtered$filtered_data$consumption_GWh)
      forecast_data <- as.numeric(na.omit(forecasts()$point_forecast))
      if (length(actual_data) == 0 || length(forecast_data) == 0) {
        return("MAE: N/A")
      }
      mae <- mean(abs(actual_data - forecast_data))
      paste("MAE:", round(mae, 2))
    } else {
      "MAE: N/A"
    }
  })
  
  #Dwelling_Type_Plot
  
  filtered_data_1 <- reactive({
    data1 %>% filter(dwellingtype == input$dwelling)
  })
  
  ts_data <- reactive({
    xts(filtered_data_1()[,c("consumptionGWh")], order.by=as.Date(filtered_data_1()$tdate))
  })
  
  output$monthlyPlot <- renderPlotly({
    p <- ggplot(ts_data(), aes(x = Index, y = consumptionGWh)) + 
      geom_line() + theme_clean() +
      labs(title = "Monthly Household Electricity Consumption", caption = "Data from EMA") +
      xlab("Month-Year") +
      ylab("Consumption in GWh") +
      theme_ipsum_rc()
    ggplotly(p)
  })
  
  # Electricity_Generation_Plot
  generateion_filtered <- reactive({
    conversion_long %>% filter(Generation==input$Generation)
  })
  
  output$generation_plot <- renderPlotly({
    p2 <- ggplot(generateion_filtered(),
                 aes(x = Year, y=Amount_ratio))+
      geom_line()+theme_clean()+
      geom_smooth(method=input$fitting_method)+
      labs(title = "Yearly Electricity Generation", caption = "Data from EMA") +
      xlab("Year") +
      ylab("Generation in GWh")+
      theme_ipsum_rc()
    ggplotly(p2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

