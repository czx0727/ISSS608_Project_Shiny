pacman::p_load(tidyverse,tmap,fable,tsibble,ggplot2,lubridate,feasts,plotly,xts,ggthemes,hrbrthemes,
               MASS,mgcv,DT,forecast,tseries,DT)

library(shiny)


mpsz_SES_filtered <- read_rds("data/mpsz_SES_filtered_1.rds")
conversion_long <- read_rds("data/conversion_long.rds")
consump_account <- read_rds("data/consump_account.rds")
housedata <- readRDS("data/helectricity.rds")
timeseries <- readRDS("data/helectricity_timeseries.rds")

# Define the MAPE function
MAPE <- function(actual, forecast) {
  mean(abs((actual - forecast) / actual), na.rm = TRUE) * 100
}

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
             fluidRow(
               column(2,
                      wellPanel(
                        selectInput(inputId = "Generation",
                                    label = "Electricity Generation:",
                                    choices=list("Inputs"="Inputs",
                                                 "Outputs"="Outputs",
                                                 "Conversion Rate"="Conversion_rate")),
                        selectInput(inputId = "fitting_method",
                                    label = "Fitting Method:",
                                    choices=list("lm"="lm",
                                                 "glm"="glm",
                                                 "loess"="loess",
                                                 "gam"="gam",
                                                 "rlm"="rlm")),
                        selectInput(inputId = "Sector",
                                    label = "Sector:",
                                    choices=list("Commerce and Services-related"="Commerce and Services-related",
                                                 "Industrial-related"="Industrial-related",
                                                 "Households"="Households",
                                                 "Others"="Others",
                                                 "Transport-Related"="Transport-Related"))
                      )
               ),
               column(5,plotlyOutput("generation_plot")),
               column(5,plotlyOutput("bubble_plot"))
             )
    ),
    tabPanel("By Region",
             sidebarLayout(
               sidebarPanel(
                 selectInput("dwellingtype", "Dwelling Type:", choices = unique(mpsz_SES_filtered$dwelling_type)),
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
                 selectInput("dwellingType", "Choose a Dwelling Type:",
                             choices = unique(timeseries$dwellingtype)), # Pre-populated with dwelling types from the dataset
                 numericInput("forecastMonths", "Months to Forecast:", 12, min = 1, step = 1),
                 selectInput("forecastModel", "Select Forecast Model:",
                             choices = c("Naive Model", "Seasonal Naive Model", "Simple Exponential Smoothing Model",
                                         "State Space Model", "Holt Winters’ Additive Seasonality Model",
                                         "Holt Winters’ Multiplicative Model", "ARIMA")),
                 actionButton("goButton", "Go")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Monthly Consumption", plotOutput("consumptionPlot")),
                   tabPanel("Decomposition", plotOutput("decompPlot")),
                   tabPanel("Forecast Table", DTOutput("forecastTable")),
                   tabPanel("Forecast Graph", plotOutput("forecastGraph")),
                   tabPanel("MAPE", verbatimTextOutput("mapeValue"))
                 )
               )
             )
    ),
    tabPanel("By Dwelling Type",
             fluidRow(
               column(4,
                      wellPanel(
                        selectInput("dwellingType1", "Dwelling Type 1",
                                    choices = unique(housedata$dwellingtype)),
                        selectInput("dwellingType2", "Dwelling Type 2",
                                    choices = unique(housedata$dwellingtype)),
                        selectInput("hyear", "Year",
                                    choices = unique(housedata$year))
                      ),
                      plotlyOutput("boxplot") # Plotly Boxplot output
               ),
               column(8,
                      plotOutput("cyclePlot1"),
                      plotOutput("cyclePlot2")
               )
             )
    )
  )
)
# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    filtered <- mpsz_SES_filtered
    if (!is.null(input$dwellingtype)) {
      filtered <- filtered %>% filter(dwelling_type == input$dwellingtype)
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
  
  # Tab 1 - Electricity_Generation_Plot
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
      theme_ipsum_rc()+
      theme(plot.title = element_text(size = 12),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 9))
    ggplotly(p2)
  })
  
  # Sector_plot
  # output$bubble_plot <- renderPlotly({
  #   
  #   years <- sort(unique(consump_account$Year))
  #   
  #   consump_account <- consump_account %>%
  #     filter(sector %in% input$Sector)
  #   
  #   axis_ranges <- consump_account %>% 
  #     group_by(sector) %>%
  #     summarise(
  #       x_min = min(Account_number, na.rm = TRUE),
  #       x_max = max(Account_number, na.rm = TRUE),
  #       y_min = 0,
  #       y_max = max(consumption, na.rm = TRUE)
  #     )
  #   
  #   plots <- split(consump_account, consump_account$sector) %>%
  #     imap(~ {
  #       data <- .x
  #       sector_name <- .y
  #       x_range <- c(axis_ranges %>% filter(sector == sector_name) %>% pull(x_min),
  #                    axis_ranges %>% filter(sector == sector_name) %>% pull(x_max))
  #       y_range <- c(axis_ranges %>% filter(sector == sector_name) %>% pull(y_min),
  #                    axis_ranges %>% filter(sector == sector_name) %>% pull(y_max))
  #       
  #       p <- plot_ly(data = data, 
  #                    x = ~Account_number, 
  #                    y = ~consumption, 
  #                    frame = ~year,
  #                    text=~paste("Sub_sector:",sub_sector, 
  #                                "<br>No. of Accounts:",Account_number,
  #                                "<br>Consumption:",consumption),
  #                    hoverinfo="text",
  #                    type = 'scatter', 
  #                    mode = 'markers',
  #                    color = ~sub_sector) %>%
  #         layout(showlegend=FALSE,
  #                hovermode="closest",
  #                title = list(text=paste("Electricity Consumption in", sector_name),
  #                             font = list(size=12)),
  #                xaxis = list(title = "Number of Accounts", range = x_range,
  #                             tickfont = list(size = 8)),
  #                yaxis = list(title = "Consumption(GWh)",range = y_range,
  #                             tickfont = list(size = 8)))
  #     })
  #   
  #   num_plots <- length(plots)
  #   
  #   final_plot <- subplot(plots, nrows = num_plots, 
  #                         shareX = FALSE, shareY = FALSE,
  #                         margin = 0.05) %>%
  #     animation_slider(frame = years, transition = list(duration = 0), 
  #                      currentvalue = list(prefix = "Year: "))
  #   
  #   final_plot})
  
  #Tab 3 - Timeseries
  # Filtered data based on user selection
  filteredData <- reactive({
    req(input$dwellingType)
    timeseries %>% 
      filter(dwellingtype == input$dwellingType)
  })
  
  # Plot for monthly consumption
  output$consumptionPlot <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = tdate, y = consumptionGWh)) +
      geom_line() +
      labs(title = "Monthly Electricity Consumption", x = "Date", y = "Consumption (GWh)")
  })
  
  # Decomposition plot
  output$decompPlot <- renderPlot({
    data <- filteredData()
    ts_data <- ts(data$consumptionGWh, frequency = 12) # Adjust frequency as needed
    decomposed <- stl(ts_data, s.window = "periodic")
    autoplot(decomposed)
  })
  
  # Forecast output and MAPE calculation
  observeEvent(input$goButton, {
    data <- filteredData()
    ts_data <- ts(data$consumptionGWh, start=c(year(min(data$tdate)), month(min(data$tdate))), frequency = 12) # Adjust start and frequency as needed
    forecast_length <- as.numeric(input$forecastMonths)
    
    model <- switch(input$forecastModel,
                    "Naive Model" = naive(ts_data, h = forecast_length),
                    "Seasonal Naive Model" = snaive(ts_data, h = forecast_length),
                    "Simple Exponential Smoothing Model" = ses(ts_data, h = forecast_length),
                    "State Space Model" = ets(ts_data),
                    "Holt Winters’ Additive Seasonality Model" = hw(ts_data, seasonal = "additive", h = forecast_length),
                    "Holt Winters’ Multiplicative Model" = hw(ts_data, seasonal = "multiplicative", h = forecast_length),
                    "ARIMA" = auto.arima(ts_data))
    
    forecast_result <- forecast(model, h = forecast_length)
    output$forecastTable <- renderDT({
      data.frame(Time = as.Date(time(forecast_result$mean)),
                 Forecast = as.numeric(forecast_result$mean))
    })
    
    # Assuming the last 'forecast_length' observations in 'df' match the forecast period
    actual_values <- tail(data$consumptionGWh, forecast_length) # Extract actual observed values for the forecast period
    
    mape_value <- MAPE(actual_values, forecast_result$mean)
    
    output$mapeValue <- renderText({
      paste("MAPE:", round(mape_value, 2), "%")
    })
    
    # Render forecast graph
    output$forecastGraph <- renderPlot({
      autoplot(forecast_result) +
        ggtitle(paste(input$forecastModel, input$dwellingType, "Forecast for Monthly Household Electricity Consumption")) +
        xlab("Time") + 
        ylab("Consumption (GWh)") + 
        theme_light()
    })
  })
  
  #Tab 4 - Dwelling_Type_Plot
  # Generate the boxplot
  filteredDataBox <- reactive({
    housedata %>% 
      filter(dwellingtype %in% c(input$dwellingType1, input$dwellingType2), 
             year == input$hyear)
  })
  
  output$boxplot <- renderPlotly({
    ggplotly(
      ggplot(filteredDataBox(), aes(x = dwellingtype, y = consumptiongwh, fill = dwellingtype)) +
        geom_boxplot() +
        theme_minimal() +
        labs(x = "Dwelling Type", y = "Consumption (GWh)", title = paste(input$hyear, "Box Plot of", input$dwellingType1, "&", input$dwellingType2)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none", plot.title = element_text(size = 10, face = "bold"))
    )
  })
  
  # Generate the cycle plots
  cycleData1 <- reactive({
    df <- housedata %>%
      filter(dwellingtype == input$dwellingType1)
    df$month <- factor(df$month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
    return(df)
  })
  
  
  cycleData2 <- reactive({
    df <- housedata %>%
      filter(dwellingtype == input$dwellingType2)
    df$month <- factor(df$month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
    return(df)
  })
  
  
  hlineData1 <- reactive({
    cycleData1() %>%
      group_by(month) %>%
      summarise(avgvalue = mean(consumptiongwh, na.rm = TRUE))
  })
  
  hlineData2 <- reactive({
    cycleData2() %>%
      group_by(month) %>%
      summarise(avgvalue = mean(consumptiongwh, na.rm = TRUE))
  })
  
  output$cyclePlot1 <- renderPlot({
    df_filtered <- cycleData1()
    hline_mean_consumption.data <- hlineData1()
    
    ggplot(df_filtered, aes(x = year, y = consumptiongwh, group = month, color = month)) +
      geom_line() +
      geom_hline(data = hline_mean_consumption.data, aes(yintercept = avgvalue), 
                 linetype = 6, color = "red", size = 0.5) +
      facet_wrap(~month, scales = "free_x") +
      labs(title = paste("Cycle Plot for", input$dwellingType1)) +
      xlab("") +
      ylab("Consumption (GWh)") +
      theme_tufte(base_family = "Helvetica") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7), plot.title = element_text(size = 15, face = "bold"))
  })
  
  output$cyclePlot2 <- renderPlot({
    df_filtered <- cycleData2()
    hline_mean_consumption.data <- hlineData2()
    
    ggplot(df_filtered, aes(x = year, y = consumptiongwh, group = month, color = month)) +
      geom_line() +
      geom_hline(data = hline_mean_consumption.data, aes(yintercept = avgvalue), 
                 linetype = 6, color = "red", size = 0.5) +
      facet_wrap(~month, scales = "free_x") +
      labs(title = paste("Cycle Plot for", input$dwellingType2)) +
      xlab("") +
      ylab("Consumption (GWh)") +
      theme_tufte(base_family = "Helvetica") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7), plot.title = element_text(size = 15, face = "bold"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)