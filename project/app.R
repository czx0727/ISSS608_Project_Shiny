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
library(leaflet)

mpsz_SES_filtered <- read_rds("data/mpsz_SES_filtered_1.rds")
conversion_long <- read_rds("data/conversion_long.rds")
housedata <- readRDS("data/helectricity.rds")

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
                                          "Conversion Rate"="Conversion_rate")),
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
             fluidRow(
               column(4,
                      wellPanel(
                        selectInput("dwellingType1", "Dwelling Type 1",
                                    choices = unique(housedata$dwellingtype)),
                        selectInput("dwellingType2", "Dwelling Type 2",
                                    choices = unique(housedata$dwellingtype)),
                        selectInput("year", "Year",
                                    choices = unique(housedata$year))
                      ),
                      plotlyOutput("boxplot") # Plotly Boxplot output
               ),
               column(8,
                      plotOutput("cyclePlot1"),
                      plotOutput("cyclePlot2")
               )
             )
    ),
    tabPanel("By Region",
             sidebarLayout(
               sidebarPanel(
                 selectInput("dwelling_type", "Dwelling Type:", choices = unique(mpsz_SES_filtered$dwelling_type)),
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
                 
               ),
               mainPanel(
                 
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
              style = input$classification,
              palette = input$colour,
              title = "Electricity Consumption by Percentile") +
      tm_borders() +
      tm_scale_bar() +
      tm_grid()
    
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
      theme_ipsum_rc()
    ggplotly(p2)
  })
  
  #Tab 2 - Dwelling_Type_Plot
  # Generate the boxplot
  filteredData <- reactive({
    housedata %>% 
      filter(dwellingtype %in% c(input$dwellingType1, input$dwellingType2), 
             year == input$year)
  })
  
  output$boxplot <- renderPlotly({
    ggplotly(
      ggplot(filteredData(), aes(x = dwellingtype, y = consumptiongwh, fill = dwellingtype)) +
        geom_boxplot() +
        theme_minimal() +
        labs(x = "Dwelling Type", y = "Consumption (GWh)", title = paste(input$year, "Box Plot of", input$dwellingType1, "&", input$dwellingType2)) +
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

