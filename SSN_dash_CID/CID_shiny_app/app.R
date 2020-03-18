### 717AA Outdoor Noise Monitoring
### Stephen Howe
### 18 March 2020
### Version 2

### Version Information ####
# 20200318 V2: interactive graphs, simplified logic to pull file; tz adjustment
# 20200317 V1: initial version

# libraries ####
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinyEventLogger)
library(lubridate)
library(aws.s3)
library(reshape2)
library(plotly)

# configurations ####
# initialize logging
set_logging(file=TRUE)

# source AWS configs
source("config.R")

ui <- dashboardPage(skin = "yellow",
                    
                    # set dashboard header ####
                    dashboardHeader(title = "Atlantic Avenue Noise"),
                    
                    #hide sidebar menu ####
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Noise Readings", tabName = "noise_readings", icon = icon("chart-area")),
                        selectInput("measurements", 
                                    "Select Metrics for Graph", 
                                    choices = c("LAFmin", "LAFmax", "LAeq", "LAF01", "LAF10", "LAF50", "LAF90", "LAF99"),
                                    selected = c("LAFmin", "LAFmax", "LAeq"),
                                    multiple = TRUE),
                        dateInput("date2", "Select Date (yyyy-mm-dd):"),
                        menuItem("Information", tabName = "information", icon = icon("info-circle"))
                      )
                    ),
                    
                    #create main body panels ####
                    dashboardBody(
                      
                      # hide error messages in UI
                      # TODO find a better way
                      # tags$style(type="text/css",
                      #            ".shiny-output-error { visibility: hidden; }",
                      #            ".shiny-output-error:before { visibility: hidden; }"
                      # ),
                      
                      tabItems(
                        # noise readings body panel ####
                        tabItem(tabName = "noise_readings",
                                fluidRow(
                                  box(title = "Noise Readings for Selected Day",
                                      width = 12,
                                      plotlyOutput("plt_noise_readings")
                                      )
                                  )
                                ),
                        
                        # information body ponel ####
                        tabItem(tabName = "information",
                                fluidRow(
                                  box(title = "Hello World"))
                                )
                        ) # end of tab items
                    ) # end of dashboard body
) # end of UI

# Define server logic ----
server <- function(input, output, session) {
  # initialize event logging
  set_logging_session()
  
  output$plt_noise_readings <- renderPlotly({
    # create file name based on selected date
    # FUTURE: substitue hardcoded device ID with user input
    selected_file <- paste("SSND_", 
                           "1453", 
                           "_",
                           substring(input$date2, 1,4), 
                           "_",
                           substring(input$date2, 6,7), 
                           "_",
                           substring(input$date2, 9, 10),
                           ".csv",
                           sep = "")
    
    # pull file from S3
    obj <- get_object(object =  selected_file, bucket = "acoustic-data-repository")
    df <- read.csv(text = rawToChar(obj))
    
    # format Time variable
    df$Time <- as_datetime(df$Time) # recast Time variable as datetime
    attr(df$Time, "tzone") <- "America/New_York"
    
    # melt it
    df_melted <- melt(df, id.vars = "Time", variable.name = "Measure")
    
    # subset data based on user inputs
    df_subset <- subset(df_melted, df_melted$Measure %in% input$measurements)
    
    # plot
    p1 <- ggplot(df_subset, aes(Time, value, color = Measure)) +
      geom_line() +
      labs(title = "Outside Sounds Readings 717 Atlantic Avenue - 7th Floor",
           y = "Decibels") +
      scale_x_datetime(date_labels = "%b %d %H:%M")
    
    ggplotly(p1)
  })
  
  output$test <- renderText({
    input$measurements
  })
  
  # log session ####
  log_event("Dashboard session started")
  
} #end of server functions

### Run the application ####
shinyApp(ui = ui, server = server)
                      


#TODO
# remove file list function [DONE]
# replace with just constructing the file name from select [DONE]
# make default graph display based on URL parameter, otherwise default to today
# change time zone to local [DONE]
# add in selectables for various measurements

