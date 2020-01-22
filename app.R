### South Station Noise Monitoring
### Stephen Howe
### 21 January 2020
### Version 3

### Version Information ####
# 20200120 V1: initial version

# libraries ####
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinyEventLogger)

# configurations ####
# initialize logging
set_logging(file=TRUE)

# Define UI for application ----
ui <- dashboardPage(skin = "red",
                    # set dashboard header ####
                    dashboardHeader(title = "South Station Noise"),
                    
                    #hide sidebar menu ####
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Noise Readings", tabName = "noise_readings"),
                        menuItem("Information", tabName = "information")
                      )
                    ),
                    
                    #create main body panels ####
                    dashboardBody(
                      
                      tabItems(
                        # noise readings body panel ####
                        tabItem(tabName = "noise_readings",
                                fluidRow(
                                  box(title = "Lastest Reading",
                                      width = 8,
                                      plotOutput("plt_ultimate_night_readings"))
                                  ),
                                ), # end of noise readings tab item
                        
                        # information body panel ####
                        tabItem(tabName = "information",
                                fluidRow(
                                  box(title = "Overview",
                                      "This dashboard was created to monitor the noise resulting from the South Station Air Rights construction project. It is anticipated that noise levels will exceed the legal limits set by the City of Boston. This dashboard displays measurements of noise levels (dB) taken directly across Atlantic Avenue from the construction site. Since the greatest concern with noise lies with the planned night-time construction, this dashboard displays decibel readings taken between 11PM and 6AM."),
                                  box(title="Methodology",
                                      "The data for this dashboard was recorded using a Reed Instruments SD-4023 Sound Level Meter datalogger. Meausurements were taken every five seconds between 11PM and 6AM each night. The decibel meter is positioned in a residential building directly across from South Station. The monitor was placed direcly inside the window of a residential unit in order to best measure the sound entering residents' living space.")
                                )
                        ) # end of information tab item
                      ) # end of tab items
                    ) # end of body
) # end of page


# Define server logic ----
server <- function(input, output) {
  # initialize event logging
  set_logging_session()
  
  # data ####
  df <- read.delim("data/20200118_to_current.XLS", sep ="\t")
  #df <- subset(df, df$Unit != 'Unit')  # remove extraneous header rows
  
  # clean data, create new variables
  df$Date <- as.Date(df$Date, format = "%m/%d/%y")
  df$dateTime <- as.character(paste(df$Date, df$Time, sep = " "))
  df$dateTime <- strptime(df$dateTime, format = "%Y-%m-%d %H:%M:%S")  # may have to fiddle with this
  df$dateTime <- as.POSIXct(df$dateTime)
  df$Value <- as.numeric(df$Value)
  df$col <- cut(df$Value, c(30,50,70,130))
  df$legal_limits <- ifelse(df$col == "(30,50]",
                            "Acceptable Level",
                            ifelse(df$col == "(50,70]",
                                   "Exceeds Nighttime Limit",
                                   "Exceeds Daytime Limit"))
  
  
  # determine latest reading (day)
  ultimate_date <- max(df$Date)
  penultimate_date <- ultimate_date - 1
  
  # plot for last night's reading ####
  output$plt_ultimate_night_readings <- renderPlot({
    df_ultimate <- subset(df, df$dateTime > paste(penultimate_date, "23:00:00", sep = " ") & df$dateTime < paste(ultimate_date, "06:00:00", sep = " "))
    
    ggplot(df_subset, aes(dateTime, Value, color = legal_limits)) +
      geom_point() +
      scale_colour_manual(name = "Legal Limits", values = c("Acceptable Level" = "dark blue", 
                                                            "Exceeds Nighttime Limit" = "orange",
                                                            "Exceeds Daytime Limit" = "red")) +
      labs(title = "Noise Monitoring of South Station (Atlantic Avenue) During the Night (11PM -  6AM)",
           subtitle = paste("Evening of", penultimate_date, "to", ultimate_date, sep = " "),
           x = "Time",
           y = "Noise Level (dB)")
  })
  
  # boxplot of all readings ####
  output$plt_boxplot_all <- renderPlot({
    ggplot(df, aes(x = "", y = Value)) +
      geom_boxplot(fill = "sky blue") +
      labs(title = "Boxplot of All Noise Readings, Pre-Construction", x = "Baseline Days", y = "Noise Level (dB)")
  })
  
  # log sesseion ####
  log_event("Dashboard session started")
  
} #end of server functions

### Run the application ####
shinyApp(ui = ui, server = server)

