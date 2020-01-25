### South Station Noise Monitoring
### Stephen Howe
### 25 January 2020
### Version 3

### Version Information ####
# 20200125 V3: added plots based on selected date
# 20200123 V2: added comparison plots
# 20200120 V1: initial version

# libraries ####
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinyEventLogger)
library(lubridate)
library(gridExtra)

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
                                      width = 6,
                                      plotOutput("plt_ultimate_night_readings")),
                                  
                                  # box(title = "Boxplot of Noise Readings",
                                  #     width = 4,
                                  #     plotOutput("plt_boxplot_all")),
                                  # 
                                  # box(title = "Distribution of Noise Readings",
                                  #     width = 4,
                                  #     plotOutput("plt_density_comparison")),
                                  
                                  box(title = "Comparison of Latest Reading to Baseline",
                                      width = 6,
                                      plotOutput("plt_comparison"))
                                  ),
                                
                                # divider
                                hr(style="border-color: black;"),
                                
                                # noise reading for selected date
                                fluidRow(
                                  box(title = "Select a Date",
                                      width = 4, 
                                      height = "125px",
                                      selectInput("date_selection", label = "", choices = dates, width = "250px")
                                  ),
                                  box(title = "Instructions",
                                      width = 8, 
                                      height = "125px",
                                      "Select a specific date from the drop-down menu to the left. The plots below will show the noise readings for that selected date as well as comparisons values to the baseline. The date you select indicates the date for the start of the night at 11PM.")
                                ),
                                
                                fluidRow(
                                  box(title = "Noise Reading for Specific Date",
                                      plotOutput("plt_nr_selected")
                                      ),
                                  box(title = "Comparison of Specific Date to Baseline",
                                      width = 6,
                                      plotOutput("plt_comp_selected"))
                                  )
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
  df1 <- read.delim("data/20200118_to_20200122.txt", sep ="\t", stringsAsFactors = FALSE)
  df2 <- read.delim("data/20200122_to_current.txt", sep ="\t", stringsAsFactors = FALSE)
  df <- rbind(df1, df2)
  
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
  df$time_in_hours <- lubridate::hour(df$dateTime) + lubridate::minute(df$dateTime)/60 
  
  # filter out readings from before 11 and after 6
  df <- subset(df, df$time_in_hours < 6 | df$time_in_hours > 23)
  
  # determine latest reading (day)
  ultimate_date <- max(df$Date)
  penultimate_date <- ultimate_date - 1
  
  # add comparison label
  df$comparison <- ifelse(df$dateTime > paste(penultimate_date, "23:00:00", sep = " ") & df$dateTime < paste(ultimate_date, "06:00:00", sep = " "), "latest", "baseline")
  
  # data for latest evening reading
  df_ultimate <- subset(df, df$dateTime > paste(penultimate_date, "23:00:00", sep = " ") & df$dateTime < paste(ultimate_date, "06:00:00", sep = " "))
  
  # plot for last night's reading ####
  output$plt_ultimate_night_readings <- renderPlot({
    ggplot(df_ultimate, aes(dateTime, Value, color = legal_limits)) +
      geom_point() +
      scale_colour_manual(name = "Legal Limits", values = c("Acceptable Level" = "dark blue", 
                                                            "Exceeds Nighttime Limit" = "orange",
                                                            "Exceeds Daytime Limit" = "red")) +
      labs(title = "Noise Monitoring of South Station (Atlantic Avenue) During the Night (11PM -  6AM)",
           subtitle = paste("Evening of", penultimate_date, "to", ultimate_date, sep = " "),
           x = "Time",
           y = "Noise Level (dB)") +
      theme(legend.position = "bottom")
  })
  
  # # boxplot of all readings ####
  # output$plt_boxplot_all <- renderPlot({
  #   ggplot(df, aes(x = "", y = Value, fill = comparison)) +
  #     geom_boxplot() +
  #     labs(title = "Comparison of All Nights (Baseline) and Latest Reading", y = "Noise Level (dB)", fill="Group") +
  #     theme(legend.position = "bottom")
  # })
  # 
  # # density plot ####
  # output$plt_density_comparison <- renderPlot({
  #   ggplot(df, aes(Value, fill = comparison)) +
  #     geom_density(alpha = 0.5) +
  #     labs(title = "Distribution of Noise Readings", x = "Noise Reading (dB)", y = "Frequency (Percent)", fill="Group")
  # })
  
  # combined comparison plots
  output$plt_comparison <- renderPlot({
    pa <- ggplot(df, aes(x = "", y = Value, fill = comparison)) +
      geom_boxplot() +
      labs(title = "Boxplot of Noise Readings", y = "Noise Level (dB)", fill="Group") +
      theme(legend.position = "bottom")
    
    pb <- ggplot(df, aes(Value, fill = comparison)) +
      geom_density(alpha = 0.5) +
      labs(title = "Distribution of Noise Readings", x = "Noise Reading (dB)", y = "Frequency (Percent)", fill="Group") +
      theme(legend.position = "bottom")
    
    grid.arrange(pa, pb, nrow = 1)
  })
  
  # Select a date ####
  # define list of dates for drop-down
  dates <- unique(df$Date)
  dates <- sort(dates, decreasing = TRUE)
  dates <- dates[-1]
  
  # plots for selected date
  output$plt_nr_selected <- renderPlot({
    selected_start <- as.Date(input$date_selection)
    selected_end <- selected_start + 1
    
    df_selected <- subset(df, df$dateTime > paste(selected_start, "23:00:00", sep = " ") & df$dateTime < paste(selected_end, "06:00:00", sep = " "))
    
    ggplot(df_selected, aes(dateTime, Value, color = legal_limits)) +
      geom_point() +
      scale_colour_manual(name = "Legal Limits", values = c("Acceptable Level" = "dark blue", 
                                                            "Exceeds Nighttime Limit" = "orange",
                                                            "Exceeds Daytime Limit" = "red")) +
      labs(title = "Noise Monitoring of South Station (Atlantic Avenue) During the Night (11PM -  6AM)",
           subtitle = paste("Evening of", selected_start, "to", selected_end, sep = " "),
           x = "Time",
           y = "Noise Level (dB)") +
      theme(legend.position = "bottom")
  })
  
  output$plt_comp_selected <- renderPlot({
    selected_start <- as.Date(input$date_selection)
    selected_end <- selected_start + 1
    
    df$selected <- ifelse(df$dateTime > paste(selected_start, "23:00:00", sep = " ") & df$dateTime < paste(selected_end, "06:00:00", sep = " "), "selected", "baseline")
    
    
    p3 <- ggplot(df, aes(x = "", y = Value, fill = selected)) +
      geom_boxplot() +
      labs(title = "Boxplot of Noise Readings", y = "Noise Level (dB)", fill="Group") +
      theme(legend.position = "bottom")
    
    p4 <- ggplot(df, aes(Value, fill = selected)) +
      geom_density(alpha = 0.5) +
      labs(title = "Distribution of Noise Readings", x = "Noise Reading (dB)", y = "Frequency (Percent)", fill="Group") +
      theme(legend.position = "bottom")
    
    grid.arrange(p3, p4, nrow = 1)
  })
  
  # log session ####
  log_event("Dashboard session started")
  
} #end of server functions

### Run the application ####
shinyApp(ui = ui, server = server)

