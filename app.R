### South Station Noise Monitoring
### Stephen Howe
### 4 February 2020
### Version 4

### Version Information ####
# 20200204 V4: added daytime readings panel
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
                        menuItem("Nighttime Readings", tabName = "nighttime_readings"),
                        menuItem("Daytime Readings", tabName = "daytime_readings"),
                        menuItem("Information", tabName = "information")
                      )
                    ),
                    
                    #create main body panels ####
                    dashboardBody(
                      
                      
                      # hide error messages in UI
                      # TODO find a better way
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      
                      tabItems(
                        # nighttime readings body panel ####
                        tabItem(tabName = "nighttime_readings",
                                fluidRow(
                                  box(title = "Lastest Reading",
                                      width = 6,
                                      plotOutput("plt_ultimate_night_readings")),
                                  
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
                                      selectInput("date_list", label = "", choices = "", width = "250px")
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
                                ), # end of nighttime readings tab item
                        
                        # daytime readings body panel ####
                        tabItem(tabName = "daytime_readings",
                                fluidRow(
                                  box(title = "Lastest Reading",
                                      width = 6,
                                      plotOutput("plt_ultimate_day_readings")),
                                  
                                  box(title = "Comparison of Latest Reading to Baseline",
                                      width = 6,
                                      plotOutput("plt_comparison_day"))
                                ),
                                
                                # divider
                                hr(style="border-color: black;"),
                                
                                # noise reading for selected date
                                fluidRow(
                                  box(title = "Select a Date",
                                      width = 4, 
                                      height = "125px",
                                      selectInput("date_list_day", label = "", choices = "", width = "250px")
                                  ),
                                  box(title = "Instructions",
                                      width = 8, 
                                      height = "125px",
                                      "Select a specific date from the drop-down menu to the left. The plots below will show the noise readings for that selected date as well as comparisons values to the baseline. The date you select indicates the date for the start of the day at 8AM.")
                                ),
                                
                                fluidRow(
                                  box(title = "Noise Reading for Specific Date",
                                      plotOutput("plt_nr_selected_day")
                                  ),
                                  box(title = "Comparison of Specific Date to Baseline",
                                      width = 6,
                                      plotOutput("plt_comp_selected_day"))
                                )
                        ), # end of dayttime readings tab item
                        
                        # information body panel ####
                        tabItem(tabName = "information",
                                fluidRow(
                                  box(title = "Overview",
                                      "This dashboard was created to monitor the noise resulting from the South Station Air Rights construction project. It is anticipated that noise levels will exceed the legal limits set by the City of Boston. This dashboard displays measurements of noise levels (dB) taken directly across Atlantic Avenue from the construction site. Since the greatest concern with noise lies with the planned night-time construction, this dashboard displays decibel readings taken between 11PM and 6AM."),
                                  box(title="Methodology",
                                      "The data for this dashboard was recorded using a Reed Instruments SD-4023 Sound Level Meter datalogger. Meausurements were taken every five seconds between 11PM and 6AM each night. The decibel meter is positioned in a residential building directly across from South Station. The monitor was placed direcly inside the window of a residential unit in order to best measure the sound entering residents' living space.")
                                ),
                                
                                fluidRow(
                                  box(title = "Information",
                                      "App Version: 4 | February 4, 2020 | Created by howetowork at gmail dot com")
                                )
                        ) # end of information tab item
                      ) # end of tab items
                    ) # end of body
) # end of page


# Define server logic ----
server <- function(input, output, session) {
  # initialize event logging
  set_logging_session()
  
  # data ####
  df1 <- read.delim("data/20200118_to_20200122.txt", sep ="\t", stringsAsFactors = FALSE)
  df2 <- read.delim("data/20200122_to_20200204.txt", sep ="\t", stringsAsFactors = FALSE)
  df3 <- read.delim("data/20200204_to_current.txt", sep ="\t", stringsAsFactors = FALSE)
  df <- rbind(df1, df2, df3)
  
  #TODO change df assignment to a readRDS on git; move df1, 2, 3 to separate script; move all the clean-up down below, too
  
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
  dfn <- subset(df, df$time_in_hours < 6 | df$time_in_hours > 23)
  dfd <- subset(df, df$time_in_hours > 8 & df$time_in_hours < 17)
  
  #TODO stop here for data file
  
  # nighttime readings ####
  
  # determine latest reading (day)
  ultimate_date <- max(dfn$Date)
  penultimate_date <- ultimate_date - 1
  
  # add comparison label
  dfn$comparison <- ifelse(dfn$dateTime > paste(penultimate_date, "23:00:00", sep = " ") & dfn$dateTime < paste(ultimate_date, "06:00:00", sep = " "), "latest", "baseline")
  
  # data for latest evening reading
  df_ultimate <- subset(dfn, dfn$dateTime > paste(penultimate_date, "23:00:00", sep = " ") & dfn$dateTime < paste(ultimate_date, "06:00:00", sep = " "))
  
  # plot for last night's reading ####
  output$plt_ultimate_night_readings <- renderPlot({
    ggplot(df_ultimate, aes(dateTime, Value, color = legal_limits)) +
      geom_point() +
      scale_colour_manual(name = "Legal Limits", values = c("Acceptable Level" = "dark blue", 
                                                            "Exceeds Nighttime Limit" = "orange",
                                                            "Exceeds Daytime Limit" = "red")) +
      labs(title = "Noise Monitoring of South Station (Atlantic Ave) During the Night (11PM -  6AM)",
           subtitle = paste("Evening of", penultimate_date, "to", ultimate_date, sep = " "),
           x = "Time",
           y = "Noise Level (dB)") +
      theme(legend.position = "bottom")
  })
  
  # combined comparison plots
  output$plt_comparison <- renderPlot({
    pa <- ggplot(dfn, aes(x = "", y = Value, fill = comparison)) +
      geom_boxplot() +
      labs(title = "Boxplot of Noise Readings", y = "Noise Level (dB)", fill="Group") +
      theme(legend.position = "bottom")
    
    pb <- ggplot(dfn, aes(Value, fill = comparison)) +
      geom_density(alpha = 0.5) +
      labs(title = "Distribution of Noise Readings", x = "Noise Reading (dB)", y = "Frequency (Percent)", fill="Group") +
      theme(legend.position = "bottom")
    
    grid.arrange(pa, pb, nrow = 1)
  })
  
  # Select a date ####
  # define list of dates for drop-down
  dates <- unique(dfn$Date)
  dates <- sort(dates, decreasing = TRUE)
  dates <- dates[-1]
  
  updateSelectInput(session, "date_list", choices = dates)
  
  # plots for selected date
  output$plt_nr_selected <- renderPlot({
    
    selected_start <- as.Date(input$date_list)
    selected_end <- selected_start + 1
    
    df_selected <- subset(dfn, dfn$dateTime > paste(selected_start, "23:00:00", sep = " ") & dfn$dateTime < paste(selected_end, "06:00:00", sep = " "))
    
    ggplot(df_selected, aes(dateTime, Value, color = legal_limits)) +
      geom_point() +
      scale_colour_manual(name = "Legal Limits", values = c("Acceptable Level" = "dark blue", 
                                                            "Exceeds Nighttime Limit" = "orange",
                                                            "Exceeds Daytime Limit" = "red")) +
      labs(title = "Noise Monitoring of South Station (Atlantic Ave) During the Night (11PM -  6AM)",
           subtitle = paste("Evening of", selected_start, "to", selected_end, sep = " "),
           x = "Time",
           y = "Noise Level (dB)") +
      theme(legend.position = "bottom")
  })
  
  output$plt_comp_selected <- renderPlot({
    selected_start <- as.Date(input$date_list)
    selected_end <- selected_start + 1
    
    dfn$selected <- ifelse(dfn$dateTime > paste(selected_start, "23:00:00", sep = " ") & dfn$dateTime < paste(selected_end, "06:00:00", sep = " "), "selected", "baseline")
    
    
    p3 <- ggplot(dfn, aes(x = "", y = Value, fill = selected)) +
      geom_boxplot() +
      labs(title = "Boxplot of Noise Readings", y = "Noise Level (dB)", fill="Group") +
      theme(legend.position = "bottom")
    
    p4 <- ggplot(dfn, aes(Value, fill = selected)) +
      geom_density(alpha = 0.5) +
      labs(title = "Distribution of Noise Readings", x = "Noise Reading (dB)", y = "Frequency (Percent)", fill="Group") +
      theme(legend.position = "bottom")
    
    grid.arrange(p3, p4, nrow = 1)
  })
  
  # daytime readings ####
  
  # add comparison label
  dfd$comparison <- ifelse(dfd$dateTime > paste(ultimate_date, "08:00:00", sep = " ") & dfd$dateTime < paste(ultimate_date, "17:00:00", sep = " "), "latest", "baseline")
  
  # data for latest evening reading
  dfd_ultimate <- subset(dfd, dfd$dateTime > paste(ultimate_date, "08:00:00", sep = " ") & dfd$dateTime < paste(ultimate_date, "17:00:00", sep = " "))
  
  # plot for last days's reading ####
  output$plt_ultimate_day_readings <- renderPlot({
    ggplot(dfd_ultimate, aes(dateTime, Value, color = legal_limits)) +
      geom_point() +
      scale_colour_manual(name = "Legal Limits", values = c("Acceptable Level" = "dark blue", 
                                                            "Exceeds Nighttime Limit" = "light blue",
                                                            "Exceeds Daytime Limit" = "orange")) +
      labs(title = "Noise Monitoring of South Station (Atlantic Ave) During the Day (8AM - 5PM)",
           subtitle = paste("Day of", penultimate_date, sep = " "),
           x = "Time",
           y = "Noise Level (dB)") +
      theme(legend.position = "bottom")
  })
  
  # combined comparison plots
  output$plt_comparison_day <- renderPlot({
    pc <- ggplot(dfd, aes(x = "", y = Value, fill = comparison)) +
      geom_boxplot() +
      labs(title = "Boxplot of Noise Readings", y = "Noise Level (dB)", fill="Group") +
      theme(legend.position = "bottom")
    
    pd <- ggplot(dfd, aes(Value, fill = comparison)) +
      geom_density(alpha = 0.5) +
      labs(title = "Distribution of Noise Readings", x = "Noise Reading (dB)", y = "Frequency (Percent)", fill="Group") +
      theme(legend.position = "bottom")
    
    grid.arrange(pc, pd, nrow = 1)
  })
  
  # Select a date ####
  # define list of dates for drop-down
  dates_day <- unique(dfd$Date)
  dates_day <- sort(dates_day, decreasing = TRUE)
  #dates_day <- dates_day[-1]
  
  updateSelectInput(session, "date_list_day", choices = dates_day)
  
  # plots for selected date
  output$plt_nr_selected_day <- renderPlot({
    
    selected_start_day <- as.Date(input$date_list_day)
    #selected_end_day <- selected_start + 1
    
    df_selected_day <- subset(dfd, dfd$dateTime > paste(selected_start_day, "08:00:00", sep = " ") & dfd$dateTime < paste(selected_start_day, "17:00:00", sep = " "))
    
    ggplot(df_selected_day, aes(dateTime, Value, color = legal_limits)) +
      geom_point() +
      scale_colour_manual(name = "Legal Limits", values = c("Acceptable Level" = "dark blue", 
                                                            "Exceeds Nighttime Limit" = "light blue",
                                                            "Exceeds Daytime Limit" = "orange")) +
      labs(title = "Noise Monitoring of South Station (Atlantic Ave) During the Day (8AM - 5PM)",
           subtitle = paste("Day of", selected_start_day, sep = " "),
           x = "Time",
           y = "Noise Level (dB)") +
      theme(legend.position = "bottom")
  })
  
  output$plt_comp_selected_day <- renderPlot({
    selected_start_day <- as.Date(input$date_list_day)
    #selected_end_day <- selected_start + 1
    
    dfd$selected <- ifelse(dfd$dateTime > paste(selected_start_day, "08:00:00", sep = " ") & dfd$dateTime < paste(selected_start_day, "17:00:00", sep = " "), "selected", "baseline")
    
    
    p5 <- ggplot(dfd, aes(x = "", y = Value, fill = selected)) +
      geom_boxplot() +
      labs(title = "Boxplot of Noise Readings", y = "Noise Level (dB)", fill="Group") +
      theme(legend.position = "bottom")
    
    p6 <- ggplot(dfd, aes(Value, fill = selected)) +
      geom_density(alpha = 0.5) +
      labs(title = "Distribution of Noise Readings", x = "Noise Reading (dB)", y = "Frequency (Percent)", fill="Group") +
      theme(legend.position = "bottom")
    
    grid.arrange(p5, p6, nrow = 1)
  })
  
  
  # log session ####
  log_event("Dashboard session started")
  
} #end of server functions

### Run the application ####
shinyApp(ui = ui, server = server)

