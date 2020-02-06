### South Station Noise Monitoring Data
### Stephen howe
### 4 February 2020
### Version 1

# Version history ####
# 20200204 V1: initial script

# libraries ####
#library(RCurl)

# load data ####
df1 <- read.delim("data/20200118_to_20200122.txt", sep ="\t", stringsAsFactors = FALSE)
df2 <- read.delim("data/20200122_to_20200204.txt", sep ="\t", stringsAsFactors = FALSE)
df3 <- read.delim("data/20200204_to_current.txt", sep ="\t", stringsAsFactors = FALSE)
df <- rbind(df1, df2, df3)

# # clean data, create new variables
# df$Date <- as.Date(df$Date, format = "%m/%d/%y")
# df$dateTime <- as.character(paste(df$Date, df$Time, sep = " "))
# df$dateTime <- strptime(df$dateTime, format = "%Y-%m-%d %H:%M:%S")  # may have to fiddle with this€€‹
# df$dateTime <- as.POSIXct(df$dateTime)
# df$Value <- as.numeric(df$Value)
# df$col <- cut(df$Value, c(30,50,70,130))
# df$legal_limits <- ifelse(df$col == "(30,50]",
#                           "Acceptable Level",
#                           ifelse(df$col == "(50,70]",
#                                  "Exceeds Nighttime Limit",
#                                  "Exceeds Daytime Limit"))
# df$time_in_hours <- lubridate::hour(df$dateTime) + lubridate::minute(df$dateTime)/60 
# 
# # subset into day and night
# dfn <- subset(df, df$time_in_hours < 6 | df$time_in_hours > 23)
# dfd <- subset(df, df$time_in_hours > 8 & df$time_in_hours < 17)

# write data to file ####
#saveRDS(dfn, "data/nighttime_readings")
write.csv(df, "data/all_readings.csv",
          row.names = FALSE,
          quote = TRUE)
