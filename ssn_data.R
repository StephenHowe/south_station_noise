### South Station Noise Monitoring Data
### Stephen howe
### 18 February 2020
### Version 2

# Version history ####
# 20200218 V2: introduced feather; subsetted data prior creating all_readings files
# 20200204 V1: initial script

# libraries ####
library(feather)

# load data ####
df1 <- read.delim("data/20200118_to_20200122.txt", sep ="\t", stringsAsFactors = FALSE)
df2 <- read.delim("data/20200122_to_20200204.txt", sep ="\t", stringsAsFactors = FALSE)
df3 <- read.delim("data/20200204_to_20200206.txt", sep ="\t", stringsAsFactors = FALSE)
df4 <- read.delim("data/20200206_to_20200209.txt", sep ="\t", stringsAsFactors = FALSE)
df5 <- read.delim("data/20200209_to_20200212.txt", sep ="\t", stringsAsFactors = FALSE)
df6 <- read.delim("data/20200212_to_20200214.txt", sep ="\t", stringsAsFactors = FALSE)
df7 <- read.delim("data/20200214_to_20200219.txt", sep ="\t", stringsAsFactors = FALSE)
df8 <- read.delim("data/20200219_to_20200221.txt", sep ="\t", stringsAsFactors = FALSE)
df9 <- read.delim("data/20200221_to_20200224.txt", sep ="\t", stringsAsFactors = FALSE)
df10 <- read.delim("data/20200224_to_20200226.txt", sep ="\t", stringsAsFactors = FALSE)
df11 <- read.delim("data/20200226_to_20200228.txt", sep ="\t", stringsAsFactors = FALSE)
df12 <- read.delim("data/20200228_to_current.txt", sep ="\t", stringsAsFactors = FALSE)
df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

# remove data outside target time ranges
df$time_in_hours <- strptime(df$Time, format = "%H:%M:%S")
df$time_in_hours <- lubridate::hour(df$time_in_hours) + lubridate::minute(df$time_in_hours)/60 
df <- subset(df, df$time_in_hours < 6 | df$time_in_hours > 8) # remove morning window between 6AM - 8AM
df <- subset(df, df$time_in_hours < 17 | df$time_in_hours > 23) # remove evening window between 5PM - 11PM
df <- df[-6]

# write data to file ####
write_feather(df, "data/all_readings.feather")
write.csv(df, "data/all_readings.csv",
          row.names = FALSE,
          quote = TRUE)
