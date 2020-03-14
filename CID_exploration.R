### Readings from Convergence Instruments Meter

library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)

# data ####
df <- read.csv("data/CID/CID_20200314.csv", stringsAsFactors = FALSE)

str(df)
colnames(df) <- c("Date", "LEQ", "Lmin", "L10", "Lmax", "L90", "L50", "X")
df <- df[1:7]

df$Date <- as_datetime(df$Date)
str(df)
df_melted <- melt(df, id.vars = "Date", variable.name = "Measure")

# plot
ggplot(df_melted, aes(Date, value, color = Measure)) +
  geom_line() +
  labs(title = "Outside Sounds Readings 717 Atlantic Avenue - 7th Floor",
       y = "Decibels")

ggplot(df, aes(Date, L10)) +
  geom_line() +
  labs(title = "Outside Sounds Readings 717 Atlantic Avenue - 7th Floor",
       y = "L10 (dBA)")
