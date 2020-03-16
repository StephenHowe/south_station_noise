library(readxl)
library(ggplot2)
library(dplyr)

df <- read.delim("data/20200119.XLS", sep ="\t")
#df <- read_xlsx("data/20200118.xlsx")
df$dateTime <- as.character(paste(df$Date, df$Time, sep = " "))
df$dateTime <- strptime(df$dateTime, format = "%Y/%m/%d %H:%M:%S")
df$dateTime <- as.POSIXct(df$dateTime)
df$col <- cut(df$Value, c(30,50,70,130))

# get values for reference lines
elevenies <- as.POSIXct(
  strptime(
    paste(min(as.character(df$Date)), "23:00:00", sep = " "),
    format = "%Y/%m/%d %H:%M:%S"
  )
)

sevenses <- as.POSIXct(
  strptime(
    paste(max(as.character(df$Date)), "07:00:00", sep = " "),
    format = "%Y/%m/%d %H:%M:%S"
  )
)


ggplot(df, aes(dateTime, Value, color = col)) +
  geom_point() +
  scale_colour_manual(name = "Legal Limits", values = c("(30,50]" = "dark blue", "(50,70]" = "orange", "(70,130]" = "red")) +
  geom_vline(xintercept = elevenies) +
  geom_hline(yintercept = sevenses)

elevenies <- as.POSIXct(
  strptime(
    paste(min(as.character(df$Date)), "23:00:00", sep = " "),
    format = "%Y/%m/%d %H:%M:%S"
  )
)

str(df)


# subsetting the data to one night

df$legal_limits <- ifelse(df$col == "(30,50]",
                          "Acceptable Level",
                          ifelse(df$col == "(50,70]",
                                 "Exceeds Nighttime Limit",
                                 "Exceeds Daytime Limit"))

df_subset <- subset(df, df$dateTime > "2020-01-18 23:00:00" & df$dateTime < "2020-01-19 07:0:0")

ggplot(df_subset, aes(dateTime, Value, color = legal_limits)) +
  geom_point() +
  scale_colour_manual(name = "Legal Limits", values = c("Acceptable Level" = "dark blue", 
                                                        "Exceeds Nighttime Limit" = "orange",
                                                        "Exceeds Daytime Limit" = "red")) +
  labs(title = "Noise Monitoring of South Station (Atlantic Avenue) During the Night (11PM - 7 AM)",
       x = "Date and Time",
       y = "Noise Level (dB)")


# create text file for notes
notes <- data.frame("date" = "2020-01-17", "note" = "typcial weekday night", stringsAsFactors = FALSE)

write.csv(notes, "data/notes.txt", sep = "\t")
