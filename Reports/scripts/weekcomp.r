library("aws.s3")
library("purrr")

SEC_IN_DAY = 24*60*60
SEC_IN_WEEK = 7*SEC_IN_DAY
SEC_PER_10MIN = 60*10

start_week1 = "2020-05-24 16:00:00 EDT"
start_week2 = "2020-09-13 16:00:00 EDT"

# Compare 2 weeks of L10 measurements.  
#
# @param start_week1 Start of first week, in local time, only hour resolution.
# @param start_week2 Start of second week, in local time, time of day must match start_week1.
# @examples
# compare_weeks("2020-05-24 16:00:00 EDT", "2020-05-31 16:00:00 EDT")
#
compare_weeks <- function(start_week1, start_week2) {

    week1dt_st = as.POSIXct(start_week1)
    attributes(week1dt_st)$tzone = "UTC"
    week1dt_end = week1dt_st + SEC_IN_WEEK

    week2dt_st = as.POSIXct(start_week2)
    attributes(week2dt_st)$tzone = "UTC"
    week2dt_end = week2dt_st + SEC_IN_WEEK

    days_need_data = unique(c(seq(as.Date(week1dt_st), as.Date(week1dt_end), "days"), seq(as.Date(week2dt_st), as.Date(week2dt_end), "days")))

    files_needed = days_need_data %>% map(function(date) sprintf("SSND_1453_%s.csv", format(date, "%Y_%m_%d")))

    master_frame = get_S3_data(files_needed)

    master_frame$TimeObj = as.POSIXct(master_frame$Time, tz="UTC")

    week1fr = subset(master_frame, week1dt_st <= TimeObj & TimeObj < week1dt_end)
    week2fr = subset(master_frame, week2dt_st <= TimeObj & TimeObj < week2dt_end)

    plot_weeks(week1dt_st, week1fr, week2dt_st, week2fr)
}


# Pulls acoustic data from S3
#
# @param files_needed A list of S3 object names desired from the bucket.
# @return A data frame containing all the data across the files.
#
get_S3_data <- function(files_needed) {
    bucket = get_bucket(bucket = Sys.getenv("SSND_S3_NAME"))
    ret = NULL

    for (file in files_needed) {
        obj = get_object(file, bucket = bucket, as = "text")
        csv = read.csv(text = obj)

        if (ncol(csv) > 1) {
            ret = rbind(ret, csv)
        }
    }
    ret
}


# Plots two weeks, using format of Cavanough report.
#
# @param week1dt_st Start datetime of first week.
# @param week1fr Data frame for first week data.
# @param week2dt_st Start datetime of second week.  Must be same time of day as week1dt_st.
# @param week2fr Data frame for second week data.
#
plot_weeks <- function(week1dt_st, week1fr, week2dt_st, week2fr) {
    begin_local_week1 = week1dt_st
    attributes(begin_local_week1)$tzone = "EST5EDT"
    begin_local_week1 = as.POSIXct(format(begin_local_week1, "%Y-%m-%d"), tz="EST5EDT")

    begin_local_week2 = week2dt_st
    attributes(begin_local_week2)$tzone = "EST5EDT"
    begin_local_week2 = as.POSIXct(format(begin_local_week2, "%Y-%m-%d"), tz="EST5EDT")

    start_lab = as.numeric(begin_local_week1 - week1dt_st, units="secs") / SEC_PER_10MIN   # 12 AM local
    end_lab = start_lab + (SEC_IN_WEEK + SEC_IN_DAY) / SEC_PER_10MIN  
    lab_base = c("12 AM", "6 AM", "12 PM", "6 PM")
    lab_times = seq(start_lab, end_lab, SEC_IN_DAY / SEC_PER_10MIN / length(lab_base)) 
    labels = c(rep(lab_base, times=8), "12 AM")

    week1fr$axis = (week1fr$TimeObj - week1dt_st) / SEC_PER_10MIN
    week2fr$axis = (week2fr$TimeObj - week2dt_st) / SEC_PER_10MIN

    plot(week1fr$axis, week1fr$LAF10, lwd=1, type="l", xlim=c(start_lab, end_lab), main="L10 Sound Levels Measured at 717 Atlantic Ave.", ylim=c(30,90), xlab="", ylab="A-weighted Sound Pressure Level [dB re: 20 μPa]", xaxt="n", xaxs="i", yaxs="i")
    lines(week2fr$axis, week2fr$LAF10, lwd=1, col="blue")

    axis(1,at=lab_times,labels=labels,las=2)

    for (idx in seq(40,80,10)) {
        abline(h=idx, lwd=0.5)
    }

    for (idx in seq(start_lab, end_lab, 6*6)) {
        abline(v=idx, col="lightgray", lwd=0.5)
    }

    for (idx in seq(start_lab, end_lab, 24*6)) {
        abline(v=idx, lwd=0.5)
    }

    for (idx in 0:7) {
        weekday1 = format(begin_local_week1 + idx*SEC_IN_DAY, "%A")
        calday1 = format(begin_local_week1 + idx*SEC_IN_DAY, "%B %d")

        text(c(start_lab+(12+24*idx)*6, start_lab+(12+24*idx)*6), c(49, 45), labels=c(weekday1, calday1), pos=1)

        weekday2 = format(begin_local_week2 + idx*SEC_IN_DAY, "%A")
        calday2 = format(begin_local_week2 + idx*SEC_IN_DAY, "%B %d")

        text(c(start_lab+(12+24*idx)*6, start_lab+(12+24*idx)*6), c(39, 35), labels=c(weekday2, calday2), pos=1, col="blue")
    }
}
