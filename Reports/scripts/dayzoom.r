library("aws.s3")
library("purrr")

SEC_IN_HOUR = 60*60
SEC_IN_MORNING = 6*SEC_IN_HOUR
SEC_PER_10MIN  = 60*10

NIGHT_NOISE_DATES = c(
    "2020-09-17 00:00:00 EDT",
    "2020-10-08 00:00:00 EDT",
    "2020-10-15 00:00:00 EDT",
    "2020-10-21 00:00:00 EDT",
    "2020-10-27 00:00:00 EDT",
    "2020-11-05 00:00:00 EDT"
)

NIGHT_NOISE_DATES_2 = c(
    "2020-11-17 00:00:00 EDT",
    "2020-11-18 00:00:00 EDT",
    "2020-11-24 00:00:00 EDT",
    "2020-11-28 00:00:00 EDT",
    "2020-12-01 00:00:00 EDT",
    "2020-12-02 00:00:00 EDT"
)

NIGHT_NOISE_DATES_3 = c(
    "2020-09-17 00:00:00 EDT",
    "2020-10-08 00:00:00 EDT",
    "2020-11-18 00:00:00 EDT",
    "2020-11-24 00:00:00 EDT",
    "2020-11-25 00:00:00 EDT",
    "2020-11-28 00:00:00 EDT"
)

MORNING_NOISE_DATES = c(
    "2020-09-19 06:00:00 EDT",
    "2020-10-03 06:00:00 EDT",
    "2020-10-10 06:00:00 EDT"
)

MORNING_NOISE_DATES_2 = c(
    "2020-10-03 09:00:00 EDT",
    "2020-12-01 09:00:00 EDT",
    "2020-12-03 09:00:00 EDT"
)

show_nighttime <- function() {
    par(mfrow=c(2,3))
    labels = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM")
    yrange = c(60,80)

    for (idx in 1:6) {
        if (idx == 1 || idx == 4) {
            ylabel = "SPL [dB re: 20 μPa]"
        } else {
            ylabel = ""
        }

        show_hours(NIGHT_NOISE_DATES_3[idx], ylabel, yrange, labels)
    }
}

show_morning <- function() {
    par(mfrow=c(1,3))
    labels = c("6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM")

    labels_2 = c("9 AM", "10 AM", "11 AM", "12 PM", "1 PM", "2 PM", "3 PM")

    yrange = c(60,90)

    for (idx in 1:3) {
        if (idx == 1) {
            ylabel = "SPL [dB re: 20 μPa]"
        } else {
            ylabel = ""
        }

        show_hours(MORNING_NOISE_DATES_2[idx], ylabel, yrange, labels_2)
    }    
}


show_hours <- function(start_morn, ylabel, yrange, labels) {
    morndt_st = as.POSIXct(start_morn)
    attributes(morndt_st)$tzone = "UTC"
    morndt_end = morndt_st + SEC_IN_MORNING

    days_need_data = seq(as.Date(morndt_st), as.Date(morndt_end), "days")

    files_needed = days_need_data %>% map(function(date) sprintf("SSND_1453_%s.csv", format(date, "%Y_%m_%d")))

    master_frame = get_S3_data(files_needed)

    master_frame$TimeObj = as.POSIXct(master_frame$Time, tz="UTC")

    mornfr = subset(master_frame, morndt_st <= TimeObj & TimeObj < morndt_end)

    plot_hours(morndt_st, mornfr, ylabel, yrange, labels)
}


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


plot_hours <- function(morndt_st, mornfr, ylabel, yrange, labels) {
    begin_local_day = morndt_st
    attributes(begin_local_day)$tzone = "EST5EDT"
    begin_local_day = as.POSIXct(format(begin_local_day, "%Y-%m-%d"), tz="EST5EDT")

    start_lab = 0
    end_lab = start_lab + SEC_IN_MORNING / SEC_PER_10MIN  
    lab_times = seq(start_lab, end_lab, SEC_IN_HOUR / SEC_PER_10MIN) 

    mornfr$axis = (mornfr$TimeObj - morndt_st) / SEC_PER_10MIN

    plot(mornfr$axis, mornfr$LAF10, lwd=1, type="b", xlim=c(start_lab, end_lab), main=begin_local_day, ylim=yrange, xlab="", ylab=ylabel, xaxt="n", xaxs="i", yaxs="i")

    axis(1,at=lab_times,labels=labels,las=2)

    for (idx in seq(60,90,5)) {
        abline(h=idx, lwd=0.5)
    }

    for (idx in seq(start_lab, end_lab, 6)) {
        abline(v=idx, col="lightgray", lwd=0.5)
    }
}
