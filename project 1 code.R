# create a new directory for the project and set as working directory
if(!file.exists("Activity_Data")) dir.create("Activity_Data")
setwd("./Activity_Data")

# download the data from the website, include date downloaded, and unzip
dataset_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fileDest <- sprintf("activity_%s.zip", format(Sys.time(),"%Y_%m_%d_%H_%M_%S"))
download.file(dataset_url, fileDest, mode = "wb", method = "libcurl")
unzip(fileDest)

# read in and check the data
data <- read.csv("activity.csv")
head(data)
str(data)

# percentage of complete cases
round(mean(complete.cases(data)), 3) * 100
# percentage of NAs
round(mean(is.na(data$steps)), 3) * 100
# percentage of zeroes
round(sum(data$steps == 0, na.rm = TRUE)/nrow(data), 3) * 100
# number of (obvious)duplicates
sum(duplicated(data)) 
# number of negative values
sum(data$steps < 0, na.rm = TRUE)

# data exploration
library(lattice)
Mydotplot <- function(DataSelected){
        
        P <- dotplot(as.matrix(as.matrix(DataSelected)),
                     groups = FALSE,
                     strip = FALSE,
                     # strip = strip.custom(bg = 'white',
                     # par.strip.text = list(cex = 1.2)),
                     scales = list(x = list(relation = "free", draw = TRUE),
                                   y = list(relation = "free", draw = FALSE)),
                     col=1, cex  = 0.5, pch = 16,
                     xlab = list(label = "Number of steps", cex = 1.5),
                     ylab = list(label = "Order of the data", cex = 1.5),
                     main = list(label = "Cleveland dotplot", cex = 1.75))
        
        print(P)  
}

# look for outliers, NAs, zeroes and general distribution
Mydotplot(data$steps)
histogram(data$steps,
          nint = 40, 
          xlab = list(label = "Number of Steps", cex = 1.5),
          ylab = list(label = "Percent of Total", cex = 1.5),
          main = list(label = "Frequency Histogram", cex = 1.75))

# examine simple transformations
plot(log10(data$steps +1) ~ data$date)
title(main = "Log transformation")
plot(sqrt(data$steps) ~ data$date)
title(main = "Square root transformation")

# create new variables for Date, day of week and weekday/weekend
library(dplyr)
data <- mutate(data, Date = as.Date(date)) %>%
        mutate(DayOfWeek = factor(weekdays(Date),
                levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
data$WeekDay[which(data$DayOfWeek %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))] <- "Weekday"
data$WeekDay[which(data$DayOfWeek %in% c("Sat", "Sun"))] <- "Weekend"

# plot the total daily steps
dailyData <- group_by(data, Date) %>%
        summarise(Total_Steps = sum(steps, na.rm = TRUE)) %>%
        plot(type = "h",
             xlab = "Consecutive Days", ylab = "Number of Steps",
             main = "Total Daily Steps (using original data)")

# calculate the mean/median of the daily total number of steps (excl days with no data)
summaryData1 <- group_by(data, Date) %>%
        summarise(Total_Steps = sum(steps, na.rm = TRUE)) %>%
        filter(Total_Steps != 0) %>%
        summarise(Grand_Total_Steps = sum(Total_Steps),
                  Average_Steps = round(mean(Total_Steps), 0),
                  Median_Steps = round(median(Total_Steps), 0))
# print(summaryData1)
library(xtable)
xt1 <- xtable(summaryData1)
print(xt1, type = "html")

# calculate and plot the mean number of steps per interval
interval_Data <- group_by(data, interval) %>%
        select(steps) %>%
        summarise(Mean_Steps = mean(steps, na.rm = TRUE))
library(ggplot2)
qplot(interval, Mean_Steps, data = interval_Data,
      geom = "line",
      main = "Time series of mean number of steps per 5 min interval over 24 hours")

# find time interval with highest mean number of steps
which(interval_Data$Mean_Steps == max(interval_Data$Mean_Steps))
max_activity <- interval_Data$interval[which(interval_Data$Mean_Steps == 
                                                     max(interval_Data$Mean_Steps))]
# find time with highest mean number of steps (2:00 pm)
(round(max_activity / 60, 0))

# data exploration for imputation
# number of NAs
sum(is.na(data$steps))

# plot NAs by date and identify which days are missing
dailyNAs <- group_by(data, Date) %>%
        summarise(NAs = sum(is.na(steps)))
barplot(dailyNAs$NAs, xlab = "Consecutive Days", ylab = "Number of NAs")
title(main = "Pattern of Missing Values by Day")

NA_indices <- which(dailyNAs$NAs > 0)
NA_days <- filter(dailyNAs, NAs > 0)
print(NA_days)

# find days for which imputed data required (all but Tuesdays)
missing_data <- data[which(!complete.cases(data)), ]
unique(missing_data$DayOfWeek)

# patterns in diurnal activity by day of week and weekday vs weekends
coplot(steps ~ interval | factor(DayOfWeek),
       data = data[!is.na(data$steps), ],
       ylab = NULL, xlab = NULL,
       bar.bg = c(fac = "light blue"))
title(main = "Coplot",
      ylab = "Steps",
      xlab = "Interval")

# plot time series by day of week and weekday vs weekend
library(reshape2)
# select out complete cases
complete_data <- data[which(complete.cases(data)), ]
wide_date1 <- acast(complete_data, interval ~ date, value.var = "steps")
wide_DayOfWeek1 <- acast(complete_data, interval ~ DayOfWeek,
                        value.var = "steps", fun.aggregate = sum)
wide_WeekDay1 <- acast(complete_data, interval ~ WeekDay,
                      value.var = "steps", fun.aggregate = sum)

# plot time series
Day_Of_Week1 <- ts(wide_DayOfWeek1)
plot(Day_Of_Week1)
Week_Day1 <- ts(wide_WeekDay1)
plot(Week_Day1)

# obtain data for imputation (based on Weekday vs Weekend profiles)
# calculate the median number of steps in each interval for weekdays and weekends
# create join_id
Weekday_profile <- complete_data %>%
        filter(WeekDay == "Weekday") %>%
        group_by(interval) %>%
        summarise(Median_Steps = round(median(steps), 0)) %>%
        mutate(join_id = paste("Weekday", interval))

Weekend_profile <- complete_data %>%
        filter(WeekDay == "Weekend") %>%
        group_by(interval) %>%
        summarise(Median_Steps = round(median(steps), 0)) %>%
        mutate(join_id = paste("Weekend", interval))

# create table with imputed values (medians for intervals by weekdays & weekends)
imputed_profile <- rbind(Weekday_profile, Weekend_profile)

# create join_id in data and join to imputed_profile
data <- mutate(data, join_id = paste(WeekDay, interval))

# join data and imputed_profile
data_imputed <- left_join(data, imputed_profile, by = "join_id")

# create new dataset with steps_imputed variable instead of steps
data_imputed <- mutate(data_imputed, 
                       steps_imputed = ifelse(is.na(steps), Median_Steps, steps)) %>%
        mutate(WeekDay = as.factor(WeekDay)) %>%
        select(interval.x, date, Date, steps_imputed, DayOfWeek, WeekDay) %>%
        rename(interval = interval.x)

# re-plot the total daily steps with new dataset
dailyData <- group_by(data_imputed, Date) %>%
        summarise(Total_Steps = sum(steps_imputed)) %>%
        plot(type = "h",
             xlab = "Consecutive Days", ylab = "Number of Steps",
             main = "Total Daily Steps (using imputed data)")

# re-calculate the mean/median of the daily total number of steps with imputed data
summaryData2 <- group_by(data_imputed, Date) %>%
        summarise(Total_Steps = sum(steps_imputed)) %>%
        summarise(Grand_Total_Steps = sum(Total_Steps),
                  Average_Steps = round(mean(Total_Steps), 0),
                  Median_Steps = round(median(Total_Steps), 0))
print(summaryData2)
xt2 <- xtable(summaryData2)
print(xt2, type = "html")

# compare grand total, mean and median steps with original and imputed data
summaryData3 <- rbind(summaryData1, summaryData2)
row.names(summaryData3) <- c("Original data (excl days with zero steps)", "Imputed data")
xt3 <- xtable(summaryData3)
print(xt3, type = "html")


# compare weekday and weekend activity profiles with imputed data
wide_WeekDay2 <- acast(data_imputed, interval ~ WeekDay,
                       value.var = "steps_imputed", fun.aggregate = sum)

# plot time series
Weekend_Activity_Comparison <- ts(wide_WeekDay2)
plot(Weekend_Activity_Comparison)

# on completion, reset wd to initial wd
setwd("../")