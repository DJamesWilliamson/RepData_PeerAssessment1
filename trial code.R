sessionInfo()
setwd("C:\\Users\\he49794\\Work\\Statistics Resources\\Coursera\\Reproducible Research\\Project 1")
# store path to current working directory
initial_wd <- getwd()

# on completion, reset wd to initial wd
# setwd("../")
# setwd("initial_wd")

# create a new directory for the project and set as working directory
if(!file.exists("Activity_Data")) dir.create("Activity_Data")
setwd("./Activity_Data")
# getwd()


# download the data from the website, including date downloaded, and unzip
dataset_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fileDest <- sprintf("activity_%s.zip", format(Sys.time(),"%Y_%m_%d_%H_%M_%S"))
download.file(dataset_url, fileDest, mode = "wb", method = "libcurl")
unzip(fileDest)

# temp <- tempfile()
# download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
#               destfile = temp)
# data <- read.csv(unz(temp, "activity.csv"))
# unlink(temp)

# read in and check the data
library(dplyr)
data <- tbl_df(read.csv("activity.csv", stringsAsFactors = FALSE))
head(data)
str(data)
View(data)
fix(data)







# number and proportion of complete cases
sum(complete.cases(data))
round(mean(complete.cases(data)), 3) * 100
# number and proportion of NAs
sum(is.na(data$steps))
round(mean(is.na(data$steps)), 3) * 100
# number and proportion of zeroes
sum(data$steps == 0, na.rm = TRUE)
round(sum(data$steps == 0, na.rm = TRUE)/nrow(data), 3) * 100
# number of (obvious)duplicates
sum(duplicated(data)) 
# number of negative values
sum(data$steps < 0, na.rm = TRUE)

# create new variables
# library(dplyr)
data <- mutate(data, Date = as.Date(date)) %>%
        mutate(DayOfWeek = factor(weekdays(Date),
                levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
data$WeekDay[which(data$DayOfWeek %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))] <- "Weekday"
data$WeekDay[which(data$DayOfWeek %in% c("Sat", "Sun"))] <- "Weekend"


library(stringr)
data$time <- str_pad(data$interval, width = 4, "left", pad = "0")
data$date.time <- paste(data$date, data$time, sep = " ")
library(lubridate)
data$date.time <- ymd_hm(data$date.time)
data <- mutate(data, sequence = 1:nrow(data))
# data$date.time.lt <- as.POSIXlt(data$date.time)
# data$date.time.list <- unclass(data$date.time.lt)
# p <- data$date.time.list["min"]
# class(p)
# ?POSIX.lt()
# DailySteps <- group_by(data, Date)
# MeanDailySteps <- summarise(DailySteps,
#                             Total_Steps = sum(steps, na.rm = TRUE),
#                             Average_Steps = mean(steps, na.rm = TRUE),
#                             Median_Steps = median(steps, na.rm = TRUE))

# plot the total daily steps
dailyData <- group_by(data, Date) %>%
                summarise(Total_Steps = sum(steps, na.rm = TRUE)) %>%
                plot(type = "h",
                     xlab = "Consecutive Days", ylab = "Number of Steps",
                     main = "Total Daily Steps")



# barplot(dailyData$Total_Steps, xlab = "Consecutive Days", ylab = "Number of Steps")
# title(main = "Daily Steps")

# calculate the mean of the daily total number of steps (excluding days with no data)
dailyData <- group_by(data, Date) %>%
                filter(steps != 0) %>%
                summarise(Total_Steps = sum(steps, na.rm = TRUE),
                                        Average_Steps = mean(steps, na.rm = TRUE),
                                        Median_Steps = median(steps, na.rm = TRUE))
round(mean(dailyData$Total_Steps, na.rm = TRUE), 0)
round(median(dailyData$Total_Steps, na.rm = TRUE), 0)

# calculate the mean number of steps per interval
interval_Data <- group_by(data, interval) %>%
        select(steps) %>%
        summarise(Mean_Steps = mean(steps, na.rm = TRUE))
# Daily_Profile <- ts(interval_Data)
# plot(Daily_Profile)

library(ggplot2)

# plot(interval_Data$time, interval_Data$Mean_Steps)
qplot(interval, Mean_Steps, data = interval_Data,
      geom = "line",
      # colour= WeekDay,
      main = "Time series of mean number of steps over 24 hours")

# find time interval with highest mean number of steps
max_activity <- interval_Data$interval[which(interval_Data$Mean_Steps == 
                                        max(interval_Data$Mean_Steps))]
print(str_pad(max_activity, width = 4, "left", pad = "0"))

# plot NAs by date and identify which days are missing
dailyNAs <- group_by(data, Date) %>%
                summarise(NAs = sum(is.na(steps)))
barplot(dailyNAs$NAs, xlab = "Consecutive Days", ylab = "Number of NAs")
title(main = "Daily Missing Values")
NA_indices <- which(dailyNAs$NAs > 0)
NA_days <- filter(dailyNAs, NAs > 0)

# alternative way of identifying incompete cases in data
missing_data <- data[which(!complete.cases(data)), ]
# find days for which imputed data required (all but Tuesdays)
unique(missing_data$DayOfWeek)





# boxplot(steps ~ DayOfWeek, 
#         data = data,
#         ylab = "Steps",
#         xlab = "Day of Week")
# title(main = "Number of Steps by Day of Week")
# 
# boxplot(steps ~ factor(WeekDay), 
#         data = data,
#         ylab = "Steps",
#         xlab = "Weekday")
# title(main = "Number of Steps by Weekday")


library(reshape2)
# select out complete cases
complete_data <- data[which(complete.cases(data)), ]

# wide_date <- acast(complete_data, interval ~ date, value.var = "steps")
# wide_DayOfWeek <- acast(complete_data, interval ~ DayOfWeek,
#                         value.var = "steps", fun.aggregate = sum)
# wide_WeekDay <- acast(complete_data, interval ~ WeekDay,
#                         value.var = "steps", fun.aggregate = sum)


wide_date <- acast(complete_data, interval ~ date, value.var = "steps")
wide_DayOfWeek <- acast(complete_data, interval ~ DayOfWeek,
                        value.var = "steps", fun.aggregate = sum)
wide_WeekDay <- acast(complete_data, interval ~ WeekDay,
                      value.var = "steps", fun.aggregate = sum)

# plot time series
Day_Of_Week <- ts(wide_DayOfWeek)
plot(Day_Of_Week)
# title(main = "Time Series comparing Days of the Week")

Week_Day <- ts(wide_WeekDay)
plot(Week_Day)
# title(main = "Time Series comparing Weekdays and Weekends")


# obtain data for imputation (based on Weekday vs Weekend profiles)
# calculate the median number of steps in each interval for weekdays and weekends
# create join_id
Weekday_profile <- complete_data %>%
        filter(WeekDay == "Weekday") %>%
        group_by(interval) %>%
        summarise(Median_Steps = round(median(steps), 0)) %>%
        mutate(join_id = paste("Weekday", interval))
# plot(Weekday_profile)
View(Weekday_profile)

Weekend_profile <- complete_data %>%
        filter(WeekDay == "Weekend") %>%
        group_by(interval) %>%
        summarise(Median_Steps = round(median(steps), 0)) %>%
        mutate(join_id = paste("Weekend", interval))
# plot(Weekend_profile)
View(Weekend_profile)
# create table with imputed values (medians for each interval for weekdays & weekends)
imputed_profile <- rbind(Weekday_profile, Weekend_profile)

# create join_id in data and join to imputed_profile
data <- mutate(data, join_id = paste(WeekDay, interval))

# head(data)
# head(imputed_profile)


# join data and imputed_profile
data_imputed <- left_join(data, imputed_profile, by = "join_id")
View(data_imputed)

# create new variable: steps_imputed
data_imputed <- mutate(data_imputed, steps_imputed = ifelse(is.na(steps), Median_Steps, steps))


library(ggplot2)
qplot(interval, log10(steps + 1), data = complete_data,
      geom = "point",
      colour= WeekDay,
      main = "Comparison of daily activity over weekdays and weekends")

coplot(steps ~ interval | factor(DayOfWeek),
       data = data,
       ylab = NULL,
       xlab = NULL,
       bar.bg = c(fac = "light blue"))
title(main = "Coplot",
      ylab = "Steps",
      xlab = "Interval")

# library(xtable)
# xt <- xtable(summary(data))
# xt
# print(xt, type = "html")
# summ <- summary(data)
# nonParamSum <- xtable(summ[c(1, 2, 3, 5, 6)])
# print(nonParamSum, type = "html")

dataSummary <- as.vector(summary(data))
as.data.frame(dataSummary[c(1, 2, 3, 5, 6)])
as.data.frame(dataSummary[4])
as.data.frame(dataSummary[7])

#Library files for courses provided by: Highland Statistics Ltd.
#To cite these functions, use:
#Mixed effects models and extensions in ecology with R. (2009).
#Zuur, AF, Ieno, EN, Walker, N, Saveliev, AA, and Smith, GM. Springer.

#Copyright Highland Statistics LTD.

#Function for multi-panel Cleveland dotplot.
#The input file must contain no categorical variables
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
          # equal.widths = FALSE,
          nint = 40, 
          xlab = list(label = "Number of Steps", cex = 1.5),
          ylab = list(label = "Percent of Total", cex = 1.5),
          main = list(label = "Frequency Histogram", cex = 1.75))

plot(log10(data$steps +1) ~ data$date)


boxplot(data$steps, 
        main = "Steps")
dotchart(data$steps, 
         xlab = "Range of data", 
         ylab = "Order of the data")



# this is unsuitable for the current problem because of nature of missingness
# near neighbour imputation
# x : vector to be imputed
# last : value to use if last value of x is empty
seqImpute <- function(x,last){
        n <- length(x)
        x <- c(x,last)
        i <- is.na(x)
        while(any(i)){
                x[i] <- x[which(i) + 1]
                i <- is.na(x)
        }
        x[1:n]
}

tail(data)
imputed_steps <- seqImpute(data$steps, 0)
head(imputed_steps)
tail(imputed_steps)