# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# load the original data into a data frame
data_df <- read.csv("activity.csv")

# remove NAs for analysis
data_noNA <- data_df[complete.cases(data_df),]

# convert to dplyr dataframe format for better analysis
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data_tbl_noNA <- tbl_df(data_noNA)
```

## What is mean total number of steps taken per day?

```r
# calculate and report mean, median & total number of steps per day
summary <- summarize(group_by(data_tbl_noNA,date), sumOfSteps=sum(steps))
print(paste("Mean =", mean(summary$sumOfSteps)))
```

```
## [1] "Mean = 10766.1886792453"
```

```r
print(paste("Median =", median(summary$sumOfSteps)))
```

```
## [1] "Median = 10765"
```

```r
# draw a histogram of the total number of steps taken each day
library(ggplot2)
library(scales)
qplot(as.Date(date), data=summary, weight=sumOfSteps, binwidth=1, geom="histogram")+xlab("Date")+ylab("Number of Steps")+ggtitle("Total number of steps taken each day")+stat_bin(binwidth=1)+scale_x_date(labels=date_format("%m.%d"), breaks=date_breaks("week"))
```

![](PA1_template_files/figure-html/mean_total_steps-1.png) 

## What is the average daily activity pattern?

```r
# draw a time series plot of the average daily activity pattern
summary2<-summarize(group_by(data_tbl_noNA,interval), avgOfSteps=mean(steps))
qplot(interval, avgOfSteps, data=summary2, geom="line") +xlab("Interval")+ylab("Avgerage of steps acrossed days")+ggtitle("Average daily activity pattern")
```

![](PA1_template_files/figure-html/average_daily_pattern-1.png) 

```r
# print out the interval with maximum number of average steps acrossed all the days
maxInterval <- summary2[which.max(summary2$avgOfSteps),1]
print(paste("The interval", maxInterval, "with maximum number of average steps."))
```

```
## [1] "The interval 835 with maximum number of average steps."
```

## Imputing missing values

```r
# calculate and report the total number of missing values
num_of_na <- length(which(is.na(data_df)))
print(paste("Total number of rows with NAs is",num_of_na))
```

```
## [1] "Total number of rows with NAs is 2304"
```

```r
# create a new data set with missing data filled in by the mean of steps for that 5 minute interval
newData <- tbl_df(data_df) %>%
            group_by(interval) %>%
            mutate(steps = ifelse(is.na(steps), mean(steps,na.rm=TRUE), steps))

# calcuate and report mean and median
summary3 <- summarize(group_by(newData,date), sumOfSteps=sum(steps))
print(paste("Mean =", mean(summary3$sumOfSteps)))
```

```
## [1] "Mean = 10766.1886792453"
```

```r
print(paste("Median =", median(summary3$sumOfSteps)))
```

```
## [1] "Median = 10766.1886792453"
```

```r
# make a histogram with missing data filled in
qplot(as.Date(date), data=summary3, weight=sumOfSteps, binwidth=1, geom="histogram")+xlab("Date")+ylab("Number of Steps")+ggtitle("Total number of steps taken each day (with NAs filled in)")+stat_bin(binwidth=1)+scale_x_date(labels=date_format("%m.%d"), breaks=date_breaks("week"))
```

![](PA1_template_files/figure-html/imputing_missing_values-1.png) 

From the results, we could see that, after filling in missing data, the mean of the total number of steps taken per day doesn't change, as the missing data takes the average of steps for that 5 minut interval. But the median value is affected, as adding the missing data changes the distribution of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?


```r
# set the system language to English in case of error arised by ifelse(weekdays(...))
Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

```r
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend”
newData <- mutate(newData, dateType=ifelse(weekdays(as.Date(date)) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
newData$dateType <- as.factor(newData$dateType)

summary4<-summarize(group_by(newData,interval,dateType), avgOfSteps=mean(steps))
qplot(interval, avgOfSteps, data=summary4, geom="line", facets=dateType~.)+xlab("Interval")+ylab("Avgerage of steps acrossed days")+ggtitle("Average activity patterns (weekdays vs. weekends)")+geom_line(color="blue")
```

![](PA1_template_files/figure-html/diff_acitivity_pattern-1.png) 

