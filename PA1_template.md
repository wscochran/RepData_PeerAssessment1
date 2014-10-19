# Reproducible Research: Peer Assessment 1
William Scott Cochran  
10/17/2014  


## Loading and preprocessing the data

```r
setwd("~/Dropbox/Coursera/Data Science/Reproducible Research/RepData_PeerAssessment1")

# Unzip the data if its not already here
if(! file.exists('activity.csv')) {
        unzip('activity.zip')
}

df <- read.csv('activity.csv', 
               header = TRUE, 
               colClasses = c(steps="numeric",
                              date="Date",
                              interval="numeric"))
library(data.table)
Activity <- as.data.table(df)

stepsPerDay <- aggregate(steps ~ date, Activity, sum)
```

## What is mean total number of steps taken per day?

```r
hist(stepsPerDay$steps, 
     xlab = "Steps Per Day", 
     main ="Histogram of Steps Taken Each Day", 
     col = "red",
     breaks=10)
```

![plot of chunk Histogram](./PA1_template_files/figure-html/Histogram.png) 

```r
meanStepsPerDay <- mean(stepsPerDay$steps)
medianStepsPerDay <- median(stepsPerDay$steps)
```

The mean steps taken per day is: 1.0766 &times; 10<sup>4</sup>

The median steps taken per day is: 1.0765 &times; 10<sup>4</sup>

## What is the average daily activity pattern?

```r
stepsPerInterval <- aggregate(steps ~ interval, Activity, mean)
plot(steps ~ interval, 
     data = stepsPerInterval, 
     type = 'l', 
     xlab="5 Minute Interval", 
     ylab="Mean Number of Steps",
     main="Average Daily Activity")
```

![plot of chunk AverageDailyActivity](./PA1_template_files/figure-html/AverageDailyActivity.png) 

```r
maxAvgStepsInterval <- 
        stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
maxAvgSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$steps
```
Interval number 835 has the maximum average number(206.1698) of steps.


## Imputing missing values

```r
missingData <- sum(!complete.cases(Activity))

ActivityImputed <- Activity[,steps := ifelse(is.na(steps), 
                                          mean(steps, na.rm=TRUE),
                                          steps),
                         by=interval]

stepsPerDayImputed <- aggregate(steps ~ date, ActivityImputed, sum)

hist(stepsPerDayImputed$steps, 
     xlab = "Steps Per Day(Imputed NA's)", 
     main ="Histogram of Steps Taken Each Day with Imputed NAs", 
     col = "red",
     breaks=10)
```

![plot of chunk stepsPerDayImputed](./PA1_template_files/figure-html/stepsPerDayImputed.png) 

```r
meanStepsPerDayImputed <- mean(stepsPerDayImputed$steps)
medianStepsPerDayImputed <- median(stepsPerDayImputed$steps)
```
There were 2304 missing values which were imputed as the mean for their given interval.

The process of imputing mean values for the missing step data coerced the step counts to floats, causing the value of the median steps to be rounded up 1 step.

The Imputing of values also resulted in the dataset used for the histogram above to include 8 additional days of data as compared to the initial histogram.

The Imputed NA mean steps taken per day is: 1.0766 &times; 10<sup>4</sup>

The Imputed NA median steps taken per day is: 1.0766 &times; 10<sup>4</sup>


## Are there differences in activity patterns between weekdays and weekends?

```r
ActivityImputed[, daytype:= ifelse(weekdays(date)=="Saturday" 
                                   | weekdays(date)=="Sunday",
                                   "weekend", "weekday")]

ActivityImputed[, daytype := as.factor(daytype)]

stepsPerIntervalImputed <- 
        aggregate(steps ~ interval + daytype, ActivityImputed, mean)

library(lattice)
xyplot(steps ~ interval | daytype,
       stepsPerIntervalImputed,
       type='l',
       layout = c(1, 2))
```

![plot of chunk ActivityPatterns](./PA1_template_files/figure-html/ActivityPatterns.png) 
