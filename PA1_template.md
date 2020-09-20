---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
We use `unzip` to extract a filepath for the activity.csv
file, and then use `read.csv` to read this file.  
No extra parameters are required to read the data correctly.  
  
We also convert the date column from character to date values.

```r
file <- unzip('activity.zip', files = 'activity.csv')
activity <- read.csv(file)
activity$date <- as.Date(activity$date,
                         '%Y-%m-%d')
```

## What is mean total number of steps taken per day?
Firstly, we use `tapply` to find the total number
of steps for each day:

```r
steps_per_day <- tapply(activity$steps,
                        activity$date,
                        sum)
head(steps_per_day)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420
```

We plot the frequencies of the resulting counts
using hist(), with breaks = 10:

```r
hist(steps_per_day,
     xlab = 'Total Steps',
     main = 'Total Steps per day',
     col = 'light blue',
     breaks = 10)
```

![](PA1_template_files/figure-html/Steps_Per_Day_Hist-1.png)<!-- -->

We can also calculate the mean and median of the total daily steps:


```r
mean_daily_steps = mean(steps_per_day, na.rm = TRUE)
paste('The mean is',mean_daily_steps)
```

```
## [1] "The mean is 10766.1886792453"
```

```r
med_daily_steps = median(steps_per_day, na.rm = TRUE)
paste('The median is',med_daily_steps)
```

```
## [1] "The median is 10765"
```

## What is the average daily activity pattern?
We use tapply (with na.rm = TRUE) to find the mean
steps for each 5-minute interval, across all recorded
days:

```r
mean_steps_per_interval <- tapply(activity$steps,
                                  activity$interval,
                                  mean,
                                  na.rm = TRUE)
head(mean_steps_per_interval)
```

```
##         0         5        10        15        20        25 
## 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```

This can then be used to create a line plot, showing the change in this mean across the full day:

```r
plot(names(mean_steps_per_interval),
     mean_steps_per_interval,
     type = 'l',
     xlab = 'Five Minute Interval',
     ylab = 'Average Steps Taken',
     main = 'Average steps taken for each 5-minute interval'
     )
```

![](PA1_template_files/figure-html/Steps_Per_Interval-1.png)<!-- -->

The above plot shows that the number of steps peaks at around 800 (i.e. at around 8 a.m.). This may be due to the test subject
preferring to exercise in the mornings.

## Imputing missing values
We can count the number of missing values present
in the steps column:

```r
tot_NAs <- sum(is.na(activity$steps))
```
This returns 2304. We will replace these values with the mean values of the associated interval, which
we calculated in the previous section and stored in 
the `mean_steps_per_interval` table.  
Firstly, we create a dataframe with the mean steps for each interval, and join this to the `activity` dataframe by the `interval` variable:

```r
mean_steps_per_interval <- 
    data.frame(interval = names(mean_steps_per_interval),
               mean_steps = mean_steps_per_interval)
activity_noNA <- merge(activity,
                       mean_steps_per_interval)
str(activity_noNA)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ interval  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ steps     : int  NA 0 0 0 0 0 0 0 0 0 ...
##  $ date      : Date, format: "2012-10-01" "2012-11-23" ...
##  $ mean_steps: num  1.72 1.72 1.72 1.72 1.72 ...
```

Now, we simply replace any `NA` values with the value in the `mean_steps` column, and reformat the new dataframe to have the same structure as our original `activity` dataframe:


```r
activity_noNA$steps[is.na(activity_noNA$steps)] <- 
    activity_noNA$mean_steps[is.na(activity_noNA$steps)]

activity_noNA <- activity_noNA[order(activity_noNA$date,
                                     activity_noNA$interval),
                               c('steps',
                                 'date',
                                 'interval')]
rownames(activity_noNA) <- NULL

head(activity_noNA)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

We can count the `NA` values in the new dataframe
to ensure that we have solved the problem:

```r
sum(is.na(activity_noNA$steps))
```

```
## [1] 0
```

Let's replot the histogram of total steps per day, to see if our solution has caused a noticeable difference:

```r
steps_per_day <- tapply(activity_noNA$steps,
                        activity_noNA$date,
                        sum)

hist(steps_per_day,
     xlab = 'Total Steps',
     main = 'Total Steps per day',
     col = 'light blue',
     breaks = 10)
```

![](PA1_template_files/figure-html/Steps_Per_Day_Hist_NoNA-1.png)<!-- -->

The new histogram is very similar to the original,
but the central bar is taller - this makes sense,
as we have added more central values to the data.  
We can also recalculate the mean and median of the total daily steps:


```r
mean_daily_steps = mean(steps_per_day, na.rm = TRUE)
paste('The mean is',mean_daily_steps)
```

```
## [1] "The mean is 10766.1886792453"
```

```r
med_daily_steps = median(steps_per_day, na.rm = TRUE)
paste('The median is',med_daily_steps)
```

```
## [1] "The median is 10766.1886792453"
```

The mean has stayed the same, and the median is now
equal to the mean - this is because our new data has
been taken from mean values, and so similarity is
expected. To avoid this we could have rounded the 
new replacement values of `steps` to the nearest 
whole number, but this may have introduced a small
amount of bias.

## Are there differences in activity patterns between weekdays and weekends?
To analyse differences between weekends and weekdays,
we will create a new factor column `weekend` which
takes the value `weekend` on a weekend and `weekday`
on a weekday:

```r
activity_noNA$weekend <- 'weekday'
activity_noNA$weekend[weekdays(activity_noNA$date) %in% c('Saturday',
                                                          'Sunday')] <- 'weekend'
activity_noNA$weekend <- factor(activity_noNA$weekend)

str(activity_noNA)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekend : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

We can plot the average steps taken for each 5-minute
interval separately for both weekdays and weekends.
To do this, we will use the `lattice` package.


```r
library(lattice)
```

We firstly use the `aggregate` function to calculate
the average number of steps taken per interval for
both weekends and weekdays:

```r
weekend_mean_steps <- aggregate(x=activity_noNA$steps,
                                by=activity_noNA[,
                                    c('interval',
                                      'weekend')],
                                FUN=mean)
head(weekend_mean_steps)
```

```
##   interval weekend          x
## 1        0 weekday 2.25115304
## 2        5 weekday 0.44528302
## 3       10 weekday 0.17316562
## 4       15 weekday 0.19790356
## 5       20 weekday 0.09895178
## 6       25 weekday 1.59035639
```
We can use this to plot the weekend and weekday data
in two time series plots, using the `xyplot` function
from the `lattice` package:

```r
xyplot(x~interval|weekend,
       data=weekend_mean_steps,
       main="Number of steps per interval, for weekends and weekdays",
       xlab="Interval",
       ylab = "Average steps taken",
       layout=c(1,2),
       type='l')
```

![](PA1_template_files/figure-html/Steps_Per_Interval_Weekends-1.png)<!-- -->

From this plot, we can see that the majority of steps
on a weekday are taken at around 8 a.m., whereas 
exercise at the weekend is more spread throughout the
day.



