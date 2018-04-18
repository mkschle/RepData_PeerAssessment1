---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
dt <- read.csv("activity.csv", header = T)
dim(dt)
```

```
## [1] 17568     3
```

```r
str(dt)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(dt)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
tail(dt)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
missing_dt <- dt[is.na(dt$steps),]
dim(missing_dt)
```

```
## [1] 2304    3
```

```r
library(ggplot2)
```

## What is mean total number of steps taken per day?

```r
dt1 <- dt[!is.na(dt$steps),]
total_number_steps <- with(dt, tapply(steps, as.factor(dt$date), sum, na.rm = T))
hist(total_number_steps, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
summary(total_number_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

```r
mean_steps <- with(dt1, tapply(steps, dt1$interval, mean))
interval <- levels(as.factor(dt1$interval))
plot(interval, mean_steps, type = "l", main = "Time series plot of the \n average number of steps taken", xlab = "interval", ylab = "Mean steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
table <- data.frame(mean_steps, interval)
```


## What is the average daily activity pattern?

```r
table[table$mean_steps==max(table$mean_steps),][2]
```

```
##     interval
## 835      835
```

```r
length(missing_dt$steps)
```

```
## [1] 2304
```

```r
mean_steps <- with(dt1, tapply(steps, dt1$interval, mean))
```


## Imputing missing values

```r
missing_dt$steps <- mean_steps
new_dt <- rbind(dt1, missing_dt)
new_dt <- new_dt[order(new_dt$date), ]
total_number_steps2 <- with(new_dt, tapply(steps, as.factor(new_dt$date), sum))
```


## Are there differences in activity patterns between weekdays and weekends?

```r
hist(total_number_steps2, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
summary(total_number_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

```r
summary(total_number_steps2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```


