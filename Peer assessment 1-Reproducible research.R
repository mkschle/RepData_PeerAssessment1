dt <- read.csv("activity.csv", header = T)
dim(dt)
str(dt)
head(dt)
tail(dt)
missing_dt <- dt[is.na(dt$steps),]
dim(missing_dt)
library(ggplot2)


#1.Code for reading in the dataset and/or processing the data
dt1 <- dt[!is.na(dt$steps),]
total_number_steps <- with(dt, tapply(steps, as.factor(dt$date), sum, na.rm = T))

#2.Histogram of the total number of steps taken each day
hist(total_number_steps, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps")

#3.Mean and median number of steps taken each day
summary(total_number_steps)

#4.Time series plot of the average number of steps taken
mean_steps <- with(dt1, tapply(steps, dt1$interval, mean))
interval <- levels(as.factor(dt1$interval))
plot(interval, mean_steps, type = "l", main = "Time series plot of the \n average number of steps taken", xlab = "interval", ylab = "Mean steps")
table <- data.frame(mean_steps, interval)

#5. The 5-minute interval that, on average, contains the maximum number of steps
table[table$mean_steps==max(table$mean_steps),][2]
length(missing_dt$steps)
mean_steps <- with(dt1, tapply(steps, dt1$interval, mean))

#6.Code to describe and show a strategy for imputing missing data
missing_dt$steps <- mean_steps
new_dt <- rbind(dt1, missing_dt)
new_dt <- new_dt[order(new_dt$date), ]
total_number_steps2 <- with(new_dt, tapply(steps, as.factor(new_dt$date), sum))

#7.Histogram of the total number of steps taken each day after missing values are imputed
hist(total_number_steps2, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps")
summary(total_number_steps)
summary(total_number_steps2)

#8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
new_dt$days <- weekdays(as.Date(new_dt$date))
weekend_feature <- grep("Saturday|Sunday", new_dt$days, ignore.case = T)
weekend_dt<-  new_dt[weekend_feature, ]
weekend_dt$weekday <- "weekend"
weekday_dt$weekday <- "weekday"
new_dt2 <- rbind(weekday_dt, weekend_dt)

#9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
mean_number_steps <- aggregate(steps~ interval+weekday, new_dt2, mean)
g <- qplot(interval, steps, data = mean_number_steps, facets = weekday~.)
g + geom_line(size = .2) + ylab("Mean steps") + ggtitle("Average number of steps taken, Weekdays and weekends ")







