# Reproducible Research: Peer Assessment 1

```r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
if(!dir.exists("figures"))
        dir.create("figures")
```

## Loading and preprocessing the data
R script for the following is given below:

1. Load the data

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
library(dplyr)
unzip("activity.zip", overwrite = TRUE)
activity <- read.csv("activity.csv")

# remove records with NAs
tidyact <- activity[!is.na(activity$steps),]
tidyact <- tidyact[!is.na(tidyact$date),]
tidyact <- tidyact[!is.na(tidyact$interval),]
```

## What is mean total number of steps taken per day?
R script for the following is given below:

1. Calculate the total number of steps taken per day

2. Make a histogram of the total number of steps taken each day

```r
total <- tapply(tidyact$steps, tidyact$date, sum)
average <- mean(total, na.rm = TRUE)
med <- median(total, na.rm = TRUE)
png("./figures/histogram_1.png")
hist(total, main = "Histogram of total steps taken per day")
dev.off()
```

```
## png 
##   2
```

3. Mean of total number of steps taken per day is: 

```r
average
```

```
## [1] 10766.19
```
and the Median of total number of steps taken per day is: 

```r
med
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
subact <- split(activity,activity$interval)
gr <- lapply(subact, function(x){mean(x[,1], na.rm = TRUE)})
png("./figures/timeseries_1.png")
plot(names(subact),as.numeric(gr), type = 'l', xlab = "5-minute interval", ylab = "Average number of steps taken")
dev.off()
```

```
## png 
##   2
```

2. The 5-minute interval, averaged across all days, that has the maximum number of steps is in between:

```r
num <- names(gr[max(as.numeric(gr))])
num <- as.numeric(num)
paste(num-4,"to",num, sep=" ") 
```

```
## [1] "1701 to 1705"
```
and the value (maximum number of steps in the  5-minute interval) is:

```r
max(as.numeric(gr))
```

```
## [1] 206.1698
```

## Imputing missing values

1. The total number of missing values in the dataset is: 

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. The strategy being employed to impute the missing values is:
"to replace NAs with the average number of steps taken for that 5-minute interval across all days" 

3. New dataset that is equal to the original dataset but with the missing data filled in:

```r
new_activity <- activity[is.na(activity$steps),]
for(i in 1:length(gr)){
        new_activity[new_activity$interval==as.integer(names(gr[i])),1] <- gr[[i]]
}
new_activity <- rbind(new_activity,activity[!is.na(activity$steps),])
new_activity <- arrange(new_activity, date,interval)

new_total <- tapply(new_activity$steps, new_activity$date, sum)
new_average <- mean(new_total, na.rm = TRUE)
new_med <- median(new_total, na.rm = TRUE)
```

4. Histogram of total number of steps taken each day

```r
png("./figures/histogram_2.png")
hist(new_total, main = "Histogram of total steps taken per day")
dev.off()
```

```
## png 
##   2
```

New mean (after imputing for missing values) of total number of steps taken per day is: 

```r
new_average
```

```
## [1] 10766.19
```
and the New median  (after imputing for missing values) of total number of steps taken per day is: 

```r
new_med
```

```
## [1] 10766.19
```

The increase in mean and median are:

```r
paste((new_average-average)," and ", (new_med-med)," respectively.", sep="")
```

```
## [1] "0 and 1.1886792452824 respectively."
```

## Are there differences in activity patterns between weekdays and weekends?

1. New factor variable in dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day: is present in the column "which_day" of the **new_activity** dataset 


```r
# Adding a new factor variable to represent weekdays and weekends
for(i in 1:nrow(new_activity)){
        if(weekdays(as.Date(new_activity[i,2])) 
           %in% c("Sunday", "Saturday"))
                new_activity[i,4] <- "weekend"
        else
                new_activity[i,4] <- "weekday"
}       
colnames(new_activity)[4] <- "which_day"  
new_activity$which_day <- as.factor(new_activity$which_day)
```

2. Time series plot of 5-minute interval and average number of steps taken, averaged across all weekdays and weekend days:

```r
# Plotting the steps data across days 
#First, divide the day into two separate subsets
weekday_activity <- new_activity[new_activity$which_day == "weekday",1:3]
weekend_activity <- new_activity[new_activity$which_day == "weekend",1:3]

#calculate averages for weekday dataset
weekday_subact <- split(weekday_activity,weekday_activity$interval)
weekday_gr <- lapply(weekday_subact, function(x){mean(x[,1], na.rm = TRUE)})
#calculate averages for weekday dataset
weekend_subact <- split(weekend_activity,weekend_activity$interval)
weekend_gr <- lapply(weekend_subact, function(x){mean(x[,1], na.rm = TRUE)})

#plot both the data
png("./figures/timeseries_2.png")
par(mfrow=c(2,1), mar = c(4,4,3,2))
plot(names(weekend_subact),as.numeric(weekend_gr), type = 'l', main = "weekend", cex.lab = 0.7, cex.axis = 0.5, xlab = "5-minute interval", ylab = "avg. steps count")
plot(names(weekday_subact),as.numeric(weekday_gr), type = 'l', main = "weekday", cex.lab = 0.7, cex.axis = 0.5, xlab = "5-minute interval", ylab = "avg. steps count")
dev.off()
```

```
## png 
##   2
```
