# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Unzip the archive file
2. Load CSV data.
3. Create a total number of steps for each day using tapply.

```r
library(data.table)
```

```
## Warning: package 'data.table' was built under R version 3.1.3
```

```r
unzip(zipfile = "activity.zip");
data = read.csv("activity.csv");
dailySteps = tapply(as.numeric(data$steps), INDEX = data$date, FUN = sum, na.rm = TRUE, simplify =  FALSE);
intervalSteps = tapply(as.numeric(data$steps), INDEX = data$interval, FUN = mean, na.rm = TRUE, simplify =  FALSE);
intervalSteps = data.table(as.numeric(as.character(rownames(intervalSteps))), as.numeric(intervalSteps))
setnames(intervalSteps, colnames(intervalSteps), c('interval','avgSteps'))
```

## What is mean total number of steps taken per day?
Histogram of total number of steps taken per day

```r
hist(as.numeric(dailySteps), xlab="Number of steps taken in a day", main="Histogram of number of steps taken per day");
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Mean and Median steps per day.

```r
cat("Mean total Number of steps per day: ", mean(as.numeric(dailySteps)));
```

```
## Mean total Number of steps per day:  9354.23
```

```r
cat("Median total Number of steps per day: ", median(as.numeric(dailySteps)));
```

```
## Median total Number of steps per day:  10395
```

## What is the average daily activity pattern?
Calculate the most active interval (interval with maximum average steps) and the average number of steps. Also plot the daily activity pattern.

```r
maxStepsPerInterval = max(intervalSteps$avgSteps);
mostActiveInterval = intervalSteps[intervalSteps$avgSteps == maxStepsPerInterval, interval]
cat("Maximum average steps (across all days) in an interval:", maxStepsPerInterval)
```

```
## Maximum average steps (across all days) in an interval: 206.1698
```

```r
cat("Interval with maxiumum number of steps (averaged across all days):", mostActiveInterval)
```

```
## Interval with maxiumum number of steps (averaged across all days): 835
```

```r
plot(intervalSteps, type='l', xlab="Time interval", ylab="Average number of steps in the Interval", main="Average number of steps taken in a 5 Minute interval")
abline(v = mostActiveInterval, lty=2 )
axis(1, at=c(mostActiveInterval))
abline(h = maxStepsPerInterval, lty=2)
axis(4, at=c(maxStepsPerInterval))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

## Imputing missing values
Poluplate the missing values with average value for that interval.

```r
cat("Number of missing values:", sum(is.na(data$steps)))
```

```
## Number of missing values: 2304
```

```r
datacopy = data.table(data)
getAvgIfNa = function(s, i){
  if(!is.na(s)){
    s;
  }else{
    intervalSteps[intervalSteps$interval == i, avgSteps];
  }
};
datacopy$steps = mapply(getAvgIfNa, datacopy$steps, datacopy$interval)
dailySteps = tapply(as.numeric(datacopy$steps), INDEX = datacopy$date, FUN = sum, na.rm = TRUE, simplify =  FALSE);
hist(as.numeric(dailySteps), xlab="Number of steps taken in a day", main="Histogram of number of steps taken per day");
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
cat("Mean total Number of steps per day: ", mean(as.numeric(dailySteps)));
```

```
## Mean total Number of steps per day:  10766.19
```

```r
cat("Median total Number of steps per day: ", median(as.numeric(dailySteps)));
```

```
## Median total Number of steps per day:  10766.19
```
Note that the mean & median of steps have increased since the missing intervals now have values. This has also caused the peak of the normal plot be higher, the slope to be steeper and tails to be thinner (i.e. number of days in the 'average' range is significantly higher than the number of days in the low/high range).

## Are there differences in activity patterns between weekdays and weekends?
Comparing weekend and weekday activity patterns plotted below, we can see that most of the activity in the weekdays is focused in the morning, around the 'peak hour'. This is expected in today's lifestyle with most offices starting around 8 or 9 AM. While in weekend, the activity level is equally spread out over the day, which is also expected. 


```r
datacopy$dayname =  weekdays(as.POSIXlt(datacopy$date));
intervalStepsWeekend = with(datacopy[dayname == "Saturday" | dayname == "Sunday",], tapply(as.numeric(steps), INDEX = interval, FUN = mean, na.rm = TRUE, simplify =  FALSE));
intervalStepsWeekday = with(datacopy[dayname == "Monday" | dayname == "Tuesday"  | dayname == "Wednesay" | dayname == "Thursday" | dayname == "Friday",], tapply(as.numeric(steps), INDEX = interval, FUN = mean, na.rm = TRUE, simplify =  FALSE));
intervalStepsWeekend = cbind(rownames(intervalStepsWeekend), intervalStepsWeekend, "Weekend");
intervalStepsWeekday = cbind(rownames(intervalStepsWeekday), intervalStepsWeekday, "Weekday");
intervalSteps = rbind(intervalStepsWeekday, intervalStepsWeekend);
colnames(intervalSteps)=c("interval", "steps", "dayType")
intervalSteps = data.table(intervalSteps);
intervalSteps$interval = unlist(intervalSteps$interval);
intervalSteps$steps = unlist(intervalSteps$steps);
intervalSteps$dayType = unlist(intervalSteps$dayType);
par(mfrow = c(2,1))
par(mar=c(0,4,4,4))
with(intervalSteps[dayType == 'Weekday'], plot(interval, steps, type="l", ylab="Weekday Steps", main="Comparing weekday and weekend activity patterns", xlab="", xaxt='n') )
par(mar=c(4,4,0,4))
with(intervalSteps[dayType == 'Weekend'], plot(interval, steps, type="l", ylab="Weekend Steps", xlab="Interval"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 
