# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
options("scipen" = 1)
# Show any code that is needed to  
# 1. Load the data (i.e. read.csv())  
# 2. Process/transform the data (if necessary) into a format suitable for your analysis

setwd("~/Coursera/courses/05_ReproducibleResearch/Assignment 01/RepData_PeerAssessment1")
data<-read.csv("activity.csv",header=TRUE, colClasses=c("integer","Date","integer"))
summary(data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```
## What is mean total number of steps taken per day?


```r
# For this part of the assignment, you can ignore the missing values in the dataset.  
# 1. Calculate the total number of steps taken per day  
# 2. If you do not understand the difference between a histogram and a barplot, research the   difference between them. Make a histogram of the total number of steps taken each day  
# 3. Calculate and report the mean and median of the total number of steps taken per day  

library(xtable)
data.aggregate<- aggregate(data$steps, FUN=sum, by=list(date=data$date))
plot(data.aggregate$date,data.aggregate$x,type="h", main="Total number of steps taken each day", xlab = "Date", ylab="Total Number Of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
data.mean<- aggregate(data$steps, FUN=mean, by=list(date=data$date))
colnames(data.mean)<-c("Date","Mean.Steps")
```
The mean of the total number of steps taken per day is 10766.1886792.

The median of the total number of steps taken per day is 10765.

## What is the average daily activity pattern?


```r
# What is the average daily activity pattern?
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# 2. Which 5-minute interval, on average across all the days in the dataset, contains the  maximum number of steps?

data.aggregateByInterval<- aggregate(data$steps, FUN=mean, by=list(Interval = data$interval), na.rm=TRUE)
summary(data.aggregateByInterval)
```

```
##     Interval            x          
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

```r
colnames(data.aggregateByInterval)<-c("Interval","Mean.Steps")
plot(data.aggregateByInterval$Interval,data.aggregateByInterval$Mean.Steps,type="l", main="Average Number of Steps Taken for each 5 minute \ninterval across all days", xlab="Interval", ylab="Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

835 is the 5 minutes interval contain the maximum number of steps

## Imputing missing values


```r
# Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```
Total number of missing values in the dataset is 2304 rows.


```r
data.na<-data[is.na(data$steps),]
data.na<-data.na[,c(2:3)]
data.nona<-data[!is.na(data$steps),]
data.merge<-merge(data.na, data.aggregateByInterval, by.x="interval", by.y="Interval")
data.merge<-data.merge[c(3,2,1)]
colnames(data.merge)<-c("steps","date","interval")
data.new<-rbind(data.nona,data.merge)
data.newaggregate<- aggregate(data.new$steps, FUN=sum, by=list(date=data.new$date))
plot(data.newaggregate$date,data.newaggregate$x,type="h", main="Total number of steps taken each day", xlab = "Date", ylab="Total Number Of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
  
The mean of the total number of steps taken per day is 10766.1886792.

The median of the total number of steps taken per day is 10766.1886792.

## Are there differences in activity patterns between weekdays and weekends?


```r
# Are there differences in activity patterns between weekdays and weekends?
# 1. For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
# 2. Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.
# 3. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

data.date<-weekdays(data.new$date)
data.new<-cbind(data.new,data.date)
data.date<- ifelse(data.new$data.date %in% c("Saturday", "Sunday"), "weekend", "weekday")
data.new<-cbind(data.new,data.date)
data.new<-data.new[,c(1,2,3,5)]
colnames(data.new)<-c("steps","date","interval","weekdayOrEnd")
data.newaggregateByInterval<- aggregate(data.new$steps, FUN=mean, by=list(Interval = data.new$interval, WeekdayOrEnd = data.new$weekdayOrEnd), na.rm=TRUE)

par(mfcol=c(2,1))
par(mar=c(1,3,3,0.5))
data.weekday<-data.newaggregateByInterval[data.newaggregateByInterval$WeekdayOrEnd=="weekday",]
data.weekend<-data.newaggregateByInterval[data.newaggregateByInterval$WeekdayOrEnd=="weekend",]
plot(data.weekday$Interval,data.weekday$x,type="l", main="Weekday", xlab="Interval", ylab="Average Number of Steps")
plot(data.weekend$Interval,data.weekend$x,type="l", main="Weekend", xlab="Interval", ylab="Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 



