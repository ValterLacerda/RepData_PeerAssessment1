---
title: "Peer Assessment 1"
author: "Valter Lacerda"
date: "Sunday, August 17, 2014"
output: html_document
---

### Getting started
This code is a study for Peer Assessment 1 on Reproducible Research Coursera course.

First, we do some basic settings, so is possible to process the document.

```r
## turn off scientific notations
options(scipen=1)
## some libraries are needed for plots
library(lattice)
library(ggplot2)
```

### Loading and preprocessing the data
This chunk of code load the data and meke some processes to make data more usefull for the first analysis.


```r
unzip('activity.zip')
activity<-read.csv('activity.csv', colClasses=c("integer","Date","factor"))
activity$month <-as.numeric(format(activity$date,"%m"))
## excludes NA values
activity_noNA<-na.omit(activity)
rownames(activity_noNA)<-1:nrow(activity_noNA)
```

### What is mean total number of steps taken per day?

#### 1. Make a histogram of the number of steps taken each day


```r
## makes the plot
ggplot(activity_noNA, aes(date, steps)) + geom_bar(stat = "identity", colour = "green" , fill = "green", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "1.Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Number of steps")
```

![plot of chunk makes the plot and calculates the mean](figure/makes the plot and calculates the mean.png) 

#### 2. Calculate and report the <i>mean</i> and <i>median</i> total number of steps taken per day


```r
## it is needed to use a funcion to sum the total number of steps taken each day
totalstepsperday<-aggregate(activity_noNA$steps, list(Date=activity_noNA$date), FUN = "sum")$x
```


Mean

```r
m<-mean(totalstepsperday)
```
The mean is **10766.1887**.


Median

```r
med<-median(totalstepsperday)
```
The median is **10765**.



### What is the average daily activity pattern?

#### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days(y-axis)


```r
stepsbyinterval<- aggregate(activity_noNA$steps, list(interval = as.numeric(as.character(activity_noNA$interval))), FUN = "mean")
plot(stepsbyinterval$interval,stepsbyinterval$x,type='l',main="2. Average steps by 5-minute interval",xlab="5-minute interval",ylab="average steps")
```

![plot of chunk Average steps by interval](figure/Average steps by interval.png) 

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
sint<-stepsbyinterval[stepsbyinterval$x==max(stepsbyinterval$x),]$interval
steps<-stepsbyinterval[stepsbyinterval$x==max(stepsbyinterval$x),]$x
```
The 5-minute interval with the maximum number of steps is 835 with 206.1698 steps on average.

### Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
na<-sum(is.na(activity$steps))
```
The total number os missing values in the dataset is <b>2304</b>.

#### 2. Devise a strategy for filling in all the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or mean for that 5-minute interval, etc.

The strategy is to input the median of each 5-minute interval


```r
mediansteps<- aggregate(activity_noNA$steps, list(interval = as.numeric(as.character(activity_noNA$interval))), FUN = "median")
names(mediansteps)[2]<-"medianSteps"
```

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityNew<-activity
## for each NA on the dataset, the value of the median for the interval is inputed
for (row in 1:nrow(activityNew)){
    if (is.na(activityNew$steps[row])){
        activityNew$steps[row]<-mediansteps[mediansteps$interval==activityNew$interval[row],]$medianSteps
    }
}
```

#### 4. Make a histogram of the total number of steps taken each day and calculate and report the <b>mean</b> and the <b>median</b> total number of steps taken per day. Do these values differ from the estimates from the first part of the assingment? What is the impact of inputing missing data on the estimates of the total daily number of steps?


```r
## makes the plot
ggplot(activityNew, aes(date, steps)) + geom_bar(stat = "identity", colour = "darkgreen" , fill = "darkgreen", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "3.Histogram of Total Number of Steps Taken Each Day \n inputed data", x = "Date", y = "Number of steps")
```

![plot of chunk makes the plot and calculates the mean on inputed data](figure/makes the plot and calculates the mean on inputed data.png) 


```r
## it is needed to use a funcion to sum the total number of steps taken each day
totalsteps<-aggregate(activityNew$steps, list(Date=activityNew$date), FUN = "sum")$x
```


Mean

```r
mnew<-mean(totalsteps)
```
The new mean is **9503.8689**.


Median

```r
mednew<-median(totalsteps)
```
The new median is **10395**.


We can see from the mean and the median that since more low values were inputed for each day, the values were lower than when missings were ignored. But the differences are not so huge.

### Are there differences in activity patterns between weekdays and weekends?

#### 1. Create a new factor variable in the dataset with two levels "weekday" and "weekend" indicating whether a given date is a weekday or a weekend day.


```r
activityNew$weektime<-as.factor(ifelse(weekdays(activityNew$date,abbreviate=T) %in% c('sáb','dom'),"weekend","weekday"))
```

#### 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days (y-axis).


```r
## calculate mean by interval and factors
 meanSteps<- aggregate(activityNew$steps,list(interval=as.numeric(as.character(activityNew$interval)),weektime=activityNew$weektime),FUN="mean")
## ploting the panels
xyplot(meanSteps$x~meanSteps$interval|factor(meanSteps$weektime),type='l',layout=c(1,2),xlab="5-minute interval",ylab="4. Average number of steps",main="Average number of steps taken each day \nweekend x weekday")
```

![plot of chunk calculate the mean by interval and factors](figure/calculate the mean by interval and factors.png) 
 