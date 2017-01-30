---
title: "Project1"
author: "xujz4f"
date: "January 25, 2017"
output: md_document
---
#import activity as data
#remove NA

data1<-data[complete.cases(data),]
data1$date<-as.Date(data1$date,format="%Y-%m-%d")

#What is mean total number of steps taken per day?
#1. Calculate the total number of steps taken per day
dailysteps<-aggregate(steps~date,data1,sum)


#2.Make a histogram of the total number of steps taken each day
hist(dailysteps$steps,main="Histogram of total number of steps per day", xlab="Steps per day")
![unnamed-chunk-2-1](https://cloud.githubusercontent.com/assets/24257469/22413649/8217c900-e66e-11e6-8858-f05c02744cf5.png)

#3Calculate and report the mean and median of the total number of steps taken per day
summary(dailysteps)
Mean of total number of steps per day is 10766, median is 10765.

#What is the average daily activity pattern?
#1
stepsinterval<-aggregate(steps ~interval,data1, mean)
plot(x=stepsinterval$interval,y=stepsinterval$steps,type="l",xlab="5-minute interval ",ylab="average number of steps",main="Average number of steps over all days")

![unnamed-chunk-2-2](https://cloud.githubusercontent.com/assets/24257469/22413667/9e69fe8e-e66e-11e6-880d-a49b0dab3fd1.png)
The 5 minute interval with the maximum average number of steps across all days is 835.

#Imputing missing values
#1
sum(is.na(data2))
#2,3 
data2<-data
for (i in 1:nrow(data2)){
  if (is.na(data2$steps[i])){
    stepsvalue<-stepsinterval$steps[which(stepsinterval$interval==data2$interval[i])]
    data2$steps[i]=stepsvalue
  }
}

#4
dailysteps1<-aggregate(steps~date,data2,sum)
hist(dailysteps1$steps,main="Histogram of total number of steps per day", xlab="Steps per day")
summary(dailysteps1)
![unnamed-chunk-2-3](https://cloud.githubusercontent.com/assets/24257469/22413669/a2629834-e66e-11e6-9e5d-54d194184253.png)
Mean Steps Per Day using imputed data is 10766. Median Steps Per Day using Imputed data is 10766. These values are different from the estimates in the first part of the assignment and result in mean and median being equal.

#Are there differences in activity patterns between weekdays and weekends?
#1
data2$date<-as.Date(data2$date,format="%Y-%m-%d")
data2$weekday<-weekdays(data2$date)
data2$weekday[data2$weekday %in% c("Saturday","Sunday")]<-"weekend"
data2$weekday[data2$weekday != "weekend"]<-"weekday"

#2
stepsinterval1<-aggregate(steps~interval+weekday,data2,mean)
xyplot(stepsinterval1$steps~stepsinterval1$interval|stepsinterval1$weekday,type="l",xlab="5-minute interval ",ylab="average number of steps",main="Average number of steps",layout=c(1,2) )

![unnamed-chunk-2-4](https://cloud.githubusercontent.com/assets/24257469/22413674/a522ed3a-e66e-11e6-90cd-7edab82a2344.png)

