---
title: "Project1"
author: "xujz4f"
date: "January 25, 2017"
output: html_document
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

#3Calculate and report the mean and median of the total number of steps taken per day
summary(dailysteps)


#What is the average daily activity pattern?
#1
stepsinterval<-aggregate(steps ~interval,data1, mean)
plot(x=stepsinterval$interval,y=stepsinterval$steps,type="l",xlab="5-minute interval ",ylab="average number of steps",main="Average number of steps over all days")

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

#Are there differences in activity patterns between weekdays and weekends?
#1
data2$date<-as.Date(data2$date,format="%Y-%m-%d")
data2$weekday<-weekdays(data2$date)
data2$weekday[data2$weekday %in% c("Saturday","Sunday")]<-"weekend"
data2$weekday[data2$weekday != "weekend"]<-"weekday"

#2
stepsinterval1<-aggregate(steps~interval+weekday,data2,mean)
xyplot(stepsinterval1$steps~stepsinterval1$interval|stepsinterval1$weekday,type="l",xlab="5-minute interval ",ylab="average number of steps",main="Average number of steps",layout=c(1,2) )


