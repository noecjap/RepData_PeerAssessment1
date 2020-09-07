###LOading and preprocesing data

unzip(zipfile = "activity.zip", exdir = getwd())
amd<-read.csv("activity.csv")

library(data.table)
library(dplyr)
library(datasets)
library(lubridate)

##transform variables
amd$date <-as.Date(amd$date,format="%Y-%m-%d")
#amna<-amd[!is.na(amd$steps),]

#amstp1 <- summarise(group_by(amna, date), steps=sum(steps))
#mean(amstp1$steps)
#summary(amstp1$steps)

#Make a histogram of the total number of steps taken each day

amstp <- summarise(group_by(amd, date), steps=sum(steps))
mean(amstp$steps, na.rm = TRUE)
hist(amstp$steps,breaks = "Sturges")

summary(amstp$steps)

### What is the average daily activity pattern?

##1. Make a time series plot (i.e. `type = "l"`) of the 5-minute 
#interval (x-axis) and the average number of steps taken, averaged 
#across all days (y-axis)

library(ggplot2)

amint<-summarise(group_by(amd, interval), stepm=mean(steps, na.rm=TRUE))

plot(x = amint$interval, y = amint$stepm, type = "l", col = "navy", lwd = 2, 
          xlab = "5-minute Time Interval", ylab = "Average Steps", main = "Daily Average Steps Activity Pattern")


##2. Which 5-minute interval, on average across all the days 
#in the dataset, contains the maximum number of steps?
MaxInt <- amint$interval[which.max(amint$stepm)]



##Calculate and report the total number of missing values in the 
##dataset (i.e. the total number of rows with NAs)

library(mice)

table(is.na(amd))
md.pattern(amd)



#Devise a strategy for filling in all of the missing values in 
#the dataset. The strategy does not need to be sophisticated. For example,
#you could use the mean/median for that day, or the mean for that 5-minute 
#interval, etc.

amdImp<-amd

amdImp$steps.Imp <-ifelse(is.na(amdImp$steps), mean(amdImp$steps, na.rm=TRUE),
                          amdImp$steps)

##Create a new dataset that is equal to the original dataset but 
##with the missing data filled in.
amdImp<-amdImp[,c(2,3,5)]



#Make a histogram of the total number of steps taken each day and 
#Calculate and report the mean and median total number of steps 
#taken per day. Do these values differ from the estimates from the first 
#part of the assignment? What is the impact of imputing missing data on 
#the estimates of the total daily number of steps?
  

amdImp.steps <- summarise(group_by(amdImp, date), steps=sum(steps.Imp))
mean(amdImp.steps$steps, na.rm = TRUE)
median(amdImp.steps$steps, na.rm = TRUE)
hist(amdImp.steps$steps,breaks = "Sturges")

summary(amdImp.steps$steps)


#Are there differences in activity patterns between weekdays and weekends?

#1.Create a new factor variable in the dataset with two levels -- "weekday" 
#and "weekend" indicating whether a given date is a weekday or weekend day.

amdImp$dayTp<-ifelse(weekdays(amdImp$date) %in% c("sábado", "domingo"), "Weekend", "Weekday")

#2.Make a panel plot containing a time series plot (i.e. type = "l") of the 
#5-minute interval (x-axis) and the average number of steps taken, averaged
#across all weekday days or weekend days (y-axis).

amdImp.dayTp<-summarise(group_by(amdImp, interval, dayTp), steps=mean(steps.Imp, na.rm=TRUE))

xyplot(steps~interval|dayTp, data=amdImp.dayTp, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")

daytp.mean<-(summarise(group_by(amdImp, dayTp), steps=mean(steps.Imp, na.rm=TRUE)))
