library(dplyr)
library(lattice)
library(knitr)

##Load and Process Data
activity<-read.csv("activity.csv", header=TRUE, stringsAsFactors=FALSE)
activity$date2<-as.factor(activity$date)
activity$date<-strptime(activity$date,format="%Y-%m-%d")

##Calculate total number of steps
step_total<-activity%>%select(-date)%>%group_by(date2)%>%summarise(Total.Steps=sum(steps))
step_total

##Create histogram
with(step_total,hist(Total.Steps, col="blue", breaks=10, main="Histogram of Total Steps"))

##Calculate Mean and Median
with(step_total,mean(Total.Steps,na.rm=TRUE))
with(step_total,median(Total.Steps,na.rm=TRUE))

##Calculate Mean for each 5 minute interval
step_avg<-activity%>%select(-date)%>%group_by(interval)%>%
          summarise(Avg.Steps=mean(steps,na.rm=TRUE))

##Plot time series of intervals and average steps
with(step_avg,plot(interval,Avg.Steps,type="l", main="Time Series of Average Steps"))

##Find time interval with largest number of average steps
max(step_avg$Avg.Steps)
filter(step_avg,Avg.Steps>=200)

## Calculate number of NAs
sum(is.na(activity$steps))

##Strategy: Replace NAs by mean of interval
activity2<-select(activity,-date)
activity2<-left_join(activity2,step_avg,by="interval")

activity.not.na<-activity2%>%filter(!is.na(steps))%>%mutate(newsteps=steps)
activity.na<-activity2%>%filter(is.na(steps))%>%mutate(newsteps=Avg.Steps)

activity.new<-bind_rows(activity.not.na,activity.na)%>%select(-steps,-Avg.Steps)%>%
              arrange(date2,interval)

##Calculate new total steps
step_total.new<-activity.new%>%group_by(date2)%>%summarise(Total.Steps=sum(newsteps))
step_total.new

##Create histogram for new total steps
with(step_total.new,hist(Total.Steps, col="green", breaks=10, main="Histogram of New Total Steps"))

##Calculate Mean and Median for new total steps
with(step_total.new,mean(Total.Steps,na.rm=TRUE))
with(step_total.new,median(Total.Steps,na.rm=TRUE))

##Yes, these values are different. 
##The mean is unchanged but the median now equals the mean.
##Overall the impact is very small since the mean and median were close to start with.

##Create weekday/weekend separation
activity.new<-activity.new%>%mutate(weekday=weekdays(ymd(date2)))
activity.new.wkd<-activity.new%>%filter(weekday!="Saturday",weekday!="Sunday")%>%mutate(day.type="Weekday")
activity.new.wke<-activity.new%>%filter(weekday=="Saturday"|weekday=="Sunday")%>%mutate(day.type="Weekend")
activity.new<-bind_rows(activity.new.wkd,activity.new.wke)%>%arrange(date2,interval)
activity.new$weekday<-as.factor(activity.new$weekday)
activity.new$day.type<-as.factor(activity.new$day.type)

##Calculate Mean for each 5 minute interval separated by Weekday and Weekend
step_avg.new<-activity.new%>%group_by(interval,day.type)%>%
  summarise(Avg.Steps=mean(newsteps))

##Create panel plot to compare Weekday and Weekend number of steps
xyplot(Avg.Steps~interval|day.type,data=step_avg.new,type="l", main="Weekday vs. Weekend Steps")

