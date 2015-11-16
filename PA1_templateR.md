```{r}

library(knitr)
library(dplyr)
library(lattice)
library(lubridate)
library(data.table)


print(getwd())
setwd("K:/CBA/Membership Dashboard/analysis_with_R/Coursera/Reproducible Research/Week 2/peer assignment 1/")
print(getwd())


activity_data <- read.csv("K:/CBA/Membership Dashboard/analysis_with_R/Coursera/Reproducible Research/Week 2/peer assignment 1/data/activity.csv",stringsAsFactors=TRUE)


attach(activity_data)

print(sum(is.na(activity_data)))

activity <- as.data.frame(activity_data[rowSums(is.na(activity_data)) != ncol(activity_data),])
is.na(activity) = activity=='?'
is.na(activity) = activity==' ?'
is.na(activity) = activity=='? '
activity <- na.omit(activity)

print(summary(activity))
print(str(activity))
print(nrow(activity))
print(sum(is.na(activity)))
  
print("Total number of steps taken per day")
total_steps <- tapply(activity$steps,activity$date,sum, na.rm=TRUE)
print(total_steps)

print("Histogram of the total number of steps taken per day")
hist(total_steps,col="red",xlab="(Total Number of (Steps Taken))",main = "Histogram of the Total Number of Steps",breaks=20)

print("Mean and median of the total number of steps taken per day")
print(mean(total_steps,na.rm=TRUE))
print(median(total_steps,na.rm=TRUE))


print("Aggregating steps by interval")
means <- aggregate(list(steps=activity$steps), by=list(interval=activity$interval),mean,na.rm=TRUE)
# 
# 
# print(str(means))
# 
print("Time series plot of the average number of steps taken")
#plot(means$interval,means$steps,type="l")
plot(means,type="l",col="green",lwd=2,xlab="5-minute interval",main="Time series of the average number of steps")

print("Maximum number of steps")
print(means[which.max(means$steps),])
# 
# 

print("Number of NA values")
print(sum(is.na(activity_data$steps)))      

# print("Replace NA by means")
# print(sum(is.na(activity_data$steps)))
# print(sum(is.na(activity_data$date)))
# print(sum(is.na(activity_data$interval)))
# 
for(i in 1:ncol(activity_data)){
  activity_data[is.na(activity_data[,i]), i] <- mean(activity_data[,i], na.rm = TRUE)
}
# 
clean <- activity_data
# 
# print(sum(is.na(activity_data)))
print(sum(is.na(clean)))
# 
# print(summary(clean))
# 
print("Total number of steps taken per day in clean")
total_steps_clean <- tapply(clean$steps,clean$date,sum, na.rm=TRUE)
# print(total_steps_clean)
# 
# #plot(hist(total_steps_clean,breaks=50))
# #plot(hist(total_steps_clean,breaks=50,main = "Histogram of the (Total Number of (Steps Taken Each Day))",col="blue",xlab = "(Total Number of (Steps Taken Each Day))"))
print("Histogram of the total number of steps taken per day in clean")
hist(total_steps_clean,col="blue",xlab="(Total Number of (Steps Taken))",main = "Histogram of the Total Number of Steps",breaks=20)


print(head(activity_data))

temp <- as.POSIXlt(activity_data$date, format = "%Y-%m-%d")
tmpWeekDays <- temp$wday
tmpWeekDays[tmpWeekDays == 0] = 0
tmpWeekDays[tmpWeekDays == 6] = 0
tmpWeekDays[tmpWeekDays != 0] = 1
tmpWeekDaysFactor <- factor(tmpWeekDays, levels = c(0, 1))
# Add the factor variable to the data
activity_data$WD <- tmpWeekDaysFactor
# Calculate the mean
stepsMeanPerWeekday <- tapply(activity_data$steps, list(activity_data$interval, activity_data$WD), mean, 
                               na.rm = TRUE)

par(mfrow = c(2, 1))
# Display the 2 plots
with(activity_data, {
  par(mai = c(0, 1, 1, 0))
  plot(stepsMeanPerWeekday[, 1], type = "l", main = ("Steps vs. Interval"), 
       xaxt = "n", ylab = "Week ends")
  title = ("# of Steps v.s. Interval")
  par(mai = c(1, 1, 0, 0))
  plot(stepsMeanPerWeekday[, 2], type = "l", xlab = "Interval", ylab = "Week days")
  
})


detach(activity_data)
```

