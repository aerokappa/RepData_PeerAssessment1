#
# This is a file that contains the exact same code in the R markdown - this
# is to test out the script before it is inserted into the Rmd file.
#

stepsData <- read.csv("activity.csv", header=TRUE, na.strings="NA",
                      colClasses=c("integer","character","integer"))

# Merge the second and third columns to get the date in POSIXlt format

stepsData$datetime <- paste(stepsData$date,formatC(stepsData$interval,digits=3,
                                                   flag="0"))
stepsData$datetime <- strptime(stepsData$datetime, "%Y-%m-%d %H%M")

# Convert the original date and time columns to factors to enable manipulation by
# date and time

stepsData$date <- as.factor(stepsData$date)
stepsData$interval <- as.factor(stepsData$interval)

#
# Find indices corresponding to non-NA values
#

nonNAvalues <- complete.cases(stepsData$steps)

#
# Make a histogram of the total number of steps taken per day
#
nSteps <- tapply(stepsData$steps[nonNAvalues],stepsData$date[nonNAvalues],sum)
nSteps <- data.frame(date=as.Date(names(nSteps)),steps=nSteps)

library(ggplot2)
g <- ggplot(nSteps,aes(x=steps))
g + geom_histogram(binwidth=1000,color="blue",fill="dark grey")

#
# Report a summary of the data - showing the mean and median, among other things
#
summary(nSteps)

#
# get some sense of average steps per interval
#
nStepsInterval <- tapply(stepsData$steps[nonNAvalues],stepsData$interval[nonNAvalues],mean)
nStepsInterval <- data.frame(interval=formatC(as.integer(names(nStepsInterval)),digits=3,flag="0"),
                             steps=nStepsInterval)
nStepsInterval$interval <- as.POSIXct(strptime(paste(nSteps$date[length(nSteps$date)],
                                          nStepsInterval$interval),
                                    "%Y-%m-%d %H%M"))

#
# time series plot
#
g2 <- ggplot(nStepsInterval,aes(x=interval,y=steps))
g2 + geom_line() +labs(title="average number of steps as a function of the time interval") +
        scale_x_datetime(labels=c("00:00","06:00","12:00","18:00","00:00"))

#
#  Find the interval that corresponds to the maximum number of steps
#
maxLocation <- which.max(nStepsInterval$steps)
nStepsInterval$interval[maxLocation[[1]]]
nStepsInterval$steps[maxLocation[[1]]]

#
#  Find the number of NA's in the original data set.
#
sum(is.na(stepsData$steps))

#
#  Impute missing values by replacing NAs with the mean for that particular time 
#  interval as calculated previously and fill it in a new dataset
#
filledStepsData <- stepsData
naValues <- is.na(filledStepsData$steps)
filledStepsData$steps[naValues] <- 
        nStepsInterval$steps[as.character(filledStepsData$interval[naValues])]

#
# Make a histogram of the total number of steps taken per day
#
nSteps2 <- tapply(filledStepsData$steps,filledStepsData$date,sum)
nSteps2 <- data.frame(date=as.Date(names(nSteps2)),steps=nSteps2)

library(ggplot2)
g3 <- ggplot(nSteps,aes(x=steps))
g3 + geom_histogram(binwidth=1000,color="blue",fill="dark grey")

#
# Report a summary of the data - showing the mean and median, among other things
#
summary(nSteps2)

#
# classify days as weekday days and weekend days
#
filledStepsData$isWeekend <- as.factor(
        weekdays(filledStepsData$datetime) %in% c("Saturday","Sunday"))
levels(filledStepsData$isWeekend)=c("weekday","weekend")


#
#  split the data as weekday data and weekend data
#
separatedStepsData <- split(filledStepsData,filledStepsData$isWeekend)

#
# get some sense of average steps per interval
#
nStepsIntervalMWF <- tapply(separatedStepsData$weekday$steps,
                            separatedStepsData$weekday$interval,mean)

nStepsIntervalSS <- tapply(separatedStepsData$weekend$steps,
                           separatedStepsData$weekend$interval,mean)

nStepsIntervalFinal <- data.frame(interval=
                                          c(formatC(as.integer(names(nStepsIntervalMWF)),digits=3,flag="0"),
                                            formatC(as.integer(names(nStepsIntervalSS)),digits=3,flag="0")),
                                  steps=c(nStepsIntervalMWF,nStepsIntervalSS),
                                  weekday=rep(as.factor(c("weekday","weekend")),each=288))

nStepsIntervalFinal$interval <- as.POSIXct(strptime(paste(nSteps$date[length(nSteps$date)],
                                                     nStepsIntervalFinal$interval),
                                               "%Y-%m-%d %H%M"))

#
# time series plot
#
g4 <- ggplot(nStepsIntervalFinal,aes(x=interval,y=steps))
g4 + geom_line() +labs(title="average number of steps as a function of the time interval") +
        scale_x_datetime(labels=c("00:00","06:00","12:00","18:00","00:00"))+
        facet_wrap(~ weekday, nrow=1, ncol=2)