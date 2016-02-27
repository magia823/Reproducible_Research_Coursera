---
  title: "Reproducible research-Project 1"
author: "Magia Kotsits"
date: "February 27, 2016"
output: html_document
---
  
  #Load data
  ```{r,  echo=TRUE}
# Load activity data
data <- read.csv("./activity.csv")
summary(data)
str(data)
```

#Preprocess data and prepare it for plotting
```{r,  echo=TRUE}
# Convert variable 'date'from factor to date
data$date <- as.Date(data$date)
# Load reshape2 library to use melt and dcast and ggplot for plotting
library(reshape2)
library(ggplot2)
# Melt data frame to prepare for casting by date:
# We set id variable to 'date'and measure variable to steps.
# We get a table with multiple values for steps taken each day.
#We ignore the missing values in the dataset.
data_melt <- melt(data, id.vars="date", measure.vars="steps", na.rm=FALSE)
# Cast data frame to find number of steps per day
data_cast <- dcast(data_melt, date ~ variable, sum)
```


#Histogram of the total number of steps taken each day

```{r,echo=TRUE}
# Plot histogram with frequency of steps by day. Blue line shows the mean value
plot(data_cast$date, data_cast$steps, type="h", main="Histogram of steps each day", xlab="Day", ylab="Number of steps per day",lwd=5)
abline(h=mean(data_cast$steps, na.rm=TRUE), col="blue", lwd=5)

```

#What is mean total number of steps taken per day?
```{r,echo=TRUE}
paste("Mean steps/day =", mean(data_cast$steps, na.rm=TRUE))
paste("Median steps/day =", median(data_cast$steps, na.rm=TRUE))
```
#What is the average daily activity pattern?

```{r, echo=TRUE}
# Re-melt data frame to prearep for casting by interval. Now we include NA values
data_melt2 <- melt(data, id.vars="interval", measure.vars="steps", na.rm=TRUE)
# Cast data frame to see mean steps per interval
data_cast2 <- dcast(data_melt2, interval ~ variable, mean)
# Plot time series of average frequency of steps per interval
plot(data_cast2$interval, data_cast2$steps, type="l", main="Frequency of Steps Taken per interval", 
     xlab="5 min interval", lwd=2)
abline(h=mean(data_cast2$steps, na.rm=TRUE), col="blue", lwd=2)
```

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r, echo=TRUE}

data_cast2[which.max(data_cast2$steps), ]
```

#Imput missing values
There are many days/intervals where there are missing values (coded as NA). The presence of missing days
may introduce bias into some calculations or summaries of the data. That is why we will imput missing
values by replacing them with the mean value.
```{r, echo=TRUE}
missing <- is.na(data$steps)
table(missing)
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps) else filled <- (data_cast2[data_cast2$interval == interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```

#Histogram 2

```{r, echo=TRUE}
#Now, we make a histogram using the filled data set with missing values.
plot(filled.data$date, filled.data$steps, type="h", main="Histogram of Daily Steps (with NAs)", xlab="Date",ylab = "Steps", lwd=3)
abline(h=mean(filled.data$steps), col="purple", lwd=3)
```

#Calculate mean and median of daily steps

```{r, echo=TRUE}
paste("Mean daily steps =", mean(filled.data$steps, na.rm=TRUE))

paste("Median daily steps =", median(filled.data$steps, na.rm=TRUE))

```

#Do these values differ from the estimates from the first part of the
#assignment? What is the impact of imputing missing data on the
#estimates of the total daily number of steps?

Mean and median values are higher after imputing missing data. The reason is that in the original data,
there are some days with steps values NA for any interval. The total number of steps taken in such days are
set to 0s by default but after replacing missing values with the mean, number of steps is not 0 any more, thus
the estimates are higher after imputing NAs.

#Are there differences in activity patterns between weekdays and weekends?
We first find the day of the week for each measurement. We use the dataset with the imputted NAs.
```{r, echo=TRUE}
weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday") else if (day %in% c("Saturday", "Sunday"))
      return("weekend") else stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN = weekday.or.weekend)
```

#Plot average number of steps taken on weekdays and weekends.

```{r, echo=TRUE}
averages <- aggregate(steps ~ interval + day, data = filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")

```
