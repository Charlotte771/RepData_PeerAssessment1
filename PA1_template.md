

```r
activity <- read.csv("~/Desktop/activity.csv")
View(activity)
library(ggplot2)
```

```{r-total steps/day}
s_d <- aggregate(activity$steps, activity$date, FUN=sum)
colnames(s_d) <- c("Date", "Steps")
View(s_d)
```

```{r-histogram of total steps per day}
qplot(s_d$Steps, geom = "histogram", xlab = "Total Steps", ylab = "Frequency", main="Total Steps Per Day")
```

```{r- mean and median steps per day}
mean(s_d$Steps)
median(s_d$Steps)
```

```{r-steps per interval}
s_i <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
colnames(s_i) <- c("interval", "average steps")
s_i_plot <- plot(s_i, type="l", main="Average Steps Per Interval")
```
The maximum number of steps occurs during the interval 835.

```{r- total number of NAs}
summary(activity)
```

```{r- NA filled in, new data set}
replace_steps <- s_i$`average steps`[match(activity$interval, s_i$interval)]
inputed <- transform(activity, steps=ifelse(is.na(activity$steps), yes=replace_steps, no=activity$steps))
View(inputed)
```

```{r-histogram with missing values}
input_sum <- aggregate(steps~date, inputed, sum)
qplot(input_sum$steps, geom = "histogram", xlab = "Total Steps", ylab = "Frequency", main="Total Steps Per Day Without Missing Values", breaks=seq(0,25000,by=2500))
```

```{r- mean and median for steps/day without missing values}
noNA_mean <- mean(input_sum$steps)
noNA_median <- median(input_sum$steps)
NA_mean <- mean(s_d$Steps)
NA_median <- median(s_d$Steps)
compare <- data.frame(mean=c(noNA_mean, NA_mean), median=c(noNA_median, NA_median))
rownames(compare) <- c("NA_included", "NA_substitute")
print(compare)
```
The mean remains the same when the NAs are replaced with the mean total steps/day however, the median is slightly lower when the NA values are replaced.By inputting the missing values there is a slight change in the estimates of total daily/steps.

```{r- weekdays/weekends}
inputed$date <- as.Date(inputed$date)
inputed$day <- ifelse(weekdays(inputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
inputed$day <- as.factor(inputed$day)
day_end <- aggregate(steps~interval+day, data=inputed, FUN=mean, na.action = na.omit)
plot1 <- ggplot(day_end, aes(interval, steps))
plot_day <- plot1+geom_line(col="blue")+ggtitle("Steps: Weekend vs. Weekday")+xlab("Time Interval")+ylab("Average Steps")+facet_grid(day ~ .)
print(plot_day)
```
