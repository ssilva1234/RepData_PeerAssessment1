---
title: "PA1_template"
output:
  html_document:
    keep_md: true

---

## Introduction 
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This report makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this datase.

This report addresses the following questions:

1. Code for reading in the dataset and/or processing the data
2. Histogram of the total number of steps taken each day
3. Mean and median number of steps taken each day
4. Time series plot of the average number of steps taken
5. The 5-minute interval that, on average, contains the maximum number of steps
6. Code to describe and show a strategy for imputing missing data
7. Histogram of the total number of steps taken each day after missing values are imputed
8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report



### **Question 1** : Code for reading in the dataset and/or processing the data
Define working directory

```r
directory<-getwd()
```
URL to download the data

```r
url1<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
```
Download the data

```r
download.file(url1, destfile = file.path(directory, "activity_mon_data.zip"))
```
Unzip the data

```r
unzip("activity_mon_data.zip", exdir = directory)
```
Read the data 

```r
activity_data<-read.csv(file = "./activity.csv", header = TRUE)
```
Format date

```r
activity_data$date<-strptime(activity_data$date, "%Y-%m-%d")
```
Group data by date

```r
activity_data_byday<-dplyr::group_by(activity_data, date)
```

### **Question 2** : Histogram of the total number of steps taken each day

```r
stepsperday<-dplyr::summarise(activity_data_byday, totalsteps=sum(steps), .groups = "drop")
hist(stepsperday$totalsteps, main = "Histogram of the total number of steps taken each day", xlab = "steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

### **Question 3** : Mean and median number of steps taken each day

```r
mean_stepsperday<-as.integer(mean(stepsperday$totalsteps, na.rm = TRUE))
median_stepsperday<-median(stepsperday$totalsteps, na.rm = TRUE)
```
The mean is 10766 and the median is 10765

### **Question 4** : Time series plot of the average number of steps taken

```r
library(ggplot2)
activity_data_byinterval<-dplyr::group_by(activity_data, interval)
stepsbyinterval<-dplyr::summarise(activity_data_byinterval, totalsteps=sum(steps, na.rm = TRUE), .groups = "drop")
ggplot(stepsbyinterval, aes(interval, totalsteps)) + geom_line() + xlab("Time")+ylab("Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### **Question 5** : The 5-minute interval that, on average, contains the maximum number of steps

```r
maximum<-subset(stepsbyinterval, totalsteps==max(stepsbyinterval$totalsteps))
library(knitr)
kable(maximum, caption = "The 5-minute interval that contains the maximum number of steps on average across all the days in the dataset:", align = "l")
```



Table: The 5-minute interval that contains the maximum number of steps on average across all the days in the dataset:

|interval |totalsteps |
|:--------|:----------|
|835      |10927      |

### **Question 6** : Code to describe and show a strategy for imputing missing data

```r
total_missing_data<-sum(is.na(activity_data))
```
The total number of missing values in the dataset is 2304

The strategy for filling in all of the missing values in the dataset was to use the mean for that 5-minute interval.

```r
steps_average_byinterval<-dplyr::summarise(activity_data_byinterval, average_steps=mean(steps, na.rm = TRUE), .groups="drop")
datawithoutNAs<-activity_data
for (i in 1:nrow(datawithoutNAs)) {
  if(is.na(datawithoutNAs$steps[i])) {
    val <- steps_average_byinterval$average_steps[which(steps_average_byinterval$interval == datawithoutNAs$interval[i])]
    datawithoutNAs$steps[i] <- val 
  }
}
```
### **Question 7** : Histogram of the total number of steps taken each day after missing values are imputed

```r
datawithoutNAs_byday<-dplyr::group_by(datawithoutNAs, date)
stepsperday_withoutNAs<-dplyr::summarise(datawithoutNAs_byday, totalsteps=sum(steps), .groups="drop")
hist(stepsperday_withoutNAs$totalsteps, main = "Histogram of the total number of steps taken each day", xlab = "steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


```r
mean_stepsperday_withoutNAs<-as.integer(mean(stepsperday_withoutNAs$totalsteps, na.rm = TRUE))
median_stepsperday_wihoutNAs<-as.integer(median(stepsperday_withoutNAs$totalsteps, na.rm = TRUE))
```
The mean is 10766 and the median is 10766.
Imputing missing data has no major impact.

### **Question 8** : Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
datawithoutNAs$weekdays<-weekdays(datawithoutNAs$date)
datawithoutNAs$weekdays[datawithoutNAs$weekdays  %in% c('Saturday','Sunday') ] <- "weekend"
datawithoutNAs$weekdays[datawithoutNAs$weekdays != "weekend"] <- "weekday"
datawithoutNAs$weekdays<-as.factor(datawithoutNAs$weekdays)
datawithoutNAs_byinterval_weekday<-aggregate(steps ~ interval + weekdays, datawithoutNAs, mean)

ggplot(datawithoutNAs_byinterval_weekday, aes(interval, steps)) +
  geom_line(aes(colour = weekdays)) +
  facet_grid(weekdays ~ ., scales="fixed", space="fixed") +
  labs(x="Interval", y="Number of Steps") +
  ggtitle("Number of steps Per Interval by day type")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

### **Question 9** :  All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
The R code needed to reproduce all this results is described in this report

The end



