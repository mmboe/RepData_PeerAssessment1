---
title: "PA1_template"
author: "Maureen McAndrew"
date: "4/9/2021"
output:
  html_document:
    keep_md: yes
  keep_md: default
---  
  


Load data

```r
unzip("C://Users/maure/Documents/R/Course 5 JHU Week 2 project/repdata_data_activity.zip")
reprodata <- read.csv("activity.csv",  header = TRUE, sep = ",")
```
change class of data variable

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 4.0.5
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
reprodata$date <- as.Date(reprodata$date)
```
create a histogram of total steps per day

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.4
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
steps_day <- reprodata %>% group_by(date) %>% summarize(totalsteps = sum(steps))
steps_day_hist <- hist(steps_day$totalsteps, main = "Total Steps per Day")
```

![](PA1_template_files/figure-html/hist1-1.png)<!-- -->
Calculate mean and median number of steps per day

```r
a <- mean(steps_day$totalsteps, na.rm = TRUE)
b <- median(steps_day$totalsteps, na.rm = TRUE)
```
Remove na values from reprodata

```r
nonareprodata <- na.omit(reprodata)
```
Get mean number of steps taken averaged across all days 

```r
nonareprodata1 <- nonareprodata %>% group_by(date) %>% summarize(mean = mean(steps))
```
Create a time series plot of average steps per day

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.4
```

```r
plot2 <- ggplot(nonareprodata1, aes(date, mean))+
geom_line()+
geom_point()+
scale_x_date(date_labels = "%Y %b %d")+
labs(title = "Average Number of Steps Taken per Day", 
  x = "Date", 
  y = "Mean Steps per Day")
print(plot2)
```

![](PA1_template_files/figure-html/plot2-1.png)<!-- -->
Determine the 5 minute interval that gives the maximum number of steps

```r
library(dplyr)
max <- nonareprodata %>% group_by(interval, date) %>% summarize(max_steps = max(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
range (max$max_steps)
```

```
## [1]   0 806
```

```r
max %>% filter(max_steps == 806)
```

```
## # A tibble: 1 x 3
## # Groups:   interval [1]
##   interval date       max_steps
##      <int> <date>         <int>
## 1      615 2012-11-27       806
```
Create a histogram of total steps per day with imputed (used mean) missing 
values. Then calculate new median and mean of total steps for imputed dataset.

```r
library(dplyr)
reprodata$steps [is.na(reprodata$steps)] <- mean(reprodata$steps, na.rm = TRUE)
steps_day_impute <- reprodata %>% group_by(date) %>% summarize(totalsteps = sum(steps))
hist_imputed <- hist(steps_day_impute$totalsteps, main = "Total Steps Per Day with Imputed Values", xlab = "Total Steps per day")
```

![](PA1_template_files/figure-html/hist2-1.png)<!-- -->

```r
print(hist_imputed)
```

```
## $breaks
## [1]     0  5000 10000 15000 20000 25000
## 
## $counts
## [1]  5 12 36  6  2
## 
## $density
## [1] 1.639344e-05 3.934426e-05 1.180328e-04 1.967213e-05 6.557377e-06
## 
## $mids
## [1]  2500  7500 12500 17500 22500
## 
## $xname
## [1] "steps_day_impute$totalsteps"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```
Get mean and median of total steps with imputed values

```chunk10
c <- mean(steps_day_impute$totalsteps)
d <- median(steps_day_impute$totalsteps)
c - a
d - b
# no mean difference between imputed values and removed na, but median difference is 1.188679
```
To create comparison panel plot between weekend and weekday values, first must separate dates into weekend and weekdays

```r
reprodata$date <- as.Date(reprodata$date)
library(dplyr)
reprodataw <- reprodata %>% mutate(weekday = weekdays(date))
reproweekday <- reprodataw %>% mutate(day = factor(ifelse(weekday == 
      c("Saturday", "Sunday"), "Weekend", "Weekday")))
```
Get mean number of steps by weekend or weekday according to time interval

```r
weekday <- reproweekday %>% group_by (interval, day) %>% summarize(mean_steps = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```
Create panel plot comparing time intervals for weekdays vs. weekends

```r
library(ggplot2)
pplot <- ggplot(weekday, aes(interval, mean_steps, color = day))+
geom_line()+
facet_wrap(~day)+
ggtitle("Average Steps per 5 Minute Interval")+
xlab ("Time Intervals")+
ylab("Average Number of Steps")
print(pplot)
```

![](PA1_template_files/figure-html/pplot-1.png)<!-- -->
