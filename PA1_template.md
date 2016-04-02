# PA1_template
# Reproducible Research
# Assignment: Course Project 1
# Gopal Bhagavatula
# April 1, 2016

## Loading and preprocessing the data

#### Show any code that is needed to

1. Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
mydata = read.csv("activity.csv")

mydata$date_corr = as.Date(as.character(mydata$date))
```

## What is mean total number of steps taken per day?

#### For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
sum_by_date <- aggregate(steps ~ date_corr, mydata, sum, na.rm=TRUE)

avg_by_int <- aggregate(steps ~ interval, mydata, mean, na.rm=TRUE)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean_orig <- mean(sum_by_date$steps)
median_orig <- median(sum_by_date$steps)

mean_orig
```

```
## [1] 10766.19
```

```r
median_orig
```

```
## [1] 10765
```

### What is the average daily activity pattern?  

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avg_by_int[which.max(avg_by_int$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

**The 5 minute interval 835 has the maximum number of average steps of 206.17**


### Imputing missing values

#### Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


```r
tot_obs <- nrow(mydata)
tot_na_orig <- sum(is.na(mydata$steps))
pct_na_orig <- mean(is.na(mydata$steps))

tot_obs
```

```
## [1] 17568
```

```r
tot_na_orig
```

```
## [1] 2304
```

```r
pct_na_orig*100
```

```
## [1] 13.11475
```

**2,304 out of 17,568 observations or 13.11% of the observations had NA in steps.**

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
mydata$steps_corr = mydata$steps

for (i in 1:nrow(mydata)) {
  mydata[i,5] =
    (
      ifelse (is.na(mydata[i,1]), 
             (avg_by_int[which(grepl(mydata[i,3],avg_by_int$interval)),2]),
             mydata[i,1])
     )
}

sum_by_date2 <- aggregate(steps_corr ~ date_corr, mydata, sum, na.rm=TRUE)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)



```r
mean_new <- mean(sum_by_date2$steps_corr)
median_new <- median(sum_by_date2$steps_corr)

mean_new
```

```
## [1] 10766.19
```

```r
median_new
```

```
## [1] 10766.19
```

**The mean did not change much as a result of imputing data into the NAs. The median changed slightly to become equal to the mean.**


### Are there differences in activity patterns between weekdays and weekends?

#### For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
mydata$Day = weekdays(mydata$date_corr)
Weekend = c("Saturday", "Sunday")
mydata$Day2 = ifelse(mydata$Day %in% Weekend, "Weekend", "Weekday")

avg_by_intandday <- aggregate(steps ~ interval+Day2, mydata, mean, na.rm=TRUE)
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)
