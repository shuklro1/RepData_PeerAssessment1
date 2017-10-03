Exploratory Analysis of daily movement data from a personal activity monitoring device
======================================================================================

Introduction
------------

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
"quantified self" movement - a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web
site:  
- Dataset: [Activity monitoring
data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
\[52K\]

The variables included in this dataset are:  
- steps: Number of steps taking in a 5-minute interval (missing values
are coded as NA)  
- date: The date on which the measurement was taken in YYYY-MM-DD
format  
- interval: Identifier for the 5-minute interval in which measurement
was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset.

### Code for reading in the dataset and/or processing the data

    # set the working directory
    #setwd("Desktop\\Coursera_Data_Science_Course\\Course 5 - Reproducible Research")
     
    # Url of the data zip file
    #url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
     
    # Downlaod zip file
    #download.file(url, destfile='activity.zip',method='curl')
     
    # Unzip the data file. Use read.csv() to load the data set into R
    activity <- read.csv("activity.csv")
    summary(activity)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

### What is mean total number of steps taken per day?

Note - For this part of the assignment, we will ignore the missing
values in the dataset.

1.  Calculate the total number of steps taken per day

<!-- -->

    #Total num. of steps/day
    ttl.steps.day <- aggregate(data=activity,steps~date,sum,na.rm = T)
     
    head(ttl.steps.day)

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    hist(ttl.steps.day$steps,
         xlab = "Total number of steps taken each day",
         main = "Histogram of the total number of steps taken each day",
         breaks = 10, col = "green")
    abline(v = mean(ttl.steps.day$steps),col = "black",lwd = 2)
    abline(v = median(ttl.steps.day$steps),col = "red",lwd = 2)
    legend(x = "topright", c("Histogram", "Mean", "Median"),
           col = c("green", "black", "red"),
           lwd = c(2, 2, 2))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

1.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    mean(ttl.steps.day$steps)

    ## [1] 10766.19

    median(ttl.steps.day$steps)

    ## [1] 10765

The values of mean and median are **10766** and **10765** respectively.
The mean and median values are very close, as we can see in histogram
also.

### What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute
    interval (x-axis) and the average number of steps taken, averaged
    across all days (y-axis)

<!-- -->

    #calculating average steps/interval
    avg.steps.interval <- aggregate(data=activity,steps~interval,mean,na.rm = T)
     
    #Time series plot
    library(ggplot2)
    ggplot(avg.steps.interval,aes(c(interval),steps)) +
      geom_line(col = "steelblue") +
      labs(x = "Interval", y = "Average number of Steps") +
      ggtitle("Average number of Steps/Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    max.step.interval <- subset(avg.steps.interval,steps == max(steps))

    max.step.interval

    ##     interval    steps
    ## 104      835 206.1698

On average 5-minute interval number **835** has the maximum number of
steps **206**.

### Imputing missing values

Note - There are a number of days/intervals where there are missing
values (coded as NA). The presence of missing days may introduce bias
into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs).

<!-- -->

    #Totl number of record having NAs
    sum(!complete.cases(activity))

    ## [1] 2304

Out of total **17,568** observations **2304** records have missing
values.

1.  Devise a strategy for filling in all of the missing values in
    the dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

We will replace the missing step values in dataset with the mean of num.
of steps for that 5-minute interval.

    #We already have calculated the average steps/interval
    avg.steps.interval <- aggregate(data=activity,steps~interval,mean,na.rm = T)

    activity_no_na <- activity

    #replacing NAs with mean 
    for (i in 1:nrow(activity_no_na))
    {
      if(is.na(activity_no_na$steps[i]))
      {
        #finding interval for missing step
        interval.to.search <- which(activity_no_na$interval[i] == avg.steps.interval$interval) 
        #assigning mean to missing step value
        activity_no_na$steps[i] <- avg.steps.interval[interval.to.search,]$steps
      }
    }

1.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

    head(activity_no_na)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

    incomp.records <- sum(!complete.cases(activity_no_na))

In the new data set **activity\_no\_na** there are **0** records with
missing values.

1.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- -->

    #Average steps/day
    avg.steps.day <- aggregate(data=activity_no_na,steps~date,sum)

    #Histogram
    hist(avg.steps.day$steps,
         xlab = "Total number of steps taken each day",
         main = "Histogram of the total number of steps taken each day",
         breaks = 10, col = "green")
    abline(v = mean(avg.steps.day$steps),col = "black",lwd = 2)
    abline(v = median(avg.steps.day$steps),col = "red",lwd = 2)
    legend(x = "topright", c("Histogram", "Mean", "Median"),
           col = c("green", "black", "red"),
           lwd = c(2, 2, 2))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    mean(avg.steps.day$steps)

    ## [1] 10766.19

    median(avg.steps.day$steps)

    ## [1] 10766.19

The values of mean and median for new dataset are **10766** and
**10766** respectively. As we can observe that the mean and median
values haven't changed much from before.

### Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels â€“
    â€œweekdayâ€ and â€œweekendâ€ indicating whether a given date is a
    weekday or weekend day.

<!-- -->

    # converting date into weekdays
    activity_no_na$weekday <- weekdays(as.Date(activity_no_na$date))

    # classifying days names into weekday or weekend
    for(i in 1:nrow(activity_no_na))
    {
      if (activity_no_na$weekday[i] %in% c("Saturday", "Sunday"))
      {activity_no_na$weekday[i] = "Weekend"}
      else
      {activity_no_na$weekday[i] = "Weekday"}
    }

    #converting the column into a factor variable.
    activity_no_na$weekday <- as.factor(activity_no_na$weekday)

1.  Make a panel plot containing a time series plot (i.e. type = "l") of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).

<!-- -->

    #Average steps/weekday + interval
    avg.steps.weekday <- aggregate(data=activity_no_na,steps~interval+weekday,mean)

    #Time series plot
    library(ggplot2)
    ggplot(avg.steps.weekday,aes(c(interval),steps)) +
      geom_line(col = "steelblue") + facet_grid(weekday ~ .) +
      labs(x = "Interval", y = "Number of Steps", title ="Average number of Steps/Interval") 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)

There seems to be no activity during 12:00 AM to 5:00 AM (0 to 500 on
interval scale) as the subject might be sleeping during this time. From
interval 500 (around 5:00 AM) the number of steps start increasing and
peaked around 830 (around 8:30 AM) on both weekdays and Weekends, which
suggest subject might be excersing or walk to his office.  
On weekdays on average the number of steps are low during 9:30 AM to
6:00 PM (930 to 1800 on interval scale) which suggests subject might be
having a desk job.On Weekends the subject is more active than on
weekdays as on average the number of steps are higher during 9:30 AM to
6:00 PM.
