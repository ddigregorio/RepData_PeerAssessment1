# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
```


------ 

1. Calculate the total number of steps taken per day

```r
stepsperday<-by(data$steps, data$date, function(x) sum(x) )
```


```r
## we now have a "by" class and need to convert to dataframe
stepsdf<-data.frame(cbind(stepsperday))
print("the total number of steps per day is: ")
```

```
## [1] "the total number of steps per day is: "
```

```r
print( stepsdf)
```

```
##            stepsperday
## 2012-10-01          NA
## 2012-10-02         126
## 2012-10-03       11352
## 2012-10-04       12116
## 2012-10-05       13294
## 2012-10-06       15420
## 2012-10-07       11015
## 2012-10-08          NA
## 2012-10-09       12811
## 2012-10-10        9900
## 2012-10-11       10304
## 2012-10-12       17382
## 2012-10-13       12426
## 2012-10-14       15098
## 2012-10-15       10139
## 2012-10-16       15084
## 2012-10-17       13452
## 2012-10-18       10056
## 2012-10-19       11829
## 2012-10-20       10395
## 2012-10-21        8821
## 2012-10-22       13460
## 2012-10-23        8918
## 2012-10-24        8355
## 2012-10-25        2492
## 2012-10-26        6778
## 2012-10-27       10119
## 2012-10-28       11458
## 2012-10-29        5018
## 2012-10-30        9819
## 2012-10-31       15414
## 2012-11-01          NA
## 2012-11-02       10600
## 2012-11-03       10571
## 2012-11-04          NA
## 2012-11-05       10439
## 2012-11-06        8334
## 2012-11-07       12883
## 2012-11-08        3219
## 2012-11-09          NA
## 2012-11-10          NA
## 2012-11-11       12608
## 2012-11-12       10765
## 2012-11-13        7336
## 2012-11-14          NA
## 2012-11-15          41
## 2012-11-16        5441
## 2012-11-17       14339
## 2012-11-18       15110
## 2012-11-19        8841
## 2012-11-20        4472
## 2012-11-21       12787
## 2012-11-22       20427
## 2012-11-23       21194
## 2012-11-24       14478
## 2012-11-25       11834
## 2012-11-26       11162
## 2012-11-27       13646
## 2012-11-28       10183
## 2012-11-29        7047
## 2012-11-30          NA
```


2. Make a histogram of the total number of steps taken each day

```r
hist(stepsdf$stepsperday, main="Histogram of Steps Per Day", xlab="steps", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The Chart above shows that 10k-15k steps were taken on more than 25 different days during the study.


3. Calculate and report the mean and median of the total number of steps taken per day

```r
    options(scipen=999)
    meanstepsperalldays<-mean(stepsdf$stepsperday, na.rm=TRUE)
    medianstepsperday<-median(stepsdf$stepsperday, na.rm=TRUE)
```
The mean steps per day is 10766.1886792.   

The median steps per day is 10765.     


------  

## What is the average daily activity pattern?  
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). 
+ Get the average of steps per interval
+ ConverT the "by" type to a dataframe
+ Since intervals are located only as row names create a column in the data frame of the row names
+ Rename the new column as "interval"
+ Convert column interval from factor to numeric
+ change row names to 1 through max (OPTIONAL)

```r
meanstepsperinterval<-by(data$steps, data$interval, function(x) mean(x, na.rm=TRUE) )
meandf<- data.frame(cbind(meanstepsperinterval))
#intervals are in the 1st col (rownames), so we must create a col of that.
meandf<-cbind(rownames(meandf), meandf)

#rename the 1st column
names(meandf)[1]<-"interval"

##interval is a factor so we need to make it and int
meandf$interval <- as.numeric(as.character(meandf$interval))

### null the rownames (not really needed)
# rownames(meansdf)<- NULL
```

Plot the data on a time series graph.

```r
plot(meanstepsperinterval~interval, data=meandf, type="l", main="Steps at 5 Min Intervals Across All Days", xlab="Time (mins)" , ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


Get the index of the maximum average of steps. 
Use the index to find the value of interval

```r
maxindex <- which(meandf$meanstepsperinterval == max(meandf$meanstepsperinterval))
## get that value from the interval
maxvalue <- meandf$interval[maxindex]
##can verify using this
###meandf[meandf$interval < 870 & meandf$interval> 810,]
```

The greatest mean value of steps occurs at interval 835. This can be confirmed using the graph above where the greatest peak occurrs about 3/4 between 500 and 1000 minutes.  



------  

## Imputing Missing Values   
#### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data. 

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs) 

```r
noOfNa<-sum(is.na(data$steps))
```
The total number of rows missing data is: 2304. 

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- reviewing the data in a couple different ways, I chose to use the median value per interval. The data appears to have many outliers. Given the few number of observation and the outliers the median was the best choice to use for the missing values. 

+ Get the median per interval. 
+ Need to convert the resulting "by' variable to a dataframe.  
+ Need to get the rownames, which are the intervals, and create a column with rownames as the data.  
+ Rename the column to "intervals".  
+ Convert the interval column from factor to numeric.  

```r
# use median because it is more representative of the tyipcal value
median.step.per.interval<-by(data$steps, data$interval, function(x) median(x, na.rm=TRUE) )
median.df<- data.frame(cbind(median.step.per.interval))
#intervals are in the 1st col (rownames), so we must create a col of that.
median.df<-cbind(rownames(median.df), median.df)

#rename the 1st column
names(median.df)[1]<-"interval"

##interval is a factor so we need to make it and int
median.df$interval <- as.numeric(as.character(median.df$interval))
```

+ Create a copy of the original data set.  
+ Replace all missing values with median values for that specific interval.  

```r
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
dataimputed<-data

### gotta loop
for(qq in 1:nrow(dataimputed))
{
    #if(is.na(dataimputed[qq, dataimputed$steps ])) ## if the steps value in the master df is na
    if(is.na(dataimputed[qq,1]))
    {
        ## here we know we have an na
        ## get the interval number
        index<-which( dataimputed[10,3] == median.df$interval)
        dataimputed[qq,1] <- median.df$median.step.per.interval[index]
        
    }
}
```



4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

+ Condense the data to the sum of the steps per day. 
+ Convert to Dataframe

```r
stepsperday2<-by(dataimputed$steps, dataimputed$date, function(x) sum(x) )
## we now have a "by" class and need to convert to dataframe
stepsdf2<-data.frame(cbind(stepsperday2))
#print("steps per day is: ")
#print( stepsdf2)
```

+ Make a histogram of the total number of steps taken each day  

```r
hist(stepsdf2$stepsperday2, main="Histogram of Steps Per Day", xlab="steps", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

+ Comparing the histogram of the imputed data to the Chart of data with the missing values, the imputed chart now shows to be slightly skewed to the right. Typically this is occurs when the median of the data is less than the mean.  


+ Calculate and report the mean and median of the total number of steps taken per day.  

```r
meanstepsperalldaysimputed<-mean(stepsdf2$stepsperday2)
medianstepsperdayimputed<-median(stepsdf2$stepsperday2)
```
The mean of the imputed data is 9354.2295082.  
The median of the imputed data is 10395.  

+ Interestingly the calculated mean and median did not fall in a typical skewness case where the median is less than the mean. 







------ 
 
## Are there differences in activity patterns between weekdays and weekends?  
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.  


```r
## need to actually convert the date column to dates now
dataimputed$date <- as.Date(dataimputed$date)

## add a column with the days of the week
dataimputed$day <- weekdays(dataimputed$date)
```

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

+ Need to add another column. however this one takes the weekday and determines if it is the weeked or not.   

```r
dataimputed$weekpart <-  c('weekday', 'weekend') [ dataimputed$day %in% c("Saturday", "Sunday")+1L ] 
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

+ Here the data is split into two new data frames.  
+ the mean is calculated on the grouped by the interval.  

```r
weekends <- dataimputed[which(dataimputed$weekpart == 'weekend'), ]
Weadgg<- aggregate(steps~interval, data=weekends, FUN = mean)

weekdays <- dataimputed[which(dataimputed$weekpart == 'weekday'), ]
wdagg <- aggregate(steps~interval, data=weekdays, FUN = mean)
```
    
+ Now the data is sorted correctly, we can plot the differences between weekends and weekdays    

```r
par(mfrow = c (2,1))
par(mar= c(1,0,3,2))
par(cex = 0.8)
par(oma=c(6,4,2,0))
plot(steps~interval , data= wdagg, type="l", main="Week Days" )
plot(steps~interval , data= Weadgg, type="l", main="Week Ends" )
title("Mean Number of Steps per Interval", outer = TRUE, xlab = "Interval" , ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

+ The graphs show similar trends between intervals whether during the week or weekends. 
