Loading and Preprocessing the data

    unzip(zipfile="activity.zip")
    data <- read.csv("activity.csv")

What is mean total number of steps taken each day

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.2.4

    total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
    qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    mean(total.steps, na.rm=TRUE)

    ## [1] 9354.23

    median(total.steps, na.rm=TRUE)

    ## [1] 10395

What is average daily activity pattern

    library(ggplot2)
    averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                          FUN=mean, na.rm=TRUE)
    ggplot(data=averages, aes(x=interval, y=steps)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("average number of steps taken")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    #average across all days in dataset, 5 min interval containing max # steps?
    averages[which.max(averages$steps),]

    ##     interval    steps
    ## 104      835 206.1698

Inputting missing values

    missing <- is.na(data$steps)
    # How many missing
    table(missing)

    ## missing
    ## FALSE  TRUE 
    ## 15264  2304

    #replace each missing value with mean val of its 5 min interval
    fill.value <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps))
            filled <- c(steps)
        else
            filled <- (averages[averages$interval==interval, "steps"])
        return(filled)
    }
    filled.data <- data
    filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
    #Using filled dataset, histogram of number of steps taken each day
    total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
    qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    mean(total.steps)

    ## [1] 10766.19

    median(total.steps)

    ## [1] 10766.19

Differences in activity patterns between weekday/weekend?

    weekday.or.weekend <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
            return("weekday")
        else if (day %in% c("Saturday", "Sunday"))
            return("weekend")
        else
            stop("invalid date")
    }
    filled.data$date <- as.Date(filled.data$date)
    filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

Panel Plot including plots of average \# steps on weekday/weekend

    averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
    ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)
