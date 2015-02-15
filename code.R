

activity <- read.csv("activity.csv")
print(dim(activity))
print(summary(activity))
print(head(activity))

# Total number of steps per day
daysums <- aggregate(activity$steps, list(activity$date), sum)
print(head(daysums))
hist(daysums$x, breaks=seq(from=0, to=22000, by=1000))

# Mean and median each day
totalmean <- mean(daysums$x, na.rm=TRUE)
print(totalmean)
totalmedian <- median(daysums$x, na.rm=TRUE)
print(totalmedian)

# Avg number of steps taken per 5-min interval
activity_nona <- na.omit(activity)
intavgs <- aggregate(activity_nona$steps, list(activity_nona$interval), mean)

par(mfrow=c(4,1))

plot(intavgs$Group.1, intavgs$x, type="l")
maxrow <- which.max(intavgs$x)
print(intavgs[maxrow,])

# Number of rows with missing values
row_na_count <- sum(!complete.cases(activity))
print(row_na_count)

# Replace missing step values by interval average
activity_replace_na <- activity
for(n in which(is.na(activity$steps))) {
    i <- activity_replace_na[n, "interval"]
    activity_replace_na[n, "steps"] <- intavgs[intavgs$Group.1==i, "x"]
}

# Total number of steps per day
daysums_replace_na <- aggregate(activity_replace_na$steps, list(activity_replace_na$date), sum)
print(head(daysums_replace_na))

hist(daysums$x, col=rgb(1,0,0,0.5), breaks=seq(from=0, to=22000, by=500))
hist(daysums_replace_na$x, col=rgb(0,0,1,0.5), breaks=seq(from=0, to=22000, by=500))
hist(daysums$x, col=rgb(1,0,0,0.5), breaks=seq(from=0, to=22000, by=500))
hist(daysums_replace_na$x, col=rgb(0,0,1,0.5), breaks=seq(from=0, to=22000, by=500), add=T)
box()

# Mean and median each day
totalmean_replace_na <- mean(daysums_replace_na$x, na.rm=TRUE)
print(totalmean_replace_na)
totalmedian_replace_na <- median(daysums_replace_na$x, na.rm=TRUE)
print(totalmedian_replace_na)

# Add weekday factor column
activity_days <- activity[!(is.na(activity$steps)),]
activity_days$day <- as.factor(weekdays(as.Date(activity_days$date)))

activity_weekday <- subset(activity_days, day=="Monday"|day=="Tuesday"|day=="Wednesday"|day=="Thursday"|day=="Friday")
activity_weekend <- subset(activity_days, day=="Saturday"|day=="Sunday")

intavgs_weekday <- aggregate(activity_weekday$steps, list(activity_weekday$interval), mean)
intavgs_weekend <- aggregate(activity_weekend$steps, list(activity_weekend$interval), mean)
plot(intavgs_weekday$Group.1, intavgs_weekday$x, col=rgb(1,0,0,0.5), type="l")
lines(intavgs_weekend$Group.1, intavgs_weekend$x, col=rgb(0,0,1,0.5))

