library(plyr)
library(car)

treat <- read.csv("Data/Treatment.csv")
dates <- seq(as.Date("2016-3-4"), as.Date("2016-4-17"), by = 'day')
dates <- dates[weekdays(dates) %in% c("Monday", "Wednesday", "Friday")]


mergeDD <- ldply(dates, function(x) data.frame(Date = x, WeekDay = weekdays(x), treat))

WaterRes <- ddply(mergeDD, .(Date, co2), function(x) x[x$block %in% sample(unique(x$block), size = 4), ])
WaterRes <- WaterRes[, c("Date", "WeekDay", "co2", "block", "water", "species", "ID")]
write.csv(WaterRes, file = "Output/Table/WaterResume.csv", row.names = FALSE)

