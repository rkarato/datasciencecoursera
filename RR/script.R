library(lubridate)
library(dplyr)

filledact_interval <- filledact %>% select ( steps2, date, interval) %>% group_by(date, interval) %>% 
  summarise( steps2 = sum(steps2))

is.na(filledact_interval$steps2)

str(filledact_interval)
str(filledact)

#weekdays(as.Date(filledact_day$date,"%y-%m-%d"))
#install.packages("lubridate")
#lubridate::as_date(filledact_day$date)
filledact_interval$weekdays <- weekdays(lubridate::as_date(filledact_interval$date))

filledact_interval$isweekend <- factor(x = filledact_interval$weekdays, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday" ), 
       labels = c("Weekday","Weekday","Weekday","Weekday","Weekday","Weekend","Weekend"))

levels(filledact_interval$isweekend) <- list(Weekend = c("Saturday","Sunday" ), 
                                             Weekday = c("Monday","Tuesday","Wednesday","Thursday","Friday")) 
str(filledact_interval$isweekend)
#levels(x) <- list(Yes=c("Y", "Yes"), No=c("N", "No"))

weekend <- filledact_interval %>% select ( isweekend, interval, steps2 ) %>% filter(isweekend == "Weekend") %>% group_by ( isweekend,interval) %>% summarise( steps2 = sum(steps2))
weekday <- filledact_interval %>% select ( isweekend, interval, steps2 ) %>% filter(isweekend == "Weekday") %>% group_by ( isweekend,interval) %>% summarise( steps2 = sum(steps2))
head(weekend)
head(weekday)
nrow(weekend)
nrow(weekday)
mean(weekend$steps2)
mean(weekday$steps2)
sum(weekend$steps2)
sum(weekday$steps2)
View(filledact_interval)
max(weekday$steps2)
max(weekend$steps2)

mean(filledact$steps, na.rm = TRUE)
mean(weekday$steps2)

str(filledact_interval)

filledact_interval[filledact_interval$weekdays == "Sunday" && filledact_interval$interval == 5,]
      
with ( weekday, plot(interval, steps2, type='l', col = "blue"))
with ( weekend, points(interval, steps2, type='l', col = "red"))
dev.off()



head(act_min)
act_min <- act
act_min$steps2 <- ifelse(is.na(act_min$steps), 0, act_min$steps)
act_min <- act %>% select ( steps, date, interval) %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarize(steps = mean(steps) )

act_min$weekdays <- weekdays(lubridate::as_date(act_min$date))

filledact_interval$isweekend <- factor(x = filledact_interval$weekdays, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday" ), 
                                       labels = c("Weekday","Weekday","Weekday","Weekday","Weekday","Weekend","Weekend"))

levels(filledact_interval$isweekend) <- list(Weekend = c("Saturday","Sunday" ), 
                                             Weekday = c("Monday","Tuesday","Wednesday","Thursday","Friday")) 
