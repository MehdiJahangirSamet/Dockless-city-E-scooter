# This program is part of data analytics which aims to provide datatime serries
# visualizations for all recorded routs

rm(list=objects())

library(dplyr)
library(ggplot2)

#Import all route data
dataset1<-read.csv("Dockless_With_Zones.csv", stringsAsFactors = FALSE, header = TRUE)
View(dataset1)
str(dataset1)
dataset1 <- mutate(dataset1, StartDateTime = paste(StartDate, StartTime))
str(dataset1)

#creat start data time
# Now we transform from text to date format variable
dataset1$StartDateTime <- strptime(dataset1$StartDateTime, "%m/%d/%Y %H:%M")
str(dataset1$StartDateTime)
dataset1$StartDateTime<-as.POSIXct(dataset1$StartDateTime)
class(dataset1$StartDateTime)
str(dataset1$StartDateTime)

#check for missing value
sum(is.na(dataset1))
#[1] 7435 which is the number of points (origines or destinations) outside zones
#Omit Null value
dataset1=na.omit(dataset1)
#selecting the weekdays
#datesetweekdays=filter(dataset1,DayOfWeek!=7)

#plot the frequency of travels per day(different groups in one shot!!!!)

dataset1 %>% 
  ggplot(aes(StartDateTime)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("nycflights13")


library(tidyverse)
library(lubridate)
library(nycflights13)

dataset1$year<- year(dataset1$StartDateTime)

dataset1$month <- month(dataset1$StartDateTime)

dataset1$yearday <- yday(dataset1$StartDateTime)

dataset1$monthday <- mday(dataset1$StartDateTime)

dataset1 %>% 
  mutate(yearday= yday(StartDateTime)) %>% 
  ggplot(aes(x = yearday)) +
 # geom_bar() +
  geom_freqpoly(binwidth = 1)

# plot daily
dataset1 %>% 
  mutate(wday = wday(StartDateTime, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

nf <- count(dataset1, year, month, monthday)
nf <- mutate(nf, date = make_date(year, month, monthday))

#plot the frequency of travels per day(different groups in one shot!!!!)
p <- ggplot(nf, aes(date, n)) + geom_line()
p

#weekly plot
p + facet_wrap(~ wday(date, TRUE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#A simple calendar plot:

monthweek <- function(d, w) ceiling((d - w) / 7) + 1

nf <- mutate(nf, wd = wday(date, label = TRUE))
nf <- mutate(nf, wd = factor(wd, levels = rev(levels(wd))))
nf <- mutate(nf, mw = monthweek(monthday, wday(date)))

ggplot(nf, aes(x = as.character(mw), y = wd, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white") +
  facet_wrap(~ month(date, TRUE)) +
  ylab("") + xlab("Week of Month") +
  theme(panel.grid.major = element_blank())

#second plot
nf2 <- mutate(nf, wd = wday(date, label = TRUE))
nf2 <- mutate(nf2, wd = factor(wd))
nf2 <- mutate(nf2, mw = factor(monthweek(monthday, wday(date))))
nf2 <- mutate(nf2, mw = factor(mw, rev(levels(mw))))

ggplot(nf2, aes(x = wd, y = mw, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white") +
  facet_wrap(~ month(date, TRUE)) +
  ylab("") + xlab("Week of Month") +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# write the augmented Zones dataset to CSV
#write.csv(dataset1, "Full_Dockless_With_Zones.csv", row.names=FALSE)

