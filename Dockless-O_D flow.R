#O-D flow presentations
rm(list=objects())
#install.packages("tidyr")
#install.packages("sf")
#install.packages("dplyr")
#install.packages("spDataLarge")
#install.packages("stplanr")      # geographic transport data package
#install.packages("tmap")         # visualization package (see Chapter 8)

library(tidyr)
library(sf)
library(dplyr)
library(spDataLarge)
library(stplanr)      # geographic transport data package
library(tmap)         # visualization package (see Chapter 8)
require(sp)
require(rgdal)
require(maps)

#read sf
Zones<- st_read ("ShapefileDir","Dockless_Vehicle_Distribution_Zones" )

# read in Zone polygons
#Zones <- readOGR(".", "Dockless_Vehicle_Distribution_Zones")
class(Zones)

# tell R that End and startpoints coordinates are in the same lat/lon reference system
# as the parks data -- BUT ONLY BECAUSE WE KNOW THIS IS THE CASE!
#Zones <- spTransform(Zones, CRS("+proj=longlat +datum=WGS84"))
#proj4string(Zones)
names(Zones)


# read in o-d data, and turn it into a SpatialPointsDataFrame
Primary_OD <- read.csv("Full_Dockless_With_Zones.csv", stringsAsFactors = FALSE, header = TRUE)
# build counter trip
Primary_OD$TripNo<-1
str(Primary_OD)
#fill the outer band zones by 10 
Primary_OD[,14:15][is.na(Primary_OD[,14:15])]<-10

#Filter based on the defined criteria
#Setfilter for daytime hours

#Primary_OD <- Primary_OD[Primary_OD$StartTime >= "00:00:00" & Primary_OD$StartTime <= "12:00:00",]

#SetFilter for weekdays

#Primary_OD <- Primary_OD[Primary_OD$DayOfWeek ==1 | Primary_OD$DayOfWeek==6| Primary_OD$DayOfWeek==7,]

#set filter for the day of year the most crowded day (13th of July 2019)

Primary_OD <- Primary_OD[Primary_OD$yearday ==194,]

#SetFilter for Months

#Primary_OD <- Primary_OD[Primary_OD$month >=5 & Primary_OD$month<=7,]

#setFilter for certain average daily results
Primary_OD_D= Primary_OD %>%
  group_by(StartDate,StartZones, EndZones)%>%
  summarize(TripNo=sum(TripNo),TripDuration=sum(TripDuration), TripDistance=sum(TripDistance))


#Origine_Destination summary Zones
zones_attr = Primary_OD_D %>% 
  group_by(StartZones, EndZones) %>% 
  summarize(TripDuration_O=sum(TripDuration), TripDistance_O=sum(TripDistance),TripNo_O=mean(TripNo)) %>% 
  dplyr::rename(Dist_Zone = StartZones)


summary(zones_attr$Dist_Zone %in% Zones$Dist_Zone)
#>Mode   FALSE    TRUE 
#>logical       1       9 

# 10 stand az out of Zones
#zones_attr[10,1] <- 10

zones_joined = left_join(Zones, zones_attr, by = "Dist_Zone")
str(zones_joined)
sum(zones_joined$TripNo)
#> [1] 238805
names(zones_joined)
#> [1] "geo_code"   "name"       "all"        "bicycle"    "foot"      
#> [6] "car_driver" "train"      "geometry"

#Destination summary zones
zones_od = Primary_OD_D %>% 
  group_by(EndZones) %>% 
  #summarize_if(is.numeric, sum) %>% 
  summarize(TripDuration_D = sum(TripDuration), TripDistance_D = sum(TripDistance),TripNo_D = mean(TripNo)) %>% 
  dplyr::select(Dist_Zone = EndZones, TripNo_D) %>% 
  inner_join(zones_joined, ., by = "Dist_Zone")
zones_od
zones_attr

#Abstract O_D 
Abs_zones_od = zones_od %>%
  group_by(Dist_Zone) %>%
  summarize(TripDuration_O=sum(TripDuration_O),TripNo_D= mean(TripNo_D), TripDistance_O=sum(TripDistance_O),TripNo_O=mean(TripNo_O)) %>% 
  dplyr::rename(Zone_No=Dist_Zone)

#Faceted Maps for average daily trip attractions and generations in different zones 
qtm(Abs_zones_od, fill=c("TripNo_O", "TripNo_D"), fill.n = 8) +
  tm_layout(panel.labels = c("Origin", "Destination"))

#Desire Lines
od_top5 = zones_od %>% 
  arrange(desc(TripNo_O)) %>% 
  top_n(5, wt = TripNo_O)
#od_top5
zones_od$Pl2040Area<-NULL

od_intra = filter(Primary_OD_D, StartZones == EndZones)
od_inter = filter(zones_od, Dist_Zone != EndZones & Dist_Zone!=10 & EndZones!=10 )
#<-zones_od[Dist_Zone & EndZone, ]
zones
#odlines <- od_coords2line(zones_od)
desire_lines = od2line(od_inter, zones_od)
#desire_line2<- desire_lines

#> Creating centroids representing desire line start and end points.
#qtm(desire_lines, lines.col ="TripNo_O", lines.lwd = "TripNo_O")+ 
#  tm_layout(legend.width=20)+
#  tm_plygones(Abs_zones_od)
class(desire_lines)
Abs_zones_od
#plot(Abs_zones_od)
#plot(desire_lines,breaks = c(0, 50, 70, 80, 90, 95, 100))

#desire_lines and the backgroundzones
  
tm_shape(Abs_zones_od) + tm_borders() +
  tm_shape(desire_lines) +
  tm_lines(
    palette = "plasma",breaks = c(0, 5,10,15,20,25,30,35,40,45,50, 100),
    lwd = "TripNo_O",
    scale = 5,
    title.lwd = "TripNo_O",
    alpha = 0.5,
    col = "TripNo_O",
    title = "Active travel",
    legend.lwd.show = TRUE
  ) +
  tm_scale_bar() +
  tm_layout(
    legend.bg.alpha = 0.5,
    legend.bg.color = "white"
  )
#sum(desire_line2$TripNo_O)

#sum(desire_lines$TripNo_O)

#other plots
library(tidyverse)
library(lubridate)
library(nycflights13)
library(ggplot2)
library(dplyr)

dataset1<-Primary_OD
#plot filtered year-day
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
library(ggplot2)
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


