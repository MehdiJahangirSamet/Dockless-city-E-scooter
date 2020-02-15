# This program is part of data analytics which aims to provide animation map
# visualizations for all recorded routs

#O-D flow presentations
rm(list=objects())

#install.packages("magrittr")
#install.packages("tidyr")
#install.packages("sf")
#install.packages("dplyr")
#install.packages("spDataLarge")
#install.packages("stplanr")      # geographic transport data package
#install.packages("tmap")         # visualization package (see Chapter 8)
#install.packages("gganimate")
library(magrittr)
library(tidyr)
library(gganimate)
library(sf)
library(dplyr)
library(spDataLarge)
library(stplanr)      # geographic transport data package
library(tmap)         # visualization package (see Chapter 8)
require(sp)
require(rgdal)
require(maps)
library(ggplot2)

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

Primary_OD <- Primary_OD[Primary_OD$StartTime >= "05:00:00" & Primary_OD$StartTime <= "10:00:00",]

#SetFilter for weekdays
#Primary_OD <- Primary_OD[Primary_OD$DayOfWeek ==1 | Primary_OD$DayOfWeek==7,]

#SetFilter for Months
#Primary_OD <- Primary_OD[Primary_OD$month >=4 & Primary_OD$month<=7,]


# inset start and end day of period presentation in the yearday Stday, Enday
Stday <- 150
Enday <- 165


##setFilter for plot origin 
Primary_O_D= Primary_OD %>%
  group_by(StartDate,StartZones)%>%
  summarize(monthday=mean(monthday),month= mean(month),yearday=mean(yearday),year=mean(year), TripGenerationNo=sum(TripNo),TripDuration=sum(TripDuration), TripDistance=sum(TripDistance))%>%
  dplyr::rename(Dist_Zone = StartZones)
O_daily = left_join(Zones, Primary_O_D, by = "Dist_Zone")
str(O_daily)
sum(O_daily$TripGenerationNo)

#> [1] 238805
names(O_daily)
#OD_daily$monthday<=as.integer(OD_daily$monthday)
SO_daily = O_daily %>%
  filter (yearday %in% c(Stday:Enday))

tm_shape(Zones) +
  tm_polygons ()+
  tm_shape(SO_daily)+
  tm_symbols(col = "black", border.col = "white",size = "TripGenerationNo")+
  tm_facets(by="yearday", nrow = 2, free.coords = FALSE)



##setFilter for plot Destination 
Primary_D_D= Primary_OD %>%
  group_by(StartDate,EndZones)%>%
  summarize(monthday=mean(monthday),month=mean(month),yearday=mean(yearday),year=mean(year), TripAttractionNo=sum(TripNo),TripDuration=sum(TripDuration), TripDistance=sum(TripDistance))%>%
  dplyr::rename(Dist_Zone = EndZones)
D_daily = left_join(Zones, Primary_D_D, by = "Dist_Zone")
#str(D_daily)
sum(D_daily$TripAttractionNo)

names(D_daily)



#OD_daily$monthday<=as.integer(OD_daily$monthday)
SD_daily = D_daily %>%
  filter (yearday %in% c(Stday:Enday))

SD_daily$TripGenerationNo <- 0
SO_daily$TripAttractionNo <- 0
#OD_daily <- bind_rows(SD_daily, SO_daily)

Counter1<-10
Counter2<- 10 -Stday +Enday+1

#spread the StartDay into columns
Wide_SD_daily<- SD_daily %>% spread(yearday,TripAttractionNo)
colnames(Wide_SD_daily)[Counter1:Counter2]<- paste("Trip Attraction in ", colnames(Wide_SD_daily[Counter1:Counter2]))
colnames(Wide_SD_daily) [Counter2]<- "geometry"

Wide_SO_daily <- SO_daily %>% spread(yearday,TripGenerationNo)
colnames(Wide_SO_daily)[Counter1:Counter2]<- paste("Trip Generation in ", colnames(Wide_SO_daily[Counter1:Counter2]))
colnames(Wide_SO_daily) [Counter2]<- "geometry"

#colnames(Wide_SO_daily)<- paste("Trip Generation in ", colnames(Wide_SO_daily))

#bind_rows
Wide_OD_daily <- bind_rows(Wide_SD_daily%>% as.data.frame(),Wide_SO_daily%>% as.data.frame())

#Wide_OD_daily <- full_join(Wide_SD_daily%>% as.data.frame(),Wide_SO_daily%>% as.data.frame(), by=NULL)

Wide_OD_daily <- left_join(Zones,Wide_OD_daily, "Dist_Zone" )

Wide_OD_daily %<>% st_sf(sf_column_name = 'geometry.x')
st_crs(Wide_OD_daily)

names(Wide_OD_daily)


# define paire serries of following attraction and generation trips for each time frams
vars <- c("Trip Generation in  ", "Trip Attraction in  ")
Series<- apply(expand.grid(vars, seq(Stday, Enday)), 1, paste, collapse="")
Series

vars2 <- c("Trip Generation in day ", "Trip Attraction in day ")
Series2<- apply(expand.grid(vars2, seq(Stday, Enday)), 1, paste, collapse="")
Series2

# define paire serries for symbol colours
Counter3 <- Counter2- Counter1
Colnn <- rep(c("red", "blue"),Counter3)


#class(Wide_SD_daily)
#test plot and Animation for pair view of attraction and generation trips for each time frams


m2 <- tm_shape(Zones) +
  tm_borders() +
  tm_shape(Wide_OD_daily) + 
  tm_bubbles(size = Series,
             col = Colnn,
             border.col = "black", border.alpha = .5,
             scale = 2, title.size = Series2)+
  tm_facets(free.scales.symbol.size = FALSE, nrow=1,ncol=2) + 
  tm_format("World", scale=.5)+
  tm_legend(position = c("right", "bottom"), stack = "horizontal")

m2
tmap_animation(m2, filename="OD_Animation.gif", width=1200, delay=200)





# Daily Trip Generation vs. Attraction 
SO_Plot = tm_shape(Zones)+
  tm_borders()+
  tm_shape(SO_daily)+
  tm_borders()+
  tm_facets(by="StartDate", ncol = 1)+
  tm_fill ("TripGenerationNo", breaks = c(0,20,50,100, 200, 400, 600, 800, 1000, 1200))


SD_Plot = tm_shape(Zones)+
  tm_borders()+
  tm_shape(SD_daily)+
  tm_borders()+
  tm_facets(by="StartDate", ncol = 1)+
  tm_fill ("TripAttractionNo", breaks = c(0,20,50,100, 200, 400, 600, 800, 1000, 1200))

tmap_arrange(SO_Plot, SD_Plot)
    

 
#animation

SD_Anim = tm_shape(Zones)+
  tm_borders()+
  tm_shape(SD_daily)+
  tm_borders()+
  tm_facets(along = "StartDate",free.coords = FALSE, ncol=2, nrow = 1)+
  tm_fill ("TripAttractionNo", breaks = c(0,20,50,100, 200, 400, 600, 800, 1000, 1200))
#SO_Anim = tm_shape(Zones)+
 # tm_borders()+
  #tm_shape(SO_daily) + 
  #tm_borders()+
  #tm_facets(along = "StartDate", free.coords = FALSE)+
  #tm_fill ("TripGenerationNo", breaks = c(0,20,50,100, 200, 400, 600, 800, 1000, 1200))

#O_Anim = tm_shape(Zones)+ tm_polygons()+
  #tm_shape(SO_daily)+ tm_dots(size = "TripGenerationNo", col = "red") +
  #tm_facets(along = "yearday", free.coords = FALSE)


#tmap_animation(SO_Anim,filename = "D_Anim.gif",width= 8, height = 8, delay = 45)
tmap_animation(SD_Anim,filename = "D_Anim.gif",width= 16, height = 8, delay = 45)



