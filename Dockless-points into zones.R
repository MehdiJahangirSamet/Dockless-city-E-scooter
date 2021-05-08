#This sub Program aims to read the route data of e-scooters from a CSV file 
#of dockless case study. As a result 300550 routes with origin and desrination
#coordination was clustered in 9 traffic zones.

#install.packages("sp")
#install.packages("rgdal")
#install.packages("maps")
rm(list=objects())
require(sp)
require(rgdal)
require(maps)

# read in startpoint data, and turn it into a SpatialPointsDataFrame
Startpoints <- read.csv("Copy of DocklessTripOpenData_6.csv")
str(Startpoints)
coordinates(Startpoints) <- c("StartLongitude", "StartLatitude")
Endpoints <- read.csv("Copy of DocklessTripOpenData_6.csv")
str(Endpoints)
coordinates(Endpoints) <- c("EndLongitude", "EndLatitude")

# read in Zone polygons
Zones <- readOGR(".", "Dockless_Vehicle_Distribution_Zones")
class(Zones)


# tell R that End and startpoints coordinates are in the same lat/lon reference system
Zones <- spTransform(Zones, CRS("+proj=longlat +datum=WGS84"))
proj4string(Zones)

proj4string(Startpoints) <- proj4string(Zones)

#the same for Endpoints
proj4string(Endpoints) <- proj4string(Zones)


# combine is.na() with over() to do the containment test for Startpoints only; note that we
# need to "demote" Zones to a SpatialPolygons object first
inside.StartZone <- !is.na(over(Startpoints, as(Zones, "SpatialPolygons")))
inside.StartZone

#the same for EndZone
# what fraction of Startpoints were inside a zone?
mean(inside.StartZone)
## [1] 0.990208

# use 'over' again, this time with zones as a SpatialPolygonsDataFrame
# object, to determine which zone (if any) contains each sighting, and
# store the zone name as an attribute of the bears data
Startpoints$StartZones <- over(Startpoints, Zones)$Dist_Zone
# the same for EndZones
Endpoints$EndZones <- over(Endpoints, Zones)$Dist_Zone

# add Endzones in the Startpoints table
Startpoints$EndZones <- Endpoints$EndZones

# draw a map big enough to encompass all points (but don't actually plot
# the points yet), then add in zone boundaries superimposed upon a map
# of the United States
plot(coordinates(Startpoints), type="n")
map("world", region="usa", add=TRUE)
plot(Zones, border="green", add=TRUE)
legend("topright", cex=0.85,
       c("Startpoints in Zones", "Startpoints not in Zone", "Zones boundary"),
       pch=c(16, 1, NA), lty=c(NA, NA, 1),
       col=c("red", "grey", "green"), bty="n")
title(expression(paste(italic("O-D"),
                       "E-scooter Dockless project")))

# now plot bear points with separate colors inside and outside of zones
points(Startpoints[!inside.StartZone, ], pch=1, col="gray")
points(Startpoints[inside.StartZone, ], pch=16, col="red")

# write the augmented Zones dataset to CSV
write.csv(Startpoints, "Dockless_With_Zones.csv", row.names=FALSE)

# ...or create a shapefile from the points
writeOGR(Startpoints, ".", "route-by-zones2", driver="ESRI Shapefile")


