#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

g1966 <- readOGR("/Users/vani/Documents/MyCode/GEOG331/a06/GNPglaciers/GNPglaciers_1966.shp")
## OGR data source with driver: ESRI Shapefile 
## Source: "/Users/vani/Documents/MyCode/GEOG331/a06/GNPglaciers/GNPglaciers_1966.shp", layer: "GNPglaciers_1966"
## with 39 features
## It has 13 fields
## Integer64 fields read as strings:  OBJECTID

g1998 <- readOGR("/Users/vani/Documents/MyCode/GEOG331/a06/GNPglaciers/GNPglaciers_1998.shp")
## OGR data source with driver: ESRI Shapefile 
## Source: "/Users/vani/Documents/MyCode/GEOG331/a06/GNPglaciers/GNPglaciers_1998.shp", layer: "GNPglaciers_1998"
## with 39 features
## It has 13 fields
## Integer64 fields read as strings:  OBJECTID

g2005 <- readOGR("/Users/vani/Documents/MyCode/GEOG331/a06/GNPglaciers/GNPglaciers_2005.shp")
## OGR data source with driver: ESRI Shapefile 
## Source: "/Users/vani/Documents/MyCode/GEOG331/a06/GNPglaciers/GNPglaciers_2005.shp", layer: "GNPglaciers_2005"
## with 39 features
## It has 13 fields
## Integer64 fields read as strings:  OBJECTID

g2015 <- readOGR("/Users/vani/Documents/MyCode/GEOG331/a06/GNPglaciers/GNPglaciers_2015.shp")
## OGR data source with driver: ESRI Shapefile 
## Source: "/Users/vani/Documents/MyCode/GEOG331/a06/GNPglaciers/GNPglaciers_2015.shp", layer: "GNPglaciers_2015"
## with 39 features
## It has 21 fields
## Integer64 fields read as strings:  OBJECTID Recno

str(g2015)
#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]

g1966@proj4string
## CRS arguments:
##  +proj=utm +zone=12 +datum=NAD83 +units=m +no_defs +ellps=GRS80
## +towgs84=0,0,0

##end of Question 1

spplot(g1966, "GLACNAME")
#check glacier names
g1966@data$GLACNAME
g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent
Glacier","N. Swiftcurrent Glacier",ifelse(g2015@data$GLACNAME ==  "Miche Wabun", 
"Miche Wabun Glacier", as.character(g2015@data$GLACNAME)))

### Working with raster data
## Mapping satellite imagery

#read in rgb imagery from landsat
redL <- raster("/Users/vani/Documents/MyCode/GEOG331/a06/glacier_09_05_14/l08_red.tif")
greenL <- raster("/Users/vani/Documents/MyCode/GEOG331/a06/glacier_09_05_14/l08_green.tif")
blueL <- raster("/Users/vani/Documents/MyCode/GEOG331/a06/glacier_09_05_14/l08_blue.tif")

#check coordinate system
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

##zooming in on a specific area
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("/Users/vani/Documents/MyCode/GEOG331/a06/NDVI/NDVI_",ndviYear[i],".tif"))
  
}

str(NDVIraster[[1]])
#get projection
NDVIraster[[1]]@crs

## Question 3:
plot(NDVIraster[[1]])

plot(g1966, col="tan3", border=NA, add=TRUE)

# making a plot of the 2003 NDVI data side by side with the 1966 glacier extent
par(mfrow=c(1,2))
# 2003 DVI plot
plot(NDVIraster[[1]], axes=TRUE)
axis(side = 1, labels = TRUE)

# 1966 glacier plot
plotRGB(rgbL, stretch="lin", axes=TRUE)
plot(g1966, col="tan3", border=NA, add=TRUE)

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

## Question 4
par(mai=c(1,1,1,1))
plot(NDVIraster[[length(ndviYear) -1]], axes = FALSE, frame.plot = TRUE)
plot(g2015p, col= NA, border="black", add=TRUE)
axis(side = 1, labels = FALSE)
axis(side = 2, labels = FALSE)

## end of Question 4
#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

#joining the data together
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}   

### Question 5

# % change in area between 1966 and 2015
percent_change_in_area = abs(g2015p@data$a2015m.sq - g1966p@data$a1966m.sq)*100/g1966p@data$a1966m.sq
g2015p@data$percent_change_in_area_since_1966 <- percent_change_in_area
spplot(NDVIraster[[length(ndviYear) -1]], zcol=g2015p@datapercent_change_in_area_since_1966)
spplot(g2015@data$percent_change_in_area_since_1966,  add=TRUE, border=NA)

### Question 6

#glacier with the largest % loss
largest_loss_glacier_name <- g2015p@data$GLACNAME[g2015p@data$percent_change_in_area == 
                       max(g2015p@data$percent_change_in_area)]
largest_loss_glacier_name

boulder2015 <- g2015p[g2015p@data$GLACNAME == "Boulder Glacier"]
boulder2005 <- g2005p[g2005p@data$GLACNAME == "Boulder Glacier"]
boulder1998 <- g1998p[g1998p@data$GLACNAME == "Boulder Glacier"]
boulder1966 <- g1966p[g1966p@data$GLACNAME == "Boulder Glacier"]

par(mai=c(1,1,1,1))
plot(boulder1966, col="yellow2", border=NA )
plot(boulder2005, col="blue", border=NA, add=TRUE)
plot(boulder1998, col="palegreen2", add=TRUE, border=NA)
plot(boulder2015, col= "navyblue", add=TRUE, border=NA)

#background imagery
# ADD MAP TITLE


###from start:
# plot(g1966)

