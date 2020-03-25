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

plot(NDVIraster[[1]])

## Question 3:


###from start:
# plot(g1966)
# plot(g1966, color = "black", axes = TRUE)
# str(g1966)
# #subpolygons within polygons
# 
# g1966@polygons[[1]]@Polygons[[1]]
# #this is vector data
# #google the projections to understand it
# 
