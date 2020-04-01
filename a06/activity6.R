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

#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

# % change in area between 1966 and 2015
percent_change_in_area <- list()
for (i in 1:length(g2015p)){
  percent_change_in_area[i] = abs(g2015p@data$a2015m.sq[i] - g1966p@data$a1966m.sq[i])*100/g1966p@data$a1966m.sq[i]
}
#adding the data as a column to g2015p
g2015p@data$percent_change_in_area_since_1966 <- percent_change_in_area

#plotting by that column
spplot(g2015p, "percent_change_in_area_since_1966")


### Question 6

#glacier with the largest % loss
largest_loss_glacier_name <- g2015p@data$GLACNAME[g2015p@data$percent_change_in_area == 
                       max(g2015p@data$percent_change_in_area)]
largest_loss_glacier_name

#map plot
par(mar=c(1,1,1,1))
plot(subset(g1966,g1966p@data$GLACNAME == "Boulder Glacier"),col="yellow2",border=NA, add=T )
plot(subset(g1998,g1998@data$GLACNAME == "Boulder Glacier"),col= "palegreen2",border=NA, add=TRUE)
plot(subset(g2005,g2005@data$GLACNAME == "Boulder Glacier"),col="blue",border=NA, add=TRUE)
plot(subset(g2015,g2015@data$GLACNAME == "Boulder Glacier"),col= "navyblue",border=NA, add=TRUE)

legend(legend=c("1966", 1998, 2005, 2015), col= c("yellow2","palegreen2", "blue","navyblue"), "bottomleft", fill=TRUE)
title()

#background imagery
# ADD MAP TITLE

#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)

## End of Question 7
#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

## end of Question 8

meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
meanChange

## Question 9

#adding the data as a column to g2015p
g2015p@data$mean_change_in_NDVI <- meanChange[2:40, 2]

#plotting by that column
spplot(g2015p, "mean_change_in_NDVI")

## Question 11
#average maximum NDVI across all years within the Glacier National Park

#extent of the park
park_extent =  NDVIraster[[1]]@extent

yearly_mean_NDVI<-list()
NDVIvalues <- list()
meanvalues <- numeric(0)

#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIvalues[[i]] <- extract(NDVIraster[[i]], park_extent)[[1]]
  #calculate the mean of the NDVI values
  meanvalues[i] <- mean(NDVIvalues[[i]], na.rm=TRUE)
}

#mean of all the years
#### Need to clean the code

