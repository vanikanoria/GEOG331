#install.packages(c("raster","sp","rgdal","rgeos","plyr"))
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)
g1966 <- readOGR("Y:\\Data\\activities\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
plot(g1966)
plot(g1966, color = "black", axes = TRUE)
str(g1966)
#subpolygons within polygons

g1966@polygons[[1]]@Polygons[[1]]
#this is vector data
#google the projections to understand it

