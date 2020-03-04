#install.packages(c('lubridate'))
#loading in lubridate
library(lubridate)

datH <- read.csv("/Users/vani/Desktop/a05/stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)              

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("/Users/vani/Desktop/a05/2049867.csv")                          
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#converting date and time:
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$decDay-1)/366),
                       datD$year + ((datD$decDay-1)/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + ((datP$decDay-1)/366),
                       datP$year + ((datP$decDay-1)/365))   
##Question 4
#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN = "mean")
colnames(aveF) <- c("doy", "dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)


#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)


#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,200),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,180, by=20),
     seq(0,180, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "observations in 2017"), #legend items
       lwd=c(2,NA,2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), rgb(0.9, 0.05, 0.2,.2)),#colors
       pch=c(NA,15, NA),#symbols
       bty="n")#no legend border

##Question 5---
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Months of the Year", 
     ylab=expression(paste("Average Daily Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,200),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)  
#x-axis with months as labels
axis(1, c(1,32,61,92,122,153,183,214,245,275,306,336,366), #tick intervals
     lab=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","" )) #tick labels
axis(2, seq(0,180, by=20),
     seq(0,180, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "observations in 2017"), #legend items
       lwd=c(2,NA,2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), rgb(0.9, 0.05, 0.2,.2)),#colors
       pch=c(NA,15, NA),#symbols
       bty="n")#no legend border

#adding a line thatshows observations for 2017
lines(datD$doy[datD$year==2017],datD$discharge[datD$year==2017], 
     type="l", 
     lwd=2,
     col = rgb(0.9, 0.05, 0.2,.2)) #red

##Question 7-----

#creating a dataframe numObsP that aggregates the datP$hour by doy and year
#It counts the hours, and hence number of observations, for each day in the data
numObsP <- aggregate(datP$hour, by=list(datP$doy,datP$year), FUN = "length")
colnames(numObsP) <- c("doy","Year","NumOfObs")
numObsP$discharge <- aggregate(datP$HPCP, by=list(datP$doy,datP$year), FUN = "sum")

##creating another dataframe fullP, subsetting the days that had 24 observations
fullP <- numObsP[numObsP$NumOfObs==24, ]

#adding the column decimal_year
fullP$decYear <- ifelse(leap_year(fullP$Year),fullP$Year + ((fullP$doy)/366),
                        fullP$Year + ((fullP$doy-1)/365))
fullP$point  <-200
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
 
points(fullP$decYear, fullP$point, lwd=0.2, pch = 3, col= rgb(0.99, 0.05, 0.1,.7))#red 
      
legend("topleft", c("discharge","days with 24 observations"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.99, 0.05, 0.1,.7)),#colors
       pch=c(NA,3),#symbols
       bty="n")#no legend border

##Question 8:
  
  #provided code
#subset discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#My hydrograph:

hydroD1 <- datD[datD$doy >= 296 & datD$doy < 298 & datD$year == 2007,]
hydroP1 <- datP[datP$doy >= 296 & datP$doy < 298 & datP$year == 2007,]


#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl1 <- floor(min(hydroD1$discharge))-1
#celing rounds up to the integer
yh1 <- ceiling(max(hydroD1$discharge))+1
#minimum and maximum range of precipitation to plot
pl1 <- 0
pm1 <-  ceiling(max(hydroP1$HPCP))+.5
#scale precipitation to fit on the 
hydroP1$pscale <- (((yh1-yl1)/(pm1-pl1)) * hydroP1$HPCP) + yl1

    #making the plot
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD1$decDay,
     hydroD1$discharge, 
     type="l", 
     ylim=c(yl1,yh1), 
     lwd=2,
     xlab="Day of 2007", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP1)){
  polygon(c(hydroP1$decDay[i]-0.017,hydroP1$decDay[i]-0.017,
            hydroP1$decDay[i]+0.017,hydroP1$decDay[i]+0.017),
          c(yl1,hydroP1$pscale[i],hydroP1$pscale[i],yl1),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#using ggplot to make boxplots and violin plots
#install.packages("ggplot2")
library(ggplot2)

#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()
##Question 9

#c(1,32,61,92,122,153,183,214,245,275,306,336,366)

#Spring: March 1 to May 31, or doy 61 to 152
#Summer:1 June to 31 August, or doy 153 to 244
#Autumn:1 September to 30 November, or doy 245 to 335 
#Winter:1 December to 28 February, or doy 336 to 366 and doy 1 to 60
#assigning seasons to dates according to calendar definition
datD$seasons <- ifelse(datD$doy>60 & datD$doy<=152, "Spring", 
              ifelse(datD$doy>=153 & datD$doy<=244,"Summer",
                     ifelse(datD$doy>=245 & datD$doy<=335,"Autumn","Winter")))

#making them factor data
datD$seasons<- as.factor(datD$seasons)

#making violin plots:
#2016
ggplot(data= datD[datD$year==2016,], aes(seasons,discharge)) + 
  geom_violin(fill= "yellow", trim=FALSE, show.legend=FALSE) +

xlab("Seasons of 2016") +
ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1"))) + ggtitle ("Discharge by season:2016")

#2017

ggplot(data= datD[datD$year==2017,], aes(seasons,discharge)) + 
  geom_violin(fill= "yellow", trim=FALSE, show.legend=FALSE) +
  
  xlab("Seasons of 2016") +
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1"))) + ggtitle ("Discharge by season:2017")




