
#creating a function for error messages:
assert <- function(statement, err.message){
  if (statement ==  FALSE){
    print(err.message)
  }
}

assert(1==2, "error: unequal values")
assert(2==2, "error: unequal values")

##Question 2 --

#reading in the data file
#skip the first 3 rows since there is additional column info and specify that
#NA is designated differently

datW <- read.csv("/Users/vani/Desktop/GEOGActivity3/bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)

#preview data
print(datW[1,])

#get sensor info from file
sensorInfo <- read.csv("/Users/vani/Desktop/GEOGActivity3/bewkes_weather.csv", na.strings=c("#N/A"), nrows=2)
print(sensorInfo)

#get column names from sensorInfo table
#and set weather station colnames to be the same

colnames(datW) <- colnames(sensorInfo)
#preview data
print(datW[1,])

#installing lubridate
#install.packages(c("lubridate"))
#commented it after installing it

#to load the package into my R working environment:
library(lubridate)

#converting date to standardized format
#the date is m/d/y
dates <- mdy_hm(datW$timestamp, tz="America/New_York")

#calculate day of year
datW$doy <- yday(dates)

#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)

#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)

#quick preview of new date calculations
datW[1,]

##checking missing data:

#air temperature
length(which(is.na(datW$air.temperature)))

#wind speed
length(which(is.na(datW$wind.speed)))

#precipitation
length(which(is.na(datW$precipitation)))

#soil temp
length(which(is.na(datW$soil.temp)))

#soil moisture
length(which(is.na(datW$soil.moisture)))

#make a plot with filled in points (using pch)
#line lines
#ploting soil moisture:
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#plotting air temp:

plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

datW[2,]

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

##Question 4:

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,] 

#the station 'Rome, NY, which is close to Hamilton did have similar values
#on 06/22/2018 and 06/26/2018


##Question 5:

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",type="n")

#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0], col= rgb(95/255,158/255,160/255,.5), pch=15)       

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0], col= "tomato3", pch=19)

#test to ensure that I can use the varibale lightscale to subset values in datW:
#that the length of subset >0:
assert(length(datW$DD[lightscale > 0])!= 0, "Error: null value created")
#that the length of subset < nrows(datW$DD)
assert(length(datW$DD[lightscale > 0])< length(datW$DD), "Error: subset = original set")

##Question 6:
  
#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation>=2 & datW$lightning.acvitivy>0, NA,ifelse(datW$precipitation>5, NA, datW$air.tempQ1)) 
print(datW$air.tempQ2[datW$precipitation>5])
#ifelse(test, yes, no)

#assertion to check if the data has been filtered correctly:
assert(length(which(is.na(datW$air.tempQ2[(datW$precipitation  >= 2 & datW$lightning.acvitivy >0)|datW$precipitation>5])))!=0, "Error: results not as expected")

#Removing suspect measurements from wind speed measurements

datW$wind.speedQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA, ifelse(datW$precipitation > 5, NA, datW$wind.speed))

#assertion to check if the data has been filtered correctly:
assert(length(which(is.na(datW$wind.speedQ2[(datW$precipitation  >= 2 & datW$lightning.acvitivy >0)|datW$precipitation>5])))!=0, "Error: results not as expected")


#the new plot with lines and points of windspeed with the new data
plot(datW$DD , datW$wind.speedQ2, type="b",  xlab = "Day of Year", ylab = "Wind speed (mph)")


##Question 7

#looking at soil moisture and precipitation on dates before the soil sensor outage
datW$soil.moisture[datW$DD > 190 & datW$DD < 193]
datW$precipitation[datW$DD > 190 & datW$DD < 193]

#checking that air temperature data is reliable, that none of the values have been stored as NA for those dates in the datW$temp.airQ2 column
assert(length(which(is.na(datW$air.tempQ2) & (datW$DD > 190 & datW$DD < 192))) == 0)
#looking at air temperature data for those dates
datW$air.temperature[datW$DD > 190 & datW$DD < 193]

#looking at difference between soil temperature and air temperature
datW$soil.temp[datW$DD > 190 & datW$DD < 193] - datW$air.temperature[datW$DD > 190 & datW$DD < 193]

#checking that air temperature data is reliable, that none of the values have been stored as NA for those dates in the datW$temp.airQ2 column
assert(length(which(is.na(datW$air.tempQ2) & (datW$DD > 190 & datW$DD < 192))) == 0)


##Question 8

#table
averages$air.temp <- mean(datW$air.tempQ2,na.rm=TRUE)
averages$wind.speed <- mean(datW$wind.speedQ2,na.rm=TRUE)
averages$soil.moisture <- mean(datW$soil.moisture,na.rm=TRUE)
averages$soil.temp <- mean(datW$soil.temp,na.rm=TRUE)
#total precipitation
averages$total_prec <- sum(datW$precipitation,na.rm=TRUE)
#to abtain all the above values:
averages

#number of observations
num_obs$air.temp <- length(datW$air.tempQ2) - sum(is.na(datW$air.tempQ2))
num_obs$wind.speed <- length(datW$wind.speed) - sum(is.na(datW$wind.speed))
num_obs$soil.moisture <- length(datW$soil.moisture) - sum(is.na(datW$soil.moisture))
num_obs$soil.temp <- length(datW$soil.temp) - sum(is.na(datW$soil.temp))
num_obs$total_prec <- length(datW$precipitation) - sum(is.na(datW$precipitation))
#to abtain all the above values:
num_obs

##time perioda of measurement:

#prec and wind speed: dd of last obs - dd of first obs
datW$DD[2118] - datW$DD[1] 

#soil moisture and soil temperature: dd of last obs - dd of first obs
#since the measurements are at regular intervals, the na's are accounted for
datW$DD[1411] - datW$DD[1]

#air temperature: dd of last obs - dd of first obs
#since the measurements are at regular intervals, the na's are accounted for
datW$DD[2105] - datW$DD[1]

##Question 9:

par(mfrow=c(2,2))
#soil moisture:
plot(datW$DD , datW$soil.moisture, type="b",  xlab = "Day of Year", ylab = "Soil Moisture(meters cubed per meters cubed")

#air temperature:
plot(datW$DD , datW$air.tempQ2, type="b",  xlab = "Day of Year", ylab = "Air temperature (degrees C)")

#soil temperature
plot(datW$DD , datW$soil.temp, type="b",  xlab = "Day of Year", ylab = "Soil Temperature (degrees C)")

#precipitation
plot(datW$DD , datW$precipitation, type="b",  xlab = "Day of Year", ylab = "Precipitation (mm)")








