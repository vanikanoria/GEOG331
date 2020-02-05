heights <- c(30, 41, 20, 22)
heights
heights[1]
Mat <- matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)

Mat.bycol <- matrix(c(1,2,3,4,5,6), ncol=2, byrow = FALSE)
Mat.bycol

##Question 1 ----
#read in weather station file from the data folder
datW <- read.csv("/Users/vani/Documents/MyCode/GEOG331/activity2/a02/2011124.csv")

#get more information about the dataframe
str(datW)

##Question 2 ----
#Creating example vectors of each data type
character(5)
numeric(5)
integer(5)
factor(5)
NumericVector <- c(13.5, 20, 10.2, 1.05, 7)
CharacterVector <- c('13.5', '20', '10.2', '1.05', '7')
type(CharacterVector)
integerVector <- c(13L, 20L, 10L, 1L, 7L)
factor1 <- factor(NumericVector)

#specify a column with a proper date format
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

#create a date column by reformatting the date to only include years
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#find out all unique site names
levels(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate the average daily temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
colnames(averageTemp) <- c("NAME", "MAAT")
averageTemp

#convert level to number for factor data type
datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

##Question 3 ----
help(hist)
help(paste)


##Question 4 ----

#adding all the histograms into the same window
par(mfrow=c(2,2))

#making a vector for colors of different histograms
color_vector <- c("grey50", "blue", "orange", "green")

#for loop to create four histograms including the one for Aberdeen:
for (val in 1:4)
  
{hist(datW$TAVE[datW$siteN == val],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[val]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col=color_vector[val],  #color chosen from vector according to the index val
     border="white")

#calculating mean for histogram so I don't have to do it multiple times
hist_mean = mean(datW$TAVE[datW$siteN == val],na.rm=TRUE)
#calculating standard deviation for histogram
hist_sd = sd(datW$TAVE[datW$siteN == val],na.rm=TRUE)

#add mean line with red (tomato3) color
#and thickness of 3
abline(v = hist_mean, 
       col = "tomato3",
       lwd = 3)

#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = hist_mean - hist_sd, 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = hist_mean + hist_sd, 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
} ##end of for-loop

h1 <- hist(datW$TAVE[datW$siteN == 2],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[2]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")

x.plot <- seq(0,30, length.out = 100)
y.plot <-  dnorm(seq(0,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE))
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

##Question 6 ----

#Assigning the value of current threshold of unusually high temperatures 
#to the variable 'orig_extreme_high_threshold'
orig_extreme_high_threshold = qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#orig_extreme_high_threshold = 18.51026
#calculating the original mean for Aberdeen
orig_mean_Aberdeen = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)

#calculating the increased mean (old + 4) in degrees C
new_mean_Aberdeen = orig_mean_Aberdeen + 4

#calculating the sd for Aberdeen
sd_Aberdeen = sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)

#Probability of temperatures being 
#above the current threshold for extreme high temperatures(18.51026):
1 - pnorm(orig_extreme_high_threshold,
          new_mean_Aberdeen, sd_Aberdeen)
# = 0.2031656 = approximately 20.32% of the time


##Question 7 ----
#Histogram of daily precipitation for Aberdeen:
prcp_hist <- hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily Precipitation (mm)", 
     ylab="Relative frequency",
     xlim = c(0, 100),
     col="purple",
     border="white")
prcp_hist

##Question 8 ---
#Using the sum function to aggregate precipitation by year and site
yearlyPrec <- aggregate(datW$PRCP, by = list(datW$NAME, datW$year), FUN = "sum", na.rm = TRUE)
colnames(yearlyPrec) <- c("NAME", "YEAR", "TOTAL_PREC")
yearlyPrec$siteN <- as.numeric(yearlyPrec$NAME)

#Making a histogram for annual precipitation of Aberdeen:
yearly_prcp_hist_Aberdeen <- hist(yearlyPrec$TOTAL_PREC[yearlyPrec$siteN == 1],
                  freq=FALSE,
                  main = paste(levels(yearlyPrec$NAME)[1]),
                  xlab = "Yearly Precipitation (mm)", 
                  ylab="Relative frequency",
                  col="yellow",
                  border="white")
yearly_prcp_hist_Aberdeen

##Question 9 ---
# mean of annual precipitation for all sites:
mean_annual_prec <- aggregate(yearlyPrec$TOTAL_PREC, by=list(yearlyPrec$NAME), FUN ="mean", na.rm = TRUE)
#giving column names to the dataframe
colnames(mean_annual_prec) <- c("NAME","Mean Annual Prec")
mean_annual_prec




