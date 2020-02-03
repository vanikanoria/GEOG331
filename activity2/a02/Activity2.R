heights <- c(30, 41, 20, 22)
heights
heights[1]
Mat <- matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)

Mat.bycol <- matrix(c(1,2,3,4,5,6), ncol=2, byrow = FALSE)
Mat.bycol

datW <- read.csv("/Users/vani/Documents/MyCode/GEOG331/activity2/a02/2011124.csv")
str(datW)
character(5)
numeric(5)
integer(5)
factor(5)
NumericVector <- c(13.5, 20, 10.2, 1.05, 7)
CharacterVector <- c('13.5', '20', '10.2', '1.05', '7')
type(CharacterVector)
integerVector <- c(13L, 20L, 10L, 1L, 7L)
factor1 <- factor(NumericVector)

datW <- read.csv("/Users/vani/Documents/MyCode/GEOG331/activity2/a02/2011124.csv")
levels(datW$name)

