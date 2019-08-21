"
****************************************************************
Name:Himani Dave
Student Number:A00178443

ANA1002 Module 10 R Assignment
****************************************************************
"
#############						   	                  
#Question #1							
#############

airquality <- read.csv("airquality10.csv", header = TRUE)
library(naniar)

# a) 

install.packages("mice")
library(mice)
anyNA(airquality)
table(is.na(airquality)) 
#There are 44083 missing entries in all. 

prop.table(table(is.na(airquality))) 
#1.3% of the values are missing in the given data.

md.pattern(airquality)
# i) 65 entires are missing in all.
# ii) Ozone has the maximum number of missing values: 37.
# iii) Ozone and wind are the variables that are missing at the same time. There are 3 such instances. 

# b) 

mean.temp <- mean(airquality$Temp, na.rm = TRUE)
mean.ozone <- mean(airquality$Ozone, na.rm = TRUE)
mean.solar.r <- mean(airquality$Solar.R, na.rm = TRUE)
mean.wind <- mean(airquality$Wind, na.rm = TRUE)
mean.month <- mean(airquality$Month, na.rm = TRUE)
mean.day <- mean(airquality$Day, na.rm = TRUE)

# c) 

air.median <- airquality
I<-is.na(air.median$Solar.R)
median.solar <- median(air.median$Solar.R, na.rm = TRUE)
air.median$Solar.R[I] <- median.solar
air.median

# d)

air.mean <- air.median

air.mean.5 <- subset(air.mean, air.mean$Month == 5)
I5 <- is.na(air.mean.5$Temp)
mean.temp.5 <- mean(air.mean.5$Temp, na.rm = TRUE)
air.mean.5$Temp[I5] <- mean.temp.5


air.mean.6 <- subset(air.mean, air.mean$Month == 6)
I6 <- is.na(air.mean.6$Temp)
mean.temp.6 <- mean(air.mean.6$Temp, na.rm = TRUE)
air.mean.6$Temp[I6] <- mean.temp.6


air.mean.7 <- subset(air.mean, air.mean$Month == 7)
I7 <- is.na(air.mean.7$Temp)
mean.temp.7 <- mean(air.mean.7$Temp, na.rm = TRUE)
air.mean.7$Temp[I7] <- mean.temp.7


air.mean.8 <- subset(air.mean, air.mean$Month == 8)
I8 <- is.na(air.mean.8$Temp)
mean.temp.8 <- mean(air.mean.8$Temp, na.rm = TRUE)
air.mean.8$Temp[I8] <- mean.temp.8


air.mean.9 <- subset(air.mean, air.mean$Month == 9)
I9 <- is.na(air.mean.9$Temp)
mean.temp.9 <- mean(air.mean.9$Temp, na.rm = TRUE)
air.mean.9$Temp[I9] <- mean.temp.9

air.mean <- rbind(air.mean.5, air.mean.6, air.mean.7, air.mean.8, air.mean.9)

# e) 

air.ratio <- air.mean
cor(air.ratio, use="complete.obs")
Ir <- is.na(air.ratio$Ozone)
Ratio <- sum(air.ratio$Ozone[!Ir])/sum(air.ratio$Temp[!Ir])
air.ratio$Ozone[Ir] <- Ratio * air.ratio$Temp[Ir]
mean(air.ratio$Ozone)

# f) 

air.complete <- air.ratio
cor(air.complete, use="complete.obs")  

Wind<-air.complete$Wind
Ozone<-air.complete$Ozone
Wind.lm <-lm(Wind~Ozone)
summary(Wind.lm)

Index <- is.na(air.complete$Wind)
ozone.data<- data.frame(Ozone = air.complete$Ozone[Index])
predict<-predict(Wind.lm, ozone.data)

air.complete$Wind[Index]<-predict
anyNA(air.complete)

# g) 

mean(air.complete$Solar.R)
mean(air.complete$Ozone)
mean(air.complete$Temp)
mean(air.complete$Wind)
mean(air.complete$Month)
mean(air.complete$Day)

# h) 
summary(airquality)
str(airquality)

imp.data <- mice(airquality, method='pmm', m=5, maxit=0, seed=2)

summary(imp.data)

final.air.1 <- complete(imp.data, 1)
final.air.2 <- complete(imp.data, 2)
final.air.3 <- complete(imp.data, 3)
final.air.4 <- complete(imp.data, 4)
final.air.5 <- complete(imp.data, 5)

airquality.final <- complete(imp.data, "stacked")

apply(airquality.final, 2,mean)

# i) 

## The mean from air.complete are given below
#  Ozone    Solar.R       Wind       Temp      Month        Day 
# 42.111442 186.803922   9.985304  77.913523   6.993464  15.803922 

## The mean from airquality.final are given below
#  Ozone    Solar.R       Wind       Temp      Month        Day 
# 40.928105 186.550327  10.025752  77.895425   6.993464  15.803922 

#The data from the airquality.final is better than that from air.complete since it
#is more precise and a single method is used to impute the entire dataset rather than multiple
#methods which might produce inaccuracies and inconsistencies. 
