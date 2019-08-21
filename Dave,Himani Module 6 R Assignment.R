"
****************************************************************
Name:Himani Dave
Student Number:A00178443

ANA1002 Module 6 R Assignment
****************************************************************
"
#############						   	                  
#Question #1							
#############

fueleffec <- read.csv("FuelEfficiencyAssignment.csv", header = TRUE)
class(fueleffec$Cylinders)

#The class of the variable 'Cylinders' is "integer".

factor_cylinders <- as.factor(fueleffec$Cylinders)
levels(factor_cylinders)

#The factor has five levels. 

#############						   	                  
#Question #2							
#############

dob <- as.Date("1994, November 23", format = "%Y, %B %d")
weekdays(dob)

#I was born on a Wednesday. 

now <- Sys.Date()
alive <- now - dob
alive_weeks <- as.numeric(alive, units = "weeks")

#I have been alive for 1266.86 weeks. 

next_dob <- as.Date("2019, November 23", format = "%Y, %B %d") - now
next_days <- as.numeric(next_dob , units = "days")

#There are 263 days until my birthday. 

#############						   	                  
#Question #3							
#############

dob_time <- as.POSIXct("November 23, 1994 08:00:00 pm", format = "%B %d, %Y %I:%M:%S %p")
now_time <- Sys.time()
seconds <- now_time - dob_time
alive_secs <- as.numeric(seconds, units = "secs")

#I have been alive for 766197271.43 seconds.

#############						   	                  
#Question #4							
#############

install.packages("stringr")
library(stringr) 
 
type_factor <- as.factor(fueleffec$Type)
trim_type <- as.factor(str_trim(type_factor))
trim_type_upper <- as.factor(toupper(trim_type))
trim_type_upper[trim_type_upper=="MINIV."] <- paste0("MINIVAN")
trim_type_upper[trim_type_upper == "MINI"] <- paste0("MINIVAN")
trim_type_upper[trim_type_upper == "SED."] <- paste0("SEDAN")
trim_type_upper[trim_type_upper == "ST.WAGON"] <- paste0("WAGON")
fueleffec$Type <- trim_type_upper