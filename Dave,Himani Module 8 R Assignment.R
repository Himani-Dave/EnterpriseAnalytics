"
****************************************************************
Name:Himani Dave
Student Number:A00178443

ANA1002 Module 8 R Assignment
****************************************************************
"
#############								   	                  
#Question #1						
#############

# a) 

airquality <- read.csv("airquality.csv", header = TRUE, na.strings = c("", "NA"))
sapply(airquality, function(x) sum(is.na(x)))

#There are 37 missing values in Ozone, 7 missing values in Solar.R, 16 missing values in Wind,
#5 missing values in Temp, and 0 missing values in Month and Day.

# b) 

airquality.complete <- na.omit(airquality)
dim(airquality.complete)
anyNA(airquality.complete)

temp.mean <- mean(airquality.complete$Temp)
temp.mean

#The mean temperature using listwise deletion is 77.98958.

# c) 

temp.mean.pairwise <- mean(airquality$Temp, na.rm = TRUE)
temp.mean.pairwise

#The mean temperature using pairwise deletion is 77.91216.

# d) 

which(is.na(airquality$Temp))

#The rows at indices 9, 61, 82, 90, and 146 have missing temperature values. 

# e) 

install.packages("naniar")
library(naniar)

gg_miss_var(airquality)

#The missing variable plot shows that there are five missing values in the temperature variable. 

gg_miss_upset(airquality)

#The upset plot shows that there are 5 missing values for temperature. 

#Thus, there is a difference in means of temperature using listwise and pairwise deletion.

# f) 

gg_miss_var(airquality, facet = Month)

#The plot shows the missing variables. We can see that the maximum missing values for ozone
#are in June. That might be because of the extreme heat. That for wind was in July. 

# g) 

ozone.outlier <- boxplot(airquality$Ozone, ylab="Ozone", main="Boxplot of Ozone Levels", outcol="red")
oz.out <- ozone.outlier$out

#There are two outliers in ozone values: 135, 168

ozone.complete <- airquality %>% 
  filter(!airquality$Ozone %in% oz.out)


#############								   	                  
#Question #2						
#############

demog <- read.csv("demographics.csv", header = TRUE)

# a) 

install.packages("editrules")
library(editrules)

edit.age <- editset(c("age<125", "age >0"))
edit.age
edit.vote <- editset(c("vote>=0", "vote <= 1"))
edit.vote
edit.year <- editset(c("year==2000"))
edit.year
edit.female <- editset(c("female>=0", "female<=1"))
edit.female

# b) 

violate.age <- violatedEdits(edit.age, demog)
violate.age
summary(violate.age)

#This rule is violated 6 times. 

violate.vote <- violatedEdits(edit.vote, demog)
violate.vote
summary(violate.vote)

#This rule is violated 8 times.

violate.year <- violatedEdits(edit.year, demog)
violate.year
summary(violate.year)

#This rule is violated 4 times. 

violate.female <- violatedEdits(edit.female, demog)
violate.female
summary(violate.female)

#This rule is 8 times. 

plot(violate.age)
plot(violate.vote)
plot(violate.year)
plot(violate.female)

# d) 

duplicated(demog)
table(duplicated(demog))

#There are 542 duplicate records in the demographics database.  

# e) 

uni.demog <- unique(demog)