"
****************************************************************
Name:Himani Dave
Student Number:A00178443

ANA1002 Module 12 R Assignment
****************************************************************
"
#############						   	                  
#Question #1							
#############

normality <- read.csv("normality.csv", header = TRUE)

# a) 

mean(normality$Value) #mean = 87.09145
median(normality$Value) #median = 87.92
max(normality$Value) # maximum = 100.07
min(normality$Value) #minimum = 47.95

#Since median > mean, we can say that the data is left skewed. 

# b) 

install.packages("moments")
library(moments)

skewness(normality$Value) #skewness = -1.661031
kurtosis(normality$Value) #kurtosis = 7.92018

#Since |skew| > 1, the distribution is highly skewed.
#Since kurtosis > 4, the distribution is leptokurtic.

# c) 

hist(normality$Value)
boxplot(normality$Value)

#The histogram shows an extreme skew to the left. It is unimodal. 
#The boxplot shows that there are quite a few outliers in the data and the distribution is
#skewed.

# d) 

qqnorm(normality$Value)
qqline(normality$Value, col="red", lty=2)

#The distribution shows many outliers and they are mostly at the beginning of the line
#on the left. 

# e) 

shapiro.test(normality$Value)

#W = 0.88518, p-value = 1.701e-10
#Since the p-value is much smaller than the alpha value of 0.05, the distribution is  
#NOT NORMAL

#############						   	                  
#Question #2							
#############

array.ds <- read.csv("array.csv", header = TRUE)

# a) 

array.skew <- sapply(array.ds, skewness)
max <- max(array.skew) # maximum = 0.5264599
min <- min(array.skew) # minimum = -0.4031243
match(max, array.skew) 
match(min, array.skew)

#Thus, the most positively skewed data is in column X34 whereas the most negatively
#skewed data is in column X60.

# b) 

abs.mag <- list()
for(each in array.skew){
  if(abs(each) > 0.3){
    abs.mag <- c(abs.mag, match(each, array.skew))
  }
}
abs.mag

#Thus, there are 18 columns with high skewness.
#These columns are X5, X6, X9, X14, X23, X27, X31, X34, X39, X55, X60, X66, X67, X72, X81, X83, X94, X100.

# c) 

high.skew <- names(array.ds) %in% c("X5", "X6", "X9", "X14", "X23", "X27", "X31", "X34", "X39", "X55", "X60", "X66", "X67", "X72", "X81", "X83", "X94", "X100")
normal.array <- array.ds[!high.skew]
