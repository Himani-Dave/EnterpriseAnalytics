"
****************************************************************
Name:Himani Dave
Student Number:A00178443

ANA1002 Module 4 R Assignment
****************************************************************
"
#############								   	                  
#Question #1						
#############

addresses <- read.table("addresses.txt", header = TRUE, sep = ",", quote = "", comment.char="", stringsAsFactors=FALSE)

#The error is produced as the number of columns is different for different rows. The extra data can be removed to correct this error. 

#############								   	                  
#Question #2							
#############

install.packages("sas7bdat")
library(sas7bdat)
SASMedical<-read.sas7bdat("medical.sas7bdat")
cor(SASMedical)

#Age and medexp are the most coorelated variables.

sasmedic<- SASMedical[ ,-c(1,2,4,6)]
write.csv(sasmedic, file="medical.csv")

#############								   	                  
#Question #3							
#############

# a)

msleep_data <-read.csv("https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv", header=TRUE)

# b)

install.packages("dplyr")
library(dplyr)

msleep.data.subset <- msleep_data %>% 
  group_by(order, genus) %>% 
  mutate(prop_REM = sleep_rem/sleep_total) %>% 
  summarize(mean.sleep_tot = mean(sleep_total, na.rm = TRUE), 
            mean.prop_rem=mean(prop_REM, na.rm=TRUE), 
            min.sleep_tot=min(sleep_total, na.rm=TRUE), 
            max.sleep_tot=max(sleep_total, na.rm=TRUE)) %>% 
  filter(mean.sleep_tot>5) %>% 
  arrange(mean.sleep_tot) 

# c)

install.packages("xlsx")
library(xlsx)
write.xlsx(msleep, "mammal_sleep.xlsx", sheetName = "Original", col.names = TRUE, row.names = FALSE, append = FALSE)

# d)

write.xlsx(as.data.frame(m.subset), "mammal_sleep.xlsx", sheetName = "Sub", col.names = TRUE, row.names = FALSE, append = TRUE)
