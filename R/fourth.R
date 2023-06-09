2+3
a <- 2
a
b <- c(2,4,5,7)
b+2
getwd()
id <- c(1,2,3,4,5)
id <- seq(1,10,2)
id
id <- 1:5
id
names1 <- c("Mark","Jack","Jill","Anna","Tom")
gender <- c(0,0,1,1,0)
a <- 1:5
b <- 6:10
mat1 <- cbind(a,b)
mat2 <- rbind(a,b)

dat1 <- data.frame(ID=id, Name=names1, Gender=gender)
dat1
dat1[1,]
dat1[2,]
dat1[,2]
dat1[,c("ID","Gender")]
dat1$ID

se1 <- dat1$Gender==1
se1
!se1
dat1[se1,]

data1 <- read.csv("data/raw/perulung_ems.csv")
data1
str(data1)
head(data1)
tail(data1,10)
summary(data1)

data1$sex_ <- factor(data1$sex, levels = c(0,1), labels = c("f","m"))
table(data1$sex)
summary(data1$sex_ )

library(tidyverse)
data1%>% select(sex,sex_) %>% filter(sex_=="f") %>% table


#exercise
??framingham
install.packages("riskCommunicator")
library("riskCommunicator")
data(framingham)
###
# Framingham heart study data
framData <- riskCommunicator::framingham
str(framData)
summary(framData)
# We keep only the baseline examination data
framData_base <- subset(framData, TIME == 0)

# Numbers of rows and columns / show first rows and variables
head(framData_base)
sum(is.na(framData_base$HEARTRTE))
sum(!is.na(framData_base$HEARTRTE))
sum(framData_base$HEARTRTE, na.rm = TRUE) / 
  sum(!is.na(framData_base$HEARTRTE))
# Our median
median_hr <- median(framData_base$HEARTRTE, na.rm =  TRUE) 
median_hr
# Quartiles --------------------------------------------------------------------


## R-function ..................................................................

quart_hr <- quantile(framData_base$HEARTRTE, probs = c(0.25, 0.5, 0.75),
                     na.rm = TRUE)
quart_hr

library(psych)
