####  Set your working directory and bring in your .csv file
####  Name your data 'GSS2016'
setwd('C:/Users/Username/Location/Folder')
GSS2016 <- read.csv('Filename.csv', header=TRUE)


mean(GSS2016$age)

GSS2016$age[GSS2016$age==99]=NA

mean(GSS2016$age, na.rm = TRUE)

median(GSS2016$age, na.rm=TRUE)

table_age <- table(GSS2016$age)

subset(table_age, table_age==max(table_age))

range(GSS2016$age, na.rm=TRUE)

IQR(GSS2016$age, na.rm=TRUE)

var(GSS2016$age, na.rm=TRUE)

sd(GSS2016$age, na.rm=TRUE)

getOption("max.print")

options(max.print=5000)

scale(GSS2016$age, center=TRUE, scale=TRUE)

(72 - (mean(GSS2016$age, na.rm=TRUE)))/(sd(GSS2016$age, na.rm=TRUE))


library(dplyr)

GSS2016random <-sample_n(GSS2016, 5, replace = FALSE)

GSS2016sub <- subset(GSS2016, sex==1)


