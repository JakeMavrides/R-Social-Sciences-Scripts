(5+4+9+10+6+21+1)/7

help.start()

?getwd

install.packages("tidyverse")

library (tidyverse)

getwd()

setwd("C:/Users/Desktop/R")

GSS2016 <- read.csv("GSS2016.csv")

library(haven)
newdata <- read_sav("~/Desktop/SPSS/newdata.sav")
View(newdata)

library(haven)
demo <-    
  read_sav("https://study.sagepub.com/system/files/demo.sav")
View(demo)

x <- c(65,85,90,100,85)
y <- c(77,72,88,65,82)

cor (x,y)

testscoresdata <- data.frame(x,y)
View(testscoresdata)

save(testscoresdata, file = "testscores.RData")

save(GSS2016, file = "GSS.RData")
