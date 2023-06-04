####  Set your working directory and bring in your .csv file
####  Name your data 'GSS2016'
setwd('C:/Users/Username/Location/Folder')
GSS2016 <- read.csv('Filename.csv', header=TRUE)

library(Hmisc)
library(corrplot)
library(pastecs)

# Recoding the variables for education, maternal education, paternal education, hours of tv 
# watched/day, and # of siblings, hours on the world wide web/week. 
GSS2016$educ[GSS2016$educ==98]=NA
GSS2016$educ[GSS2016$educ==99]=NA

GSS2016$maeduc[GSS2016$maeduc==97]=NA
GSS2016$maeduc[GSS2016$maeduc==98]=NA
GSS2016$maeduc[GSS2016$maeduc==99]=NA

GSS2016$paeduc[GSS2016$paeduc==97]=NA
GSS2016$paeduc[GSS2016$paeduc==98]=NA
GSS2016$paeduc[GSS2016$paeduc==99]=NA

GSS2016$tvhours[GSS2016$tvhours=='-1']=NA
GSS2016$tvhours[GSS2016$tvhours==98]=NA

GSS2016$sibs[GSS2016$sibs==98]=NA
GSS2016$sibs[GSS2016$sibs==99]=NA

GSS2016$wwwhr[GSS2016$wwwhr=='-1']=NA
GSS2016$wwwhr[GSS2016$wwwhr==998]=NA
GSS2016$wwwhr[GSS2016$wwwhr==999]=NA

#running the tables of the variables that have been recoded to 
# double check that the non-answers have been recoded as NA.
table(GSS2016$educ)
table(GSS2016$maeduc)
table(GSS2016$paeduc)
table(GSS2016$tvhours)
table(GSS2016$sibs)
table(GSS2016$wwwhr)

cm<-GSS2016[, c("educ","maeduc", "paeduc", "tvhours", "sibs", "wwwhr")]

head(cm, 10)

rcorr(as.matrix(cm))

# Data frame of extracted data for the plot.
cmED<-rcorr(as.matrix(cm)) 

# Extract the correlation coefficients.
cmED$r 

# Extract p-values.
cmED$P 


corrplot(cmED$r, type="full", order="hclust",  tl.col = "black",
         col = c("black", "white"), bg = "lightblue", 
         p.mat = cmED$P, sig.level = 0.05, insig="blank")

# Combine 5 shades of blue to create your own color palette of blues for next matrix.
# Change type to "lower". and background (bg = ) to light grey.

corrplot(cmED$r, type="lower", order="hclust",tl.col = "black",  
         col = c("aliceblue", "lightblue", "cornflowerblue", "blue", 
         "darkblue"), bg = "lightgrey", p.mat = cmED$P, sig.level = 0.05, 
         insig="blank")

#Compute new variable by adding together the years of 
# education from the mother and the father.
GSS2016$mpeduc<-(GSS2016$maeduc+GSS2016$paeduc)
table(GSS2016$mpeduc)

cm2<-GSS2016[, c("educ","mpeduc", "tvhours", "sibs", "wwwhr")] 
rcorr(as.matrix(cm2))

cm2ED<-rcorr(as.matrix(cm2)) # Data frame of extracted data for the plot.
cm2ED$r # Extract the correlation coefficients.
cm2ED$P # Extract p-values.

corrplot(cm2ED$r, type="full", order="hclust",  tl.col = "black",
         col = c("black", "white"), bg = "lightblue", 
         p.mat = cmED$P, sig.level = 0.05, insig="blank")

corrplot(cm2ED$r, type="lower", order="hclust", tl.col = "black",  
         col = c("aliceblue", "lightblue", "cornflowerblue", "blue", "darkblue"), 
         bg = "lightgrey", p.mat = cmED$P, sig.level = 0.05, insig="blank")


# With the data name attached to each variable (this would be good to use if you were pulling data from different datasets.
reg1<-lm(cm2$educ~cm2$mpeduc + cm2$tvhours + cm2$sibs + cm2$wwwhr) 

# With the data name for all variables at the end.
reg2<-lm(educ~ mpeduc + tvhours + sibs + wwwhr, data = cm2) 

# Opening the results for the linear model.
reg1 
reg2 


summary(reg1)

# We can see the interaction objects that have been added to our original call.
reg3<-lm(formula = educ ~ mpeduc + tvhours + sibs + mpeduc * tvhours + 
           mpeduc * sibs + tvhours * sibs, data = cm2)   

reg3

summary(reg3)

confint(reg3)

# Calling for the residuals from the multiple regression.
resid(reg3) 

# Getting a histogram of residuals ## This will help to check for normal distribution of residuals.
hist (resid(reg3)) 

# Added the color 'cornflowerblue' to the script for the fill.
hist (resid(reg3), col="cornflowerblue")  

#checking the VIF on the regression model without the interaction terms. 
library (car)
vif(reg1)


#Remove the non-answers from the dwelling variable.
GSS2016$dwelBi[GSS2016$dwelown==8]=NA
GSS2016$dwelBi[GSS2016$dwelown==9]=NA
GSS2016$dwelBi[GSS2016$dwelown==0]=NA

#Recode the dwelling variable into binary variable (0,1).
GSS2016$dwelBi[GSS2016$dwelown==1]=1
GSS2016$dwelBi[GSS2016$dwelown==2]=0
GSS2016$dwelBi[GSS2016$dwelown==3]=0

table(GSS2016$dwelown)
table(GSS2016$dwelBi)

#Recode sex into a newly recoded variable sexrec and change the code for female from 2 to 0.
table(GSS2016$sex)
GSS2016$sexrec[GSS2016$sex==1]=1
GSS2016$sexrec[GSS2016$sex==2]=0
table(GSS2016$sexrec)

table(GSS2016$age)
GSS2016$age[GSS2016$age==99]=NA  
table(GSS2016$age)


#Logistic Regression
LogistRegresA<-glm(formula = dwelBi~sexrec + age, data = GSS2016, family = "binomial")
summary(LogistRegresA)
exp(LogistRegresA$coefficients)
exp(confint(LogistRegresA))




