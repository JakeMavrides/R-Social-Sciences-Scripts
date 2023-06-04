####  Set your working directory and bring in your .csv file
####  Name your data 'GSS2016'
setwd('C:/Users/Username/Location/Folder')
GSS2016 <- read.csv('Filename.csv', header=TRUE)

#Making a scatterplot before re-coding our variables of participants' highest year 
# of education and participants' mothers' highest year of education correlation analysis.
plot(GSS2016$educ, GSS2016$tvhours,
     pch = 16,
     col = "Dodgerblue3",
     main = "Scatterplot for Education Level and TV Watching Habits",
     xlab = "Highest Year of School Completed", 
     ylab = "TV Hours Watched/Day")

GSS2016$educ[GSS2016$educ==98]=NA
GSS2016$educ[GSS2016$educ==99]=NA
GSS2016$tvhours[GSS2016$tvhours==-1]=NA
GSS2016$tvhours[GSS2016$tvhours==98]=NA
GSS2016$tvhours[GSS2016$tvhours==99]=NA


# Making a scatterplot of the datapoints for participants' highest year of education 
#and participants' mothers' highest year of education correlation analysis. 
plot(GSS2016$educ, GSS2016$tvhours,
     pch = 16,
     col = "Dodgerblue3",
     main = "Scatterplot for Education Level and TV Watching Habits",
     xlab = "Highest Year of School Completed", 
     ylab = "TV Hours Watched/Day")


# Recoding the variables and removing the unknown values, if needed.
GSS2016$educ[GSS2016$educ==98]=NA 
GSS2016$educ[GSS2016$educ==99]=NA 

GSS2016$tvhours[GSS2016$tvhours=='-1']=NA
GSS2016$tvhours[GSS2016$tvhours==98]=NA

# Be sure to open the library for pastecs.
library(pastecs) 

# Running descriptive statistics.
stat.desc(GSS2016$tvhours) 
stat.desc(GSS2016$educ)


# Running the Pearson's correlation analysis to test for a relationship between 
# the two variables. 
cor.test(GSS2016$educ, GSS2016$tvhours) 

# looking at the data for the variables prior to recoding.
table(GSS2016$satfin)
table(GSS2016$happy)

# Recoding the variables.
GSS2016$satfin[GSS2016$satfin==0]=NA  
GSS2016$satfin[GSS2016$satfin==8]=NA
GSS2016$satfin[GSS2016$satfin==9]=NA

GSS2016$happy[GSS2016$happy==0]=NA
GSS2016$happy[GSS2016$happy==8]=NA
GSS2016$happy[GSS2016$happy==9]=NA

# Looking at the frequency tables a second time to double check the recoding of the variables.
table(GSS2016$satfin) 
table(GSS2016$happy)


# We need to add which test to run, ("spearman") and we also need to tell it to use the 
# pairwise complete observations  command to maneuver around the missing values.  
cor.test(GSS2016$happy, GSS2016$satfin, 
         method = "spearman", 
         use = "pairwise.complete.obs", 
         exact = FALSE)  


table(GSS2016$happy, GSS2016$satfin)
library(ggplot2)
library(Hmisc)
library(corrplot)

# Recoding the variables.
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

# Creating a dataset of the variables in the matrix.
MatrixData<-GSS2016[, c("educ", "maeduc", "paeduc","tvhours")] 

# View first 4 rows of data.
head(MatrixData, 4) 
# View first 25 rows of data.
head(MatrixData, 25) 

rcorr(as.matrix(MatrixData))

# Creating dataset of the correlation matrix.
EdMat<-rcorr(as.matrix(MatrixData)) 

# Extract the correlation coefficients.
EdMat$r 

# Extract p-values.
EdMat$P 

# This is each individual correlation that are being run in the correlation 
# matrix- this is just a simple way to double check the work.  

cor.test(GSS2016$educ, GSS2016$maeduc)
cor.test(GSS2016$paeduc, GSS2016$educ)
cor.test(GSS2016$paeduc, GSS2016$maeduc)
cor.test(GSS2016$educ, GSS2016$tvhours) 
cor.test(GSS2016$maeduc, GSS2016$tvhours) 
cor.test(GSS2016$paeduc, GSS2016$tvhours)

corrplot(EdMat$r, type="upper", order="hclust", tl.col = "black",
         p.mat = EdMat$P, sig.level = 0.01, insig = "blank")


hist(GSS2016$maeduc)
hist(GSS2016$educ)

#First run the scatterplot.
plot(GSS2016$maeduc,GSS2016$educ,
     pch = 8,
     col = "blue2",
     main = "Scatterplot for Education Level of Participants and 
     Participants' Mothers",
     xlab = "Mother's Education", 
     ylab = "Participant Education")

# Then add the regression line.
abline(lm(GSS2016$educ~GSS2016$maeduc)) 

# If running stat.desc , remember to add the library for pastecs, if you need to.
library(pastecs) 
# Descriptive statistics for the 2 variables.
stat.desc(GSS2016$maeduc) 
stat.desc(GSS2016$educ) 

# run the linear model (lm) regression of mother's education on 
# participant's education and save the regression to an object 
# (I chose reg.ed for regression education).
reg.ed<-lm(GSS2016$educ~GSS2016$maeduc) 

reg.ed

# Run summary to see the results
summary(reg.ed) 

confint(reg.ed)

#To predict the highest year of school completed by the participant based on 
# created values for the mothers' highest years of school completed.
predict(reg.ed) 

#To predict the CI for highest year of school participant.  
predict(reg.ed, interval = "predict") 

#Let's change the vsriable for satisisfied financially to a binary - 0
#0 = not satisfied
#1 = satisfied
GSS2016$satfinBi[GSS2016$satfin==1]=1  
GSS2016$satfinBi[GSS2016$satfin==2]=1
GSS2016$satfinBi[GSS2016$satfin==3]=0
table(GSS2016$satfinBi)


str(GSS2016$satfinBi)
GSS2016$satfinBi <-as.factor(GSS2016$satfinBi)
str(GSS2016$satfinBi)

#Running the logistic regression with the binary financial satisfaction variable:
logistic <-glm(GSS2016$satfinBi~GSS2016$educ, family = binomial)
summary(logistic)

logistic2 <-glm(satfinBi ~ educ, data = GSS2016, family = binomial)
summary(logistic2)

#calculations for the logistic regression probability predictions
-0.28608+0.09278*16  
1/(1+exp(-1.1984)) 

-0.28608+0.09278*11  
1/(1+exp(-.7345)) 


