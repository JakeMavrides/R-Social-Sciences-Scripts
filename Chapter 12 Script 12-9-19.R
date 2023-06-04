####  Set your working directory and bring in your .csv file
####  Name your data 'GSS2016'
setwd('C:/Users/Username/Location/Folder')
GSS2016 <- read.csv('Filename.csv', header=TRUE)


# Remember to recode the data first.
GSS2016$sei10[GSS2016$sei10=='-1']=NA

# Table to check recode (rounding numbers to 2 decimal places.  
table(round(GSS2016$sei10, digits = 2))

library(moments)
skewness(GSS2016$sei10)

#We can make histograms with different options for how many breaks and various labels.
hist(GSS2016$sei10, col="grey90")

hist(GSS2016$sei10,breaks=seq(min(GSS2016$sei10),max(GSS2016$sei10),
                              length=18),col="grey70") 


hist(GSS2016$sei10,breaks=seq(min(GSS2016$sei10),max(GSS2016$sei10), 				
    length=30), xlab = "SEI10", ylab = "Frequency", main="Histogram
     of Frequencies of SEI10 Results", col="grey50")

# Create new data of transformed sei10 data by squaring it, cubing it, and taking the square root. 
sei10squared<-(GSS2016$sei10^2)      
sei10cubed<-(GSS2016$sei10^3)       
sei10root<-sqrt(GSS2016$sei10)

# Run the command for skewness for the original, and the three transformed indices.
skewness(GSS2016$sei10)
skewness(sei10squared)
skewness(sei10cubed)
skewness(sei10root)

hist(GSS2016$sei10, col="lightblue1", main = "Histogram of SEI10")
hist(sei10squared, col="skyblue1", main = "Histogram of SEI10 Squared")
hist(sei10cubed, col=" skyblue3", main = "Histogram of SEI10 Cubed")
hist(sei10root, col=" skyblue4", main = "Histogram of Square Root of SEI10")

# Creating new indices (variables) for each log calculation.
sei10natlog<-log(GSS2016$sei10)      
sei10log2<-log2(GSS2016$sei10)
sei10log10<-log10(GSS2016$sei10)

# Running histograms for each of the sei10 log transformation (original, natural log, log2, log 10).     
hist(GSS2016$sei10, col="lightskyblue", main = "Histogram of SEI10")
hist(sei10natlog, col="dodgerblue1", main = "Histogram of SEI10 Natural Log")
hist(sei10log2, col="royalblue1", main = "Histogram of SEI10 Log Base 2")
hist(sei10log10, col = "blue2",main = "Histogram of 
     Square Root of Log Base 10")

# Creating sei10scale from sei10.
sei10scale<-round(scale(GSS2016$sei10),digits=2) 

# Running histograms and skewness for sei10 and sei10scale variables.     
hist(GSS2016$sei10, col="grey85", main = "Histogram of SEI10")
hist(sei10scale, col="grey45", main = "Histogram of SEI10 Scaled")
skewness(GSS2016$sei10)
skewness(sei10scale)




##Multicolinearity
table(GSS2016$happy)      
table(GSS2016$income16)
table(GSS2016$satjob)
table(GSS2016$satfin)
table(GSS2016$educ)
table(GSS2016$attend)
table(GSS2016$sexfreq)
table(GSS2016$polviews)


GSS2016$happy[GSS2016$happy==8]=NA
GSS2016$happy[GSS2016$happy==9]=NA
GSS2016$income16[GSS2016$income16==27]=NA
GSS2016$income16[GSS2016$income16==98]=NA
GSS2016$satjob[GSS2016$satjob==0]=NA
GSS2016$satjob[GSS2016$satjob==8]=NA
GSS2016$satjob[GSS2016$satjob==9]=NA
GSS2016$satfin[GSS2016$satfin==0]=NA
GSS2016$satfin[GSS2016$satfin==8]=NA
GSS2016$satfin[GSS2016$satfin==9]=NA
GSS2016$educ[GSS2016$educ==98]=NA
GSS2016$educ[GSS2016$educ==99]=NA
GSS2016$attend[GSS2016$attend==9]=NA
GSS2016$polviews[GSS2016$polviews==8]=NA
GSS2016$polviews[GSS2016$polviews==9]=NA
GSS2016$polviews[GSS2016$polviews==0]=NA
GSS2016$sexfreq[GSS2016$sexfreq==8]=NA
GSS2016$sexfreq[GSS2016$sexfreq==9]=NA
GSS2016$sexfreq[GSS2016$sexfreq=="-1"]=NA

#Open the library that is needed
library(Hmisc)

cm<-GSS2016[, c("income16","satjob", "satfin", "happy", "educ", "attend", "sexfreq", "polviews")]

# It is always good to check to see that your variables are appearing as you would expect. 
head(cm,8)


rcorr(as.matrix(cm))

# Assigning the correlation matrix to a data frame.
CM<-rcorr(as.matrix(cm)) 

# extracting the r- and p-values to be used in the correlation matrix plot.
CM$r 
CM$P


# Create a color ramp of blue colors to be used in the correlation matrix plot.
blues <- colorRampPalette(c("lightblue1", "darkblue"))


library(corrplot)

# Open the library corrplot to run the multiple correlation plot.
corrplot(CM$r, type="full", order="hclust", addrect = 3, 
         col = blues(100), bg = "white",
         p.mat = CM$P, sig.level = 0.05, insig="pch", 
         tl.col = "black", tl.srt = 45)

# Be sure to add the libraries for the multiple imputation.
library(mice) 
library(VIM)

# Recode the non-value data to missing data (if you haven't already).
GSS2016$age[GSS2016$age==99]=NA  
GSS2016$satfin[GSS2016$satfin==0]=NA
GSS2016$satfin[GSS2016$satfin==8]=NA
GSS2016$satfin[GSS2016$satfin==9]=NA
GSS2016$income16[GSS2016$income16==27]=NA
GSS2016$income16[GSS2016$income16==98]=NA

# Assign the variables you are working with to a small group data frame.
Group2<-GSS2016[, c("income16", "age", "satfin")]  

# Run tables to check that data was recoded.
table(GSS2016$income16) 
table(GSS2016$age)
table(GSS2016$satfin)

# Looking at pattern of missing data by variable.
md.pattern(Group2)

# Creating individual matrices of missing/observed data for pairs within the regression model.
md.pairs(Group2) 

# Running the multiple imputation and assigning it to IMP2.
IMP2<-mice(Group2) 

# Running a linear regression (including the summary).
summary(lm(income16~age+satfin, data=Group2)) 

# Imputing the data that has been pooled from the multiple imputations. 
fit<-with(IMP2,lm(income16~age+satfin)); pool(fit) 
summary(pool(fit))

















