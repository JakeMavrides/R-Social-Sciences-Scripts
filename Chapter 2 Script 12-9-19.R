####  Set your working directory and bring in your .csv file
####  Name your data 'GSS2016'
setwd('C:/Users/Username/Location/Folder')
GSS2016 <- read.csv('Filename.csv', header=TRUE)

# Uploading the libraries you will need.  This will vary from project to project.  
library(pastecs) 
library(Hmisc)
library(corrplot)

# Generate a table with raw frequencies for gender. 
# This will be the first (top) output below.
table(GSS2016$sex) 


#Recode the value labels for attributes in the variable gender.
#Change the variable attributes from integers to factors. 
GSS2016$sex <- factor(GSS2016$sex,
                      levels = c(1,2), 
                      labels = c("Male", "Female")) 


# Generate a table with count for sex attributes after adding value labels and changing variable to factor. 
table(GSS2016$sex) 


table(GSS2016$health)

# Create new (duplicate) variable.
GSS2016$healthFact <-(GSS2016$health) 
table(GSS2016$healthFact)

GSS2016$healthFact <-factor(GSS2016$health,
                            levels=c(1,2,3,4,8,9,0),
                            labels=c("Excellent", "Good", "Fair", "Poor",
                                     "Don't know", "No answer", "Not applicable"))
table(GSS2016$healthFact)


GSS2016$health


GSS2016$healthFact


# Create new (duplicate) variable.
GSS2016$healthBiNom <-(GSS2016$health) 


table(GSS2016$healthBiNom)

GSS2016$healthBiNom


GSS2016$healthBiNom[GSS2016$healthBiNom==0]=NA
GSS2016$healthBiNom[GSS2016$healthBiNom==8]=NA
GSS2016$healthBiNom[GSS2016$healthBiNom==9]=NA
GSS2016$healthBiNom[GSS2016$healthBiNom==1]=1
GSS2016$healthBiNom[GSS2016$healthBiNom==2]=1
GSS2016$healthBiNom[GSS2016$healthBiNom==3]=2
GSS2016$healthBiNom[GSS2016$healthBiNom==4]=2


GSS2016$healthBiNom
table(GSS2016$healthBiNom)

str(GSS2016$health)
str(GSS2016$healthFact)
str(GSS2016$healthBiNom)



GSS2016$healthBiNomL <-factor(GSS2016$healthBiNom,
                              levels=c(1,2),
                              labels=c("Good or better", "Less than good"))
GSS2016$healthBiNomL


table(GSS2016$healthBiNomL)


table(GSS2016$agekdbrn)


GSS2016$agekdbrnNEW<-GSS2016$agekdbrn
GSS2016$agekdbrnNEW[GSS2016$agekdbrn==0]=NA
GSS2016$agekdbrnNEW[GSS2016$agekdbrn==98]=NA
GSS2016$agekdbrnNEW[GSS2016$agekdbrn==99]=NA


table(GSS2016$agekdbrnNEW)


GSS2016$agekdbrnNEW[GSS2016$agekdbrn>35]= 1
GSS2016$agekdbrnNEW[GSS2016$agekdbrn>= 20 & GSS2016$agekdbrn<=35]= 2
GSS2016$agekdbrnNEW[GSS2016$agekdbrn<20]= 3


table(GSS2016$agekdbrnNEW)

GSS2016$agekdbrnNEW2[GSS2016$agekdbrn>35]= "Over 35"
GSS2016$agekdbrnNEW2[GSS2016$agekdbrn>=20 & GSS2016$agekdbrn<=35]= "25 to 35"
GSS2016$agekdbrnNEW2[GSS2016$agekdbrn<20]= "Less than 25"

table(GSS2016$agekdbrnNEW2)

table(GSS2016$chldidel)

# create new (duplicate) variable
GSS2016$chldidealNA <-(GSS2016$chldidel)
table(GSS2016$chldidealNA)


GSS2016$chldidealNA[GSS2016$chldidealNA=="-1"]=NA
GSS2016$chldidealNA[GSS2016$chldidealNA==9]=NA

table(GSS2016$chldidealNA)


GSS2016$chldidealNA


GSS2016$chldidealNAL <-factor(GSS2016$chldidealNA,
                              levels=c(0,1,2,3,4,5,6,7,8),
                              labels=c("No children", "1 child",
                                       "2 children ","3 children","4 children",
                                       "5 children", "6 children","7 or more",
                                       "As many as you want"))
table(GSS2016$chldidealNAL)


# Be sure to open the library for pastecs.
library(pastecs) 
# Running descriptive statistics.
stat.desc(GSS2016$chldidealNA) 


# Creating new variable.
GSS2016$zodiac2<-(GSS2016$zodiac) 

# Observing frequencies of categories.
table(GSS2016$zodiac2) 


# Recoding the missing values as NA. 
GSS2016$zodiac2[GSS2016$zodiac2==98]=NA  
GSS2016$zodiac2[GSS2016$zodiac2==99]=NA
# Observing frequencies after recoding.  
table(GSS2016$zodiac2)  

# Setting the working directory. If it is in the same folder as 
# when you started - you don't need to setwd again. 
setwd('C:/Users/username/Desktop/file') 
# Reading in the CSV file NotApp and calling it NOT.
NOT <- read.csv('NotApp.csv', header=TRUE) 

is.na(NOT$ABC)
is.na(NOT$XYZ)

is.na(NOT)

!is.na(NOT)

na.omit(NOT)

newOMIT<-na.omit(NOT)

NOT$multiplied<-na.pass(NOT$ABC*NOT$XYZ)

mean(NOT$ABC)
mean(NOT$ABC, na.rm = TRUE)

mean(NOT$multiplied, na.rm = TRUE)
median(NOT$multiplied, na.rm = TRUE)
# Whenever you are obtaining a mean value, it is always a good idea to get the 
# standard deviation as well.  
sd(NOT$multiplied, na.rm = TRUE) 

mean(NOT$multiplied[!is.na(NOT$multiplied)])
median(NOT$multiplied[!is.na(NOT$multiplied)])
sd(NOT$multiplied[!is.na(NOT$multiplied)])

#Open the pastecs library if you haven't already done so. 
library(pastecs) 
stat.desc(NOT$multiplied)

NOT$mult1<-NOT$multiplied
NOT$mult2<-NOT$multiplied

# Mean imputation
NOT$mult1[is.na(NOT$mult1)]<-mean(NOT$multiplied[!is.na(NOT$multiplied)])


# Median imputation
NOT$mult2[is.na(NOT$mult2)]<-median(NOT$multiplied, na.rm = TRUE) 

stat.desc(NOT$mult1)


table(GSS2016$maeduc)


table(GSS2016$paeduc)


GSS2016$maeduc[GSS2016$maeduc==97]=NA
GSS2016$maeduc[GSS2016$maeduc==98]=NA
GSS2016$maeduc[GSS2016$maeduc==99]=NA

GSS2016$paeduc[GSS2016$paeduc==97]=NA
GSS2016$paeduc[GSS2016$paeduc==98]=NA
GSS2016$paeduc[GSS2016$paeduc==99]=NA

table(GSS2016$maeduc)

table(GSS2016$paeduc)


GSS2016$ParentEDUC<-(GSS2016$paeduc+GSS2016$maeduc) 
table(GSS2016$ParentEDUC)


GSS2016$emailhr[GSS2016$emailhr=='-1']=NA
GSS2016$emailhr[GSS2016$emailhr==998]=NA
GSS2016$emailhr[GSS2016$emailhr==999]=NA

hist(GSS2016$emailhr)


boxplot(GSS2016$emailhr, horizontal = FALSE)
# Horizontal = FALSE will produce a vertical boxplot, while Horizontal = TRUE will produce a horizontal boxplot. 
boxplot(GSS2016$emailhr, horizontal = TRUE) 

boxplot.stats(GSS2016$emailhr)

table(GSS2016$emailhr)


# Creating the new variable 'emailhrNoOUT' from the original variable 'emailhr'.
GSS2016$emailhrNoOUT<-GSS2016$emailhr  

# Note that we chose to remove values larger than 26, instead of exact values.  
GSS2016$emailhrNoOUT[GSS2016$emailhrNoOUT>26]= NA  

# a histogram of the new variable to compare to the original variable histogram.
hist(GSS2016$emailhrNoOUT) 

# Note that the uppercase "T" or "F" can be used instead of writing out TRUE or FALSE. 
boxplot(GSS2016$emailhrNoOUT, horizontal = F) 
boxplot(GSS2016$emailhrNoOUT, horizontal = T)

boxplot.stats(GSS2016$emailhrNoOUT)

table(GSS2016$emailhrNoOUT)

Emailhrwithout26<-(GSS2016$emailhr[GSS2016$emailhr <26])

GSS2016$emailhrwithout15<-(GSS2016$emailhr)
GSS2016$emailhrwithout15[GSS2016$emailhr > 15]=NA

hist(GSS2016$emailhrwithout15)
table(GSS2016$emailhrwithout15)


boxplot(GSS2016$emailhrwithout15, horizontal = F)
boxplot(GSS2016$emailhrwithout15, horizontal = T)


boxplot.stats(GSS2016$emailhrwithout15)
Emailhrwithout15<-(GSS2016$emailhr[GSS2016$emailhr <15])

