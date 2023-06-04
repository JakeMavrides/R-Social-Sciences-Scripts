####  Set your working directory and bring in your .csv file
####  Name your data 'GSS2016'
setwd('C:/Users/Username/Location/Folder')
GSS2016 <- read.csv('Filename.csv', header=TRUE)

#Load the .csv-formatted data into RStudio. 
GSS2016 <- read.csv("C:/Users/Desktop/R/GSS2016.csv", header = TRUE )

#Generate value labels for gender.
GSS2016$sexrec <- factor(GSS2016$sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")) 

# Produce a basic table with raw frequencies for gender.
table(GSS2016$sexrec) 


# Add value labels for marital.
GSS2016$maritalrec <- factor(GSS2016$marital,
                             levels = c(1,2,3,4,5),
                             labels = c("Married", "Widowed", "Divorced", "Separated", 
                                        "Never Married")) 

# Produce a basic table with raw frequencies for marital status.
table(GSS2016$maritalrec) 


# Produce a table with raw frequencies for marital status and
# saves the table as an independent workspace object.
maritalrec.tb1 <- table(GSS2016$maritalrec) 

# Call the table that was just created to the output window.
maritalrec.tb1 

# Create the new frequencies table with the values in descending order
#(from most to least) and saves it as a new workspace object.
maritalrec.tb2 <- sort(maritalrec.tb1, decreasing = TRUE) 

# Call the new table to the output window.
maritalrec.tb2 


# Produce a table with proportions.
prop.table(maritalrec.tb2) 

# Round proportions to the thousandths place.
round(prop.table(maritalrec.tb2), 3) 


# Present frequencies as percentages (proportions multiplied by 100).
maritalrec.tb3 <- round(prop.table(maritalrec.tb2), 3)*100 

maritalrec.tb3

# Generate table with raw numbers, including missing values.
sexornt.tb1 <- table(GSS2016$sexornt) 

# Create a new table with percentages, including the percentage missing.
sexornt.tb1 <- round(prop.table(sexornt.tb1), 2)*100 

# Call the new table to the output window.
sexornt.tb1 

# Add value labels for sexorntrec.
GSS2016$sexorntrec <- factor(GSS2016$sexornt,
                             levels = c(1,2,3),
                             labels = c("Gay/Lesbian", "Bisexual", 
                                        "Heterosexual")) 

# Generate table with raw numbers, including missing values.
sexorntrec.tb1.miss <- table(GSS2016$sexorntrec) 

# Call the newly-developed table.
sexorntrec.tb1.miss 

# Create a new table with percentages, including the percentage missing.
sexorntrec.tb2.miss <- round(prop.table(sexorntrec.tb1.miss), 2)*100 

# Call the new table to the output window.
sexorntrec.tb2.miss 

# Create a new table object for childs.
childs.tb1 <- table(GSS2016$childs) 

# Produce a cumulative summary for childs.
cumsum.childs <- cumsum(childs.tb1) 

cumsum.childs

# Add value labels for degree.
GSS2016$degreerec <- factor(GSS2016$degree,
                            levels = c(0,1,2,3,4),
                            labels = c("LT HS", "HS", "Some College", "College",   
                                       "Grad School")) 

# Generate a table object of absolute frequencies for degree.
degreerec.tb1 <- table(GSS2016$degreerec)


degreerec.tb1


# Generate a table with raw frequencies and cumulative frequencies.
cbind(Freq=degreerec.tb1, Cum=cumsum(degreerec.tb1)) 

###############################################

maritalrec.tb1 <- table(GSS2016$maritalrec) 

# This code will run but the results are not intuitive since marital 
# status is a factor (qualitative/nominal) variable and does not move from low to high.
cbind(Freq=maritalrec.tb1, Cum=cumsum(maritalrec.tb1)) 
################################################

GSS2016$attendrec <- GSS2016$attend

GSS2016$attendrec[GSS2016$attendrec=="9"]=NA

# Create a table object with raw frequencies for religious service attendance.
attendrec.tb1 <- data.frame(table(GSS2016$attendrec)) 

# Based on the raw frequencies, generate relative frequencies (proportions).
attendrec.tb1$Prop <- prop.table(attendrec.tb1$Freq) 

# Calculate cumulative percentages.
attendrec.tb1$CumPct <- cumsum((attendrec.tb1$Prop)*100) 

# Call the table to the output window.
attendrec.tb1 



#################################################
GSS2016$prayrec <- GSS2016$pray

GSS2016$prayrec[GSS2016$prayrec=="0"]=NA
GSS2016$prayrec[GSS2016$prayrec=="8"]=NA
GSS2016$prayrec[GSS2016$prayrec=="9"]=NA

prayrec.tb1 <- data.frame(table(GSS2016$prayrec)) 

prayrec.tb1$Prop <- prop.table(prayrec.tb1$Freq)

prayrec.tb1$CumPct <- cumsum((prayrec.tb1$Prop)*100)
# Produce the same table as above (for church attendance), except for how often R prays (pray). 
# This is an example of the complications that can arise from reverse-coded items.
prayrec.tb1 
##############################################################




GSS2016$agerec <- GSS2016$age
GSS2016$agerec[GSS2016$agerec=="99"]=NA


#Collapse age into three categories. 
agegroup<-cut(GSS2016$agerec, c(0,35,59,90), labels = c("Young Adult", "Adult", "Older Adult")) 

table(agegroup)

# Create a data frame with the raw data from agegroup.
agegroup.tb1 <- data.frame(table(agegroup)) 

# Add relative frequencies to the data frame.
agegroup.tb1$Prop <- prop.table(agegroup.tb1$Freq) 

# Generate cumulative percentages for each age group.
agegroup.tb1$CumPct <- cumsum((agegroup.tb1$Prop)*100) 


# Produce the full table with absolute frequencies, relative frequencies, 
# and cumulative percentages in the output window.
agegroup.tb1 


####################################################
#An additional command can be entered in order to round the variables and create percentages:
agegroup<-cut(GSS2016$agerec, c(0,35,59,90), labels = c("young adult", "adult", "older adult")) 

agegroup.tb1 <- data.frame(table(agegroup)) 

agegroup.tb1$Prop <- prop.table(agegroup.tb1$Freq) 

# This is the same code as above, except for this line, 
# which rounds the proportions and cumulative percentages.
agegroup.tb1$Prop <- round((agegroup.tb1$Prop), 4)*100 

agegroup.tb1$CumPct <- cumsum(agegroup.tb1$Prop)

agegroup.tb1 
#####################################################


# Generate a basic/default histogram for age.
hist(GSS2016$agerec) 

# Create a new histogram object using the variable age. 
# Add a title for the histogram and a label for the x-axis (xlab).  
agerec.hist <- hist(GSS2016$agerec, 	
                    main = "Histogram of Age \n in the 2016 GSS",
                    xlab = "Respondent's Age") 

# Change the size of the bins.
agerec.hist <- hist(GSS2016$agerec, 
                    breaks = seq (10, 100, by = 10),
                    main = "Histogram of Age \n in the 2016 GSS",
                    xlab = "Respondent's Age") 


# Add color and produce relative frequencies.
agerec.hist <- hist(GSS2016$agerec, 
                    breaks = seq (10, 100, by = 10),
                    col = "lightsteelblue",
                    main = "Histogram of Age \n in the 2016 GSS",
                    xlab = "Respondent's Age",
                    freq = FALSE) 


GSS2016$tvhoursrec <- GSS2016$tvhours
GSS2016$tvhoursrec[GSS2016$tvhoursrec=="-1"]=NA
GSS2016$tvhoursrec[GSS2016$tvhoursrec=="98"]=NA
GSS2016$tvhoursrec[GSS2016$tvhoursrec=="99"]=NA

# This histogram has breaks defined (from 0 to 4 by 1 hour).
tvhoursrec.hist <- hist(GSS2016$tvhoursrec, 
                        breaks = seq(0,24, by = 1),
                        freq = FALSE,	
                        main = "Hours of Television Watched in the 2016 GSS",
                        xlab = "Per Day Hours of Television") 


# Install the package "moments."
install.packages("moments") 

# Require the package "moments" from the library.
library("moments") 


# Calculate skewness for the variable tvhoursrec in the GSS data.  
# Do not include missing values in the calculation.
skewness(GSS2016$tvhoursrec, na.rm = TRUE) 


# Calculate kurtosis for the variable tvhoursrec in the GSS data.  
# Do not include missing values in the calculation.
kurtosis(GSS2016$tvhoursrec, na.rm = TRUE) 


#This writes the content of the table "agegroup.tb1" to a comma-separated values file (.csv).
write.csv(agegroup.tb1) 

# Create a table (to paste into Excel) based on the information in the object "agegroup.tb1." 
# Copy the information to the clipboard and use tab-delimited formatting.
write.table(agegroup.tb1, "clipboard", sep = "\t") 
