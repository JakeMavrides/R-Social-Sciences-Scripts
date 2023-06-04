####  Set your working directory and bring in your .csv file
####  Name your data 'GSS2016'
setwd('C:/Users/Username/Location/Folder')
GSS2016 <- read.csv('Filename.csv', header=TRUE)


# Add value labels for marital.
GSS2016$maritalrec <- factor(GSS2016$marital,
                             levels = c(1,2,3,4,5),
                             labels = c("Married", "Widowed", "Divorced", "Separated", 
                                        "Never Married")) 

# Add value labels to marital and create a frequencies table as a separate object in the workspace.
maritalrec <- table(GSS2016$maritalrec) 




maritalrec


# Create a barplot based on the raw data information in the "marital" object with a main and axis titles.
barplot (maritalrec, main="Marital Status (GSS 2016)",
         xlab="Marital Status",
         ylab="Frequency") 


# Generate a bar chart for marital with values presented in descending order.
barplot(maritalrec[order(maritalrec, decreasing = TRUE)],
        main="Marital Status (GSS 2016)",
        xlab="Marital Status",
        ylab="Frequency") 

# Add value labels for gender.
GSS2016$sexrec <- factor(GSS2016$sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")) # Add value labels for gender.


sexrec <- table(GSS2016$sex) # Move the sex vector into an independent object.
barplot(sexrec [order (sexrec)], # Create a barplot ordered by the frequencies in the data.
        horiz = TRUE, # Customize the direction.
        las = 1, # Customize the axis labels.
        col = c("royalblue3", "lightsteelblue"), #Add colors for each bar.
        border = NA, # Do not use a border. 
        main = "Gender of the \n GSS 2016 Sample", # Give the bar chart a main title.
        xlab = "Frequency") # Add a label along the x-axis.


agegroup<-cut(GSS2016$age, c(0,35,59,90), labels = c("young adult", "adult", "older adult")) 

agegroup.tb1 <- data.frame(table(agegroup)) 

agegroup.tb1$Prop <- prop.table(agegroup.tb1$Freq) 

agegroup.tb1$CumPct <- cumsum((agegroup.tb1$Prop)*100) 


# This is from Chapter 3, which created cumulative percentages for each of the three age collapsed age categories.
agegroup.tb1 

# Create a pie chart based on the cumulative percentages with default configurations.
pie(agegroup.tb1$CumPct) 


agelbl <- c("Young Adults", "Adults", "Older Adults")# Step 1: Add labels to the slices.
agepct <- round(agegroup.tb1$Prop*100) # Step 2: Create an object with percentages.
agelbl <- paste(agelbl, agepct) #Step 3: Paste the percentages to the labels.
agelbl <- paste(agelbl,"%",sep="") # Step 4: Paste the "%" sign to the labels.
pie(agegroup.tb1$CumPct, labels = agelbl, #Generate the pie chart with the labels.
    main="Age Groups in the GSS 2016", # Add a main title. 
    radius = 1.0, #Change the radius of the pie chart.
    clockwise = TRUE, # Start clockwise at 12:00 (90)
    col = c("lightsteelblue", "royalblue3", "darkblue"), # Customize the colors.
    cex.main = 1.5) # Resize the main title to be twice as large as the default.




GSS2016$sexfreqrec <- GSS2016$sexfreq
GSS2016$sexfreqrec[GSS2016$sexfreqrec=="-1"]=NA
GSS2016$sexfreqrec[GSS2016$sexfreqrec=="8"]=NA
GSS2016$sexfreqrec[GSS2016$sexfreqrec=="9"]=NA

# Create a new object with the variable sex frequency.
sexfreqrec.tb1 <- table(GSS2016$sexfreqrec)

#Generate a proportions table.
sexfreqrec.prop <-prop.table(sexfreqrec.tb1) 

# Produce the cumulative summary for the variable sex frequency based on the proportions table. Multiply the proportions by 100.
sexfreqrec.cumpct <- cumsum((sexfreqrec.prop)*100) 

# Create a line plot to illustrate the cumulative frequencies.  (The letter "o" is for points plotted over a line.).
plot(sexfreqrec.cumpct, type = "o", 
     main = "Cumulative Frequency Polygon \n for Sex Frequency",
     xlab = "Frequency of Sexual Activity (Ordered Categories)",
     ylab = "Cumulative Percent") # Additional commands allow for modifications-filling in the circles for data markers (pch), colors for data markers (col), title (main), x-axis label (xlab), and y-axis label (ylab).


GSS2016$agerec <- GSS2016$age
GSS2016$agerec[GSS2016$agerec=="99"]=NA

# Generate a basic boxplot for age with default configurations.
boxplot(GSS2016$agerec, 
        main = "Distribution of Age", # Add a title along the x-axis.
        col = "blue") # Add color.



GSS2016$sei10rec <- GSS2016$sei10
GSS2016$sei10rec[GSS2016$sei10rec=="-1"]=NA

boxplot(GSS2016$sei10rec, # Generate a boxplot for variable sei10.
        main = "Socioeconomic Index Score (GSS2016)", #Add a main title.
        horizontal = TRUE, # Change the direction to horizontal (default is vertical).
        xlab = "SEI", # Add a label along the x-axis to identify the variable.
        col = "slategray", # Customize the color of the box plot.
        boxwex = 1.5, # Resize the boxplot.
        whisklty = 1,  # Change the whisker line type.
        staplelty = 2,  # Change the lines at the end.
        outcol = "dark blue") # Add a color for possible outliers.


# Present a histogram for sei10.
sei10rec.hist <- hist(GSS2016$sei10rec, 
                      col = "blue",
                      main = "Histogram of Socioeconomic Index Scores",
                      xlab = "SEI Score") 

#Define value labels for happiness.
GSS2016$happyrec<- factor(GSS2016$happy, 
                          levels = c(1,2,3),
                          labels = c("Very Happy", "Pretty Happy", "Not Happy"))

# Create a bivariate table using the sex and happy variables.
genderhappy <- table(GSS2016$happyrec, GSS2016$sexrec) 

#Produce the bivariate table in the output window.
genderhappy 

# Generate a stacked bar chart (default) using the information from the bivariate table.
barplot(genderhappy, 
        main = "Happiness by Gender in the GSS 2016",
        xlab = "Self-Reported Happiness", # Add main title and x-axis label.
        col = c("royalblue3", "slateblue", "lightblue"), # Customize colors.
        legend = rownames(genderhappy)) # Denote which categories will be in the legend.

barplot(genderhappy,
        main = "Happiness by Gender in the GSS 2016",
        xlab = "Self-Reported Happiness",
        col = c("royalblue3", "slateblue", "lightblue"),
        legend = rownames(genderhappy), # Same code used for stacked bar chart above.
        beside = TRUE) # Since the default is "stacked," this command clusters the bars instead.


GSS2016$partnersrec <- GSS2016$partners
GSS2016$partnersrec[GSS2016$partnersrec=="-1"]=NA
GSS2016$partnersrec[GSS2016$partnersrec=="98"]=NA
GSS2016$partnersrec[GSS2016$partnersrec=="99"]=NA

#Generate a numeric object for partners.
partnersrec<- as.numeric(GSS2016$partnersrec) 

# Add value labels for marital.
GSS2016$maritalrec <- factor(GSS2016$marital,
                             levels = c(1,2,3,4,5),
                             labels = c("Married", "Widowed", "Divorced", "Separated", 
                                        "Never Married")) 


#Create an object for marital status.
maritalrec <-(GSS2016$maritalrec) 

# Generate a table with mean sex partners for each marital status group and save it as an object.
partnersmarital <- aggregate(partnersrec ~ maritalrec, FUN = mean)  

partnersmarital # Call the table to the output window.

# Create a new object and transpose the information in the table of grouped means.
mean.partners <- t(partnersmarital [-1]) 

# Call the transposed object to the output window.
mean.partners 

# Replace marital status labels in the transposed table.
colnames(mean.partners) <- partnersmarital [ , 1] 

# Call the group means with column names to the output window.
mean.partners 


# Use the barplot command to create a bar chart for each of the means with any additional configurations.
barplot(mean.partners,
        col = "blue",
        main = "Sex Partners in the Last Year\n by Marital Status",
        xlab = "Marital Status",
        ylab = "Mean Number of Sex Partners") 


boxplot(GSS2016$partnersrec ~ GSS2016$maritalrec,
        bowex = .05,
        whisklty = 1,
        outpch = 16,		
        main = "Number of Sex Partners in the Last Year \n by Marital Status",
        xlab = "Marital Status",
        ylab = "Sex Partners in the Last Year", 
        outcol = "royalblue3") # Add a color for possible outliers.


GSS2016$maeducrec <- GSS2016$maeduc
GSS2016$maeducrec[GSS2016$maeducrec=="97"]=NA
GSS2016$maeducrec[GSS2016$maeducrec=="98"]=NA
GSS2016$maeducrec[GSS2016$maeducrec=="99"]=NA

GSS2016$paeducrec <- GSS2016$paeduc
GSS2016$paeducrec[GSS2016$paeducrec=="97"]=NA
GSS2016$paeducrec[GSS2016$paeducrec=="98"]=NA
GSS2016$paeducrec[GSS2016$paeducrec=="99"]=NA

# Generate a basic scatterplot with the default settings.
plot(GSS2016$maeducrec, GSS2016$paeducrec) 

# Additional commands allow for modifications-filling in the circles for data markers (pch), colors for data markers (col), title (main), x-axis label (xlab), and y-axis label (ylab).
plot(GSS2016$maeducrec, GSS2016$paeducrec,
     pch = 16,
     col = "blue",
     main = "Mothers' Education and Fathers' Education",
     xlab = "Mother's Education",
     ylab = "Father's Education") 

# Other modifications add a regression line (abline) for a linear model (lm) with a certain color (col) and width (lwd).
abline(lm(GSS2016$maeducrec ~ GSS2016$paeducrec),
       col = "darkblue",
       lwd = 2) 
