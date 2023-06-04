####  Set your working directory and bring in your .csv file
####  Name your data 'GSS2016'
setwd('C:/Users/Username/Location/Folder')
GSS2016 <- read.csv('Filename.csv', header=TRUE) 

GSS2016$agerec <- GSS2016$age
GSS2016$agerec[GSS2016$agerec=="99"]=NA

# Collapse the variable age into three categories with labels for each.

agegroup<-cut(GSS2016$agerec, c(0,35,59,90), labels = c("Young Adult", "Adult", "Older Adult")) 

# Check the recoding of the variable agegroup.  
table(agegroup) 


GSS2016$wwwhrrec <- GSS2016$wwwhr
GSS2016$wwwhrrec[GSS2016$wwwhrrec=="-1"]=NA
GSS2016$wwwhrrec[GSS2016$wwwhrrec=="998"]=NA
GSS2016$wwwhrrec[GSS2016$wwwhrrec=="999"]=NA

# Create an independent object to recode wwwhr (wwwhr.30).
wwwhr.30 <- GSS2016$wwwhrrec 

# Recode the wwwhr variable so that values greater than "30" are recoded to "30."
wwwhr.30[wwwhr.30 > 30] <- 30 

# Produce a means table. Aggregate by age group and show the mean number of hours spent on the Internet per week.
aggregate(wwwhr.30 ~ agegroup, FUN = mean) 

# Generate grouped boxplots for Internet use by age groups.
boxplot(wwwhr.30 ~ agegroup, 
        horizontal = TRUE,
        main = "Boxplots for Internet Use by Age Group",
        xlab = "Hours/Week Spent on Internet") 


# Create an independent object with information regarding an analysis of variance for Internet Use across different age groups.
age.int.aov <- aov(wwwhr.30 ~ agegroup) 

# Produce the results of the ANOVA in the output window.
age.int.aov 
# Summarize the results of the ANOVA hypothesis test.
summary(age.int.aov) 

# Run a series of pairwise tests to assess significant differences between the age groups based on the omnibus ANOVA test.
TukeyHSD(age.int.aov)

# Add value labels for sexorntrec.
GSS2016$sexorntrec <- factor(GSS2016$sexornt,
                             levels = c(1,2,3),
                             labels = c("Gay/Lesbian", "Bisexual", 
                                        "Heterosexual")) 
table(GSS2016$sexorntrec)

GSS2016$chldidelrec <- GSS2016$chldidel
GSS2016$chldidelrec[GSS2016$chldidelrec =="-1"]=NA
GSS2016$chldidelrec[GSS2016$chldidelrec =="8"]=NA
GSS2016$chldidelrec[GSS2016$chldidelrec =="9"]=NA

# Check the means across each sexual orientation group.
so.ideal <- aggregate(GSS2016$chldidelrec ~ GSS2016$sexorntrec, FUN = mean) 

so.ideal

# Create a new object and transpose the information in the table of grouped means created above.
mean.so.ideal <- t(so.ideal [-1]) 

# Replace marital status labels in the transposed table.
colnames(mean.so.ideal) <- so.ideal [ , 1] 


# Use the barplot command to create a bar chart for each of the means with any additional configurations.
barplot(mean.so.ideal,
        col = "lightsteelblue",
        main = "Ideal Number of Children\n by Sexual Orientation",
        xlab = "Sexual Orientation",
        ylab = "Mean Ideal Number of Children") 

# Generate grouped boxplots for ideal number of children by sexual orientation.
boxplot(GSS2016$chldidelrec ~ GSS2016$sexorntrec, 
        horizontal = TRUE,
        main = "Ideal Number of Children by Sexual Orientation",
        xlab = "Self-Reported Ideal Number of Children to Have") 

# Create an independent object with an ANOVA for ideal number of children across sexual orientation groups.
so.ideal.aov <- aov(GSS2016$chldidelrec ~ GSS2016$sexorntrec) 

#Produce information about the ANOVA.
so.ideal.aov 

# Produce the results of the hypothesis test.
summary(so.ideal.aov) 


GSS2016$tvhoursrec <- GSS2016$tvhours
GSS2016$tvhoursrec[GSS2016$tvhoursrec=="-1"]=NA
GSS2016$tvhoursrec[GSS2016$tvhoursrec=="98"]=NA
GSS2016$tvhoursrec[GSS2016$tvhoursrec=="99"]=NA

# Assess the mean number of hours of television by age group.
aggregate(GSS2016$tvhoursrec ~ agegroup, FUN = mean) 

# Check grouped boxplots for tvhoursrec by agegroup.
boxplot(GSS2016$tvhoursrec ~ agegroup, 
        horizontal = TRUE,
        main = "Time Spent Watching TV by Age Group",
        xlab = "Number of Hours/Day Watching TV") 


# Create an independent object to recode tvhours.
tvhours.15 <- GSS2016$tvhoursrec 

# Recode the variable so that values greater than "15" are recoded to "15."
tvhours.15[tvhours.15 > 15] <- 15 



# Check grouped boxplots for tvhours.15 by agegroup.
boxplot(tvhours.15 ~ agegroup, 
        horizontal = TRUE,
        main = "Time Spent Watching TV (Recoded) \n by Age Group",
        xlab = "Number of Hours/Day Watching TV") 




# Create an ANOVA object with information regarding an analysis of variance for number of hours of television watched across each of the different age groups.
age.tv.aov <- aov(tvhours.15 ~ agegroup) 

age.tv.aov

# Produce all of the information for the analysis of variance to test for difference in the mean number of hours of television watched across different age groups.
summary(age.tv.aov) 

# Run a series of pairwise tests to assess significant differences between the age groups based on the omnibus ANOVA results.
TukeyHSD(age.tv.aov) 

install.packages("lsr") # Install the package "lsr."

require("lsr") # Attach the package "lsr."

# Calculate eta-squared based on the ANOVA object.
etaSquared(age.tv.aov) 

#Generate value labels for gender.
GSS2016$sexrec <- factor(GSS2016$sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")) 

# Create an independent object to recode wwwhr (wwwhr.30).
wwwhr.30 <- GSS2016$wwwhrrec 

# Recode the wwwhr variable so that values greater than "30" are recoded to "30."
wwwhr.30[wwwhr.30 > 30] <- 30 

# Generate a box plot for the independent effect of gender on Internet use.
boxplot (wwwhr.30 ~ GSS2016$sexrec, horizontal = TRUE,
         main = "Internet Use by Sex (Main Effects)",
         xlab = "Weekly Number of Hours on the Internet") 

GSS2016$childsrec <- GSS2016$childs
GSS2016$childsrec[GSS2016$childsrec=="9"]=NA

# Recode the childsrec variable so that values greater than "0" are recoded to "1." Thus, parental status is "child free" (0) and "has any children" (1).
GSS2016$childsrec[GSS2016$childsrec > 0] <- 1 

#Define value labels for childsrec.
childsrec <- factor(GSS2016$childsrec,
                    levels = c(0,1),
                    labels = c("No Kids", "Has Kids")) 

# Generate a box plot for the independent effect of parental status on Internet use.
boxplot (wwwhr.30 ~ childsrec, horizontal = TRUE,
         main = "Internet Use by Parental Status (Main Effects)",
         xlab = "Number of Hours on the Internet Weekly") 

# Generate a box plot for interaction effects on Internet use between gender and parental status.
boxplot (wwwhr.30 ~ GSS2016$sexrec:childsrec, horizontal = TRUE,
         main = "Internet Use by Sex & Parental Status (Interaction)",
         xlab = "Number of Hours on the Internet Weekly") 

# Run a two-way analysis of variance using parental status and gender (IV) and number of hours per week spent on the Internet (DV). 
genpar.int.aov <- aov(wwwhr.30 ~ GSS2016$sexrec + childsrec + GSS2016$sexrec:childsrec) 


# Call the results of the two-way ANOVA from the ANOVA object to the output window.
summary(genpar.int.aov) 
