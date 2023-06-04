####  Set your working directory and bring in your .csv file
####  Name your data 'GSS2016'
setwd('C:/Users/Username/Location/Folder')
GSS2016 <- read.csv('Filename.csv', header=TRUE)

# Two-tailed (default) single proportion hypothesis test for two hypothetical proportions.
prop.test(453, 1432) 


# Test whether the sample proportion is significantly less than half of the population.
prop.test(453, 1432, alt = "less") 

GSS2016$sexorntrec <- factor(GSS2016$sexornt,
                             levels = c(1,2,3),
                             labels = c("Gay/Lesbian", "Bisexual", 
                                        "Heterosexual")) # Add value labels for sexorntrec.

sexorntrec.tb1 <- table(GSS2016$sexorntrec) #Save the table as an object in the workspace.

prop.table(sexorntrec.tb1) #Show proportions for the information in the table.
round(prop.table(sexorntrec.tb1), 2) # Round the proportions to the hundredths place.

chisq.test(sexorntrec.tb1) # Use the information in the table to run a goodness of fit test.

# Goodness of Fit test for sexual orientation based on empirically-derived expectations.
chisq.test(sexorntrec.tb1, p = c(.033, .036, .931)) 

GSS2016$sexrec <- factor(GSS2016$sex,
                         levels = c(1,2),
                         labels = c("Male", "Female"))

GSS2016$postliferec <- factor(GSS2016$postlife,
                              levels = c(1,2),
                              labels = c("Yes", "No"))

install.packages("gmodels") # Install the package to be used for the following example.

library("gmodels") # Attach the package to be used for the following example.

# Produce a full contingency table.
CrossTable(GSS2016$postliferec, GSS2016$sexrec, prop.chisq = FALSE, prop.t = FALSE) 

# Produce a contingency table with information on row and overall totals removed.
CrossTable(GSS2016$postliferec, GSS2016$sexrec, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)


# Produce a contingency table with information on column and overall totals removed.
CrossTable(GSS2016$postliferec, GSS2016$sexrec, prop.chisq = FALSE, prop.t = FALSE, prop.c = FALSE) 

detach("package:gmodels") # Detach the package "gmodels."

#Run the chi-square test and store the information as an object in the workspace. 
postlife.sex.chi <- chisq.test(GSS2016$postliferec, GSS2016$sexrec)

# Pull and show observed frequencies from the chisq.test object.
postlife.sex.chi$observed 

# Pull from the chisq.test object just created and present expected frequencies.
postlife.sex.chi$expected 
#Produce a table with all of the main results of the chi-square test of independence in the output window.
postlife.sex.chi 

GSS2016$fearrec <- factor(GSS2016$fear,
                          levels = c(1,2),
                          labels = c("Yes", "No"))

# Run the chi-square analysis and save the results as an object in the workspace.
fear.sex.chi <-chisq.test(GSS2016$fearrec, GSS2016$sexrec) 

#Call the information from the chisq.test object into the output window.
fear.sex.chi 

# Add value labels for sexorntrec.
GSS2016$sexorntrec <- factor(GSS2016$sexornt,
                             levels = c(1,2,3),
                             labels = c("Gay/Lesbian", "Bisexual", 
                                        "Heterosexual"))

# Add value labels for happy.
GSS2016$happyrec<- factor(GSS2016$happy,
                          levels = c(1,2,3),
                          labels = c("Very Happy", "Pretty Happy", "Not Happy"))

#Create object with chi-square information.
happy.sexornt.chi <-chisq.test(GSS2016$happyrec, GSS2016$sexorntrec) 

happy.sexornt.chi # Call the results of the analysis to the output window.

install.packages ("vcd") # Install the "vcd" package.

require("vcd") # Load the "vcd" package.

# Create a contingency table.
fear.sex.tb1 <- table(GSS2016$sexrec, GSS2016$fearrec) 

# Generate measures of association based on the information in the table.
assocstats(fear.sex.tb1) 

# Run the chi-square analysis and save the results as an object in the workspace.
fear.happy.chi <-chisq.test(GSS2016$fearrec, GSS2016$happyrec) 

# Call the information from the chisq.test object into the output window.
fear.happy.chi 

# Create a bivariate table with both variables in the analysis.
happy.fear.tb1 <- table(GSS2016$fearrec, GSS2016$happyrec) 

# Generate the measures of association based on the information in the table.
assocstats(happy.fear.tb1) 

detach("package:vcd") # Detach the "vcd" package.

