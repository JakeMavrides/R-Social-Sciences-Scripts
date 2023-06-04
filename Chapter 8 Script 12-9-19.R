####  Set your working directory and bring in your .csv file
####  Name your data 'GSS2016'
setwd('C:/Users/Jake/Desktop/R')
GSS2016 <- read.csv('GSS2016.csv', header=TRUE)


GSS2016$partnrs5rec <- GSS2016$partnrs5
GSS2016$partnrs5rec[GSS2016$partnrs5rec=="-1"]=NA
GSS2016$partnrs5rec[GSS2016$partnrs5rec=="95"]=NA
GSS2016$partnrs5rec[GSS2016$partnrs5rec=="98"]=NA
GSS2016$partnrs5rec[GSS2016$partnrs5rec=="99"]=NA

# Run a two-tailed one-sample hypothesis test with the default options for number of sexual partners in the last 5 years.
t.test(GSS2016$partnrs5rec) 

# Run a two-tailed one-sample t-test for a predefined value of 5.
t.test(GSS2016$partnrs5rec, mu = 5) 

# Run a one-tailed one-sample t-test for a predefined value (5).
t.test(GSS2016$partnrs5rec, mu = 5, alternative="greater") 

# Run a one-tailed one-sample hypothesis for whether the number of sexual partners in the last five years is significantly less than 1.9 using an alpha of .01.
t.test(GSS2016$partnrs5rec, mu = 1.9, alternative="less", conf.level = 0.99) 


GSS2016$childsrec <- GSS2016$childs
GSS2016$childsrec[GSS2016$childsrec=="9"]=NA

# Recode the childsrec variable so that values greater than "0" are recoded to "1." Thus, parental status is "child free" (0) and "has any children" (1).
GSS2016$childsrec[GSS2016$childsrec > 0] <- 1 

GSS2016$childsrec <- factor(GSS2016$childsrec,
                    levels = c(0,1),
                    labels = c("No Kids", "Has Kids")) #Define value labels for childsrec.

table(GSS2016$childsrec)


GSS2016$wwwhrrec <- GSS2016$wwwhr
GSS2016$wwwhrrec[GSS2016$wwwhrrec=="-1"]=NA
GSS2016$wwwhrrec[GSS2016$wwwhrrec=="998"]=NA
GSS2016$wwwhrrec[GSS2016$wwwhrrec=="999"]=NA




hist(GSS2016$wwwhrrec,
     main = "Histogram for Weekly Internet Use",
     xlab = "Number of Hours on the Internet Each Week") # Check the histogram for the interval/ratio variable (weekly hours of Internet use).


# Generate grouped boxplots of Internet use (wwwhrrec) by parental status (childsrec).
boxplot(GSS2016$wwwhrrec ~ GSS2016$childsrec, 
        horizontal = TRUE,
        main = "Boxplot for Internet Use and Parental Status",
        xlab = "Number of Hours/Week Spent Online") 

# Test whether or not the difference between those with children (1) and those without (0) in hours using the Internet is significantly different from 0.
t.test(GSS2016$wwwhrrec ~ GSS2016$childsrec) 

# Create an independent object to recode wwwhr (wwwhr.30).
wwwhr.30 <- GSS2016$wwwhrrec 

# Recode the wwwhr variable so that values greater than "30" are recoded to "30."
wwwhr.30[wwwhr.30 > 30] <- 30 


boxplot(wwwhr.30 ~ GSS2016$childsrec, 
        horizontal = TRUE,
        main = "Boxplot for Internet Use (Recoded) \n and Parental Status",
        xlab = "Number of Hours/Week Spent Online")

# Run a one-tailed t-test with a revised confidence interval.
t.test(wwwhr.30 ~ GSS2016$childsrec,
       alternative = "greater",
       conf.level = .99) 


GSS2016$sibsrec <- GSS2016$sibs
GSS2016$sibsrec[GSS2016$sibsrec=="-1"]=NA
GSS2016$sibsrec[GSS2016$sibsrec=="98"]=NA
GSS2016$sibsrec[GSS2016$sibsrec=="99"]=NA

# Generate a histogram for the dependent variable.
hist(GSS2016$sibsrec, 
     main = "Histogram for Number of Siblings",
     xlab = "Number of Siblings") 


GSS2016$sexrec <- factor(GSS2016$sex,
                         levels = c(1,2),
                         labels = c("Male", "Female"))

table(GSS2016$sexrec) # Check frequencies for the independent dichotomous variable (sexrec).

# Generate grouped boxplots for number of siblings by sex.
boxplot(GSS2016$sibsrec ~ GSS2016$sexrec, 
        horizontal = TRUE,
        main = "Number of Siblings by Sex",
        xlab = "Siblings") 


# Run a two-tailed independent-samples t-test to assess whether or not the difference between men and women (sexrec) in number of siblings (sibsrec) is significantly different from 0.
t.test(GSS2016$sibsrec ~ GSS2016$sexrec) 


# Generate grouped boxplots for number of siblings by parental status.
boxplot(GSS2016$sibsrec ~ GSS2016$childsrec, 
        horizontal = TRUE,
        main = "Number of Siblings by Parental Status",
        xlab = "Siblings") 

#Create an independent object to recode siblings (sibs.15).
sibs.15 <- GSS2016$sibsrec 

# Recode the sibs variable so that values greater than "15" are recoded to "15."
sibs.15[sibs.15 > 15] <- 15 

# Generate grouped boxplots for number of siblings by parental status.
boxplot(sibs.15 ~ GSS2016$childsrec, 
        horizontal = TRUE,
        main = "Number of Siblings by Parental Status",
        xlab = "Siblings") 

# Run a one-tailed t-test with a revised confidence interval.
t.test(sibs.15 ~ GSS2016$childsrec,
       alternative = "less") 


# Add value labels for nativity.
GSS2016$bornrec <- factor(GSS2016$born,
                          levels = c(1,2),
                          labels = c("U.S.", "Non-U.S.")) 

# Check the frequencies for the dichotomous variable (bornrec).
table(GSS2016$bornrec) 


# Generate grouped boxplots for siblings (sibs.15) by nativity (bornrec).
boxplot(sibs.15 ~ GSS2016$bornrec, 
        horizontal = TRUE,
        main = "Number of Siblings by Nativity",
        xlab = "Siblings") 

# Run an independent samples t-test.
t.test(sibs.15 ~ GSS2016$bornrec) 

# Install the package "lsr."
install.packages("lsr") 

# Attach the package "lsr."
require("lsr") 

# Run the t-test with the cohensD function.
cohensD(sibs.15 ~ GSS2016$bornrec) 

GSS2016$maeducrec <- GSS2016$maeduc
GSS2016$maeducrec[GSS2016$maeducrec=="97"]=NA
GSS2016$maeducrec[GSS2016$maeducrec=="98"]=NA
GSS2016$maeducrec[GSS2016$maeducrec=="99"]=NA

GSS2016$paeducrec <- GSS2016$paeduc
GSS2016$paeducrec[GSS2016$paeducrec=="97"]=NA
GSS2016$paeducrec[GSS2016$paeducrec=="98"]=NA
GSS2016$paeducrec[GSS2016$paeducrec=="99"]=NA

# Run a paired t-test to test the difference between mother's education (maeducrec) and father's education (paeducrec).
t.test(GSS2016$maeducrec, GSS2016$paeducrec, paired = TRUE) 


#Attach the package "lsr."
require("lsr") 

#Run the t-test with the cohensD function.
cohensD(x = GSS2016$paeducrec, y = GSS2016$maeducrec, method = "paired") 



