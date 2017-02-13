# Kaggle - Give Me Some Credit
# https://www.kaggle.com/c/GiveMeSomeCredit/data

# Author: Paul Smyth
# email: smyth7+kaggle@gmail.com

# # Required packages (Uncomment to install)
# install.packages("tidyverse")
# install.packages("ReporteRs")
# install.packages("stringr")

# Description -------------------------------------------------------------

# This scripts loads, cleans and explores the raw dataset cs-training.csv downloaded from Kaggle
# It creates a cleaned dataset stored in Data/data_credit_cleaned.csv, which is then used in the feature engineering script 02_Feature_Engineering.R


# Clear memory
rm(list = ls())

# Load libraries and functions -------------------------------------------------------------

source("Code/etl_functions.R")

library(tidyverse)
library(ReporteRs)
library(stringr)


# Load data ---------------------------------------------------------------

# Visual inspection showed the first column was row number, hence skipped here. The target variable is SeriousDlqin2yrs, which is loaded as a factor to ensure it cannot be manipulated as a double or integer. 


df.credit <- read_csv("~/Documents/Work/credit/Data/cs-training.csv", col_types = cols(X1 = col_skip(),SeriousDlqin2yrs = col_factor(levels = c("0","1"))))

View(df.credit)

# Check classes
sapply(df.credit,class)

# Summary
summary(df.credit)

# The - signs in column names can be inconvenient  - clean out. 
colnames(df.credit) <- gsub("-", "_", colnames(df.credit))



# Further column information
df.description<- data.frame("ColName"=names(df.credit),"ColType"= {sapply(df.credit,class) %>% unname()},"NumUnique"={sapply(df.credit,numUnique)%>% unname()}, "NAs"= {sapply(df.credit,numNAs) %>% unname()},"PropNAs"= {sapply(df.credit,propNAs) %>% unname()},"NumOutliers"= {sapply(df.credit,numOutliers)})

# There are many NAs in Monthly Income and NumberOfDependents
# 1st attempt - drop these values
# 2nd - impute? 

# Proportion of defaults

tb.defaults<- table(df.credit$SeriousDlqin2yrs) 
print(tb.defaults)

# We see there is 6.7 percent of defaults. Weighting and appropriate performance metrics will be required (MCC or Cohen's Kappa)
prop.table(tb.defaults)

# Initial Exploratory Analysis  -------------------------------------------

# Boxplot all numerical variables

for(i in colnames(df.credit)[-1]){
  
  p<- ggplot(data=df.credit, aes(x=SeriousDlqin2yrs,y=df.credit[paste(i)])) + geom_boxplot() +ylab(paste(i))  
  print(p)
}


# Simple loop for histograms / frequency polygons  - split 
for(i in colnames(df.credit)[-1]){
  
  p<- ggplot(data=df.credit, aes(x=df.credit[paste(i)], colour = SeriousDlqin2yrs)) + geom_freqpoly(aes(y=..density..)) +xlab(paste(i)) 
  print(p)
}

# Observations 

# Age -> shows an over-population over younger clients that default 
# Number of dependents ->  shows an over-population in clients that default with fewer dependents
# NumberOfTime60-89DaysPastDueNotWorse -> Over-population at high end? Could the large values here be a default code and not a number? 
# Real-esate loans -> defaults have proportionally fewer 
# NumberOfTimes90DaysLate - > slight over-population at higher end? Could the large values here be a default code and not a number? 
# NumberOfOpenCreditLinesAndLoans -> slight over-population at lower end
# Monthly Income -> shows no difference in initial plot
# Debt ratio ->  shows no difference in initial plot
# NumberOfTime30-59DaysPastDueNotWorse -> slight over-population at higher end? Could the large values here be a default code and not a number? 
# RevolvingUtilizationOfUnsecuredLines -> No obvious difference 

# Depending on the client's request, it may be of interest to carry out tests to understand if there are significant differences being defaults and non-defaults for the above variables e.g. Mann-Whitney-Wilcox test with effect size calculation to handle large sample size. 


# Explore unusual large values in Numbers of Times Late

table(df.credit$NumberOfTime30_59DaysPastDueNotWorse)
table(df.credit$NumberOfTimes90DaysLate)
table(df.credit$NumberOfTime60_89DaysPastDueNotWorse)

# Check other quantities

table(df.credit$NumberOfOpenCreditLinesAndLoans) # no gap or large number is entries in unusual value - no reason to believe there is a code
table(df.credit$NumberRealEstateLoansOrLines) # no large number is entries in unusual value - no reason to believe there is a code
table(df.credit$NumberOfDependents) # no large number is entries in unusual value - no reason to believe there is a code

# All show 96 and 98, which suggests that these are codes, e.g. unknown. Discuss this with client 

write.csv(df.credit,"Data/data_credit_cleaned.csv")
