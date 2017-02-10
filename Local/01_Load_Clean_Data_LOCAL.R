# Kaggle - Give Me Some Credit
# https://www.kaggle.com/c/GiveMeSomeCredit/data

# Author: Paul Smyth
# email: smyth7+kaggle@gmail.com

# # Required packages (Uncomment to install)
# install.packages("tidyverse")
# install.packages("ReporteRs")


# Description -------------------------------------------------------------

# This scripts loads, cleans and saves the raw dataset cs-training.csv downloaded from Kaggle 

# Load libraries and functions

source("Code/etl_functions.R")

library(tidyverse)
library(ReporteRs)


# Load data ---------------------------------------------------------------

# Visual inspection showed the first column was row number, hence skipped here. The target variable is SeriousDlqin2yrs, which is loaded as a factor to ensure it cannot be manipulated as a double or integer. 


df.training <- read_csv("~/Documents/Work/credit/Data/cs-training.csv", col_types = cols(X1 = col_skip(),SeriousDlqin2yrs = col_factor(levels = c("0","1"))))

View(df.training)

# Check classes
sapply(df.training,class)

# Summary
summary(df.training)

# Further column information
df.description<- data.frame("ColName"=names(df.training),"ColType"= {sapply(df.training,class) %>% unname()},"NumUnique"={sapply(df.training,numUnique)%>% unname()}, "NAs"= {sapply(df.training,numNAs) %>% unname()},"PropNAs"= {sapply(df.training,propNAs) %>% unname()})

# There are many NAs in Monthly Income and NumberOfDependents
# 1st attempt - drop these values
# 2nd - impute? 

# Proportion of defaults

tb.defaults<- table(df.training$SeriousDlqin2yrs) 
print(tb.defaults)

# We see there is 6.7 percent of defaults. Weighting and appropriate performance metrics will be required (MCC or Cohen's Kappa)
prop.table(tb.defaults)

# Initial Exploratory Analysis  -------------------------------------------

# Boxplot all numerical variables, split on SeriousDlqin2yrs

# Same y axis is a problem
df.plot<- gather(df.training, 2:11, key = "Variable", value = "Value")
p<- ggplot(data=df.plot, aes(x=SeriousDlqin2yrs,y=Value)) + geom_boxplot() + facet_wrap(~Variable)
p

# Simple loop for boxplots
for(i in colnames(df.training)[-1]){
  
  p<- ggplot(data=df.training, aes(x=SeriousDlqin2yrs,y=df.training[paste(i)])) + geom_boxplot() +ylab(paste(i))  
  print(p)
}

# Simple loop for histograms / frequency polygons
for(i in colnames(df.training)[-1]){
  
  p<- ggplot(data=df.training, aes(x=df.training[paste(i)], colour = SeriousDlqin2yrs)) + geom_freqpoly(aes(y=..density..)) +xlab(paste(i)) 
  print(p)
}

# The plot for age shows an over-population over younger clients that default 
# The plot for number of dependents shows an over-population in clients that default with fewer dependents
# NumberOfTime60-89DaysPastDueNotWorse -> under-population at high end
# defaults have proportionally fewer real-esate loans 
# NumberOfTimes90DaysLate - > slight over-population at higher end
# NumberOfOpenCreditLinesAndLoans -> slight over-population at lower end
