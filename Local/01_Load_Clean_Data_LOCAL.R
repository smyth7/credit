# Kaggle - Give Me Some Credit
# https://www.kaggle.com/c/GiveMeSomeCredit/data

# Author: Paul Smyth
# email: smyth7+kaggle@gmail.com

# # Required packages (Uncomment to install)
# install.packages("tidyverse")
# install.packages("ReporteRs")


# Description -------------------------------------------------------------

# This scripts loads and cleans the raw dataset cs-training.csv downloaded from Kaggle

# 



# Load libraries and functions -------------------------------------------------------------

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
df.description<- data.frame("ColName"=names(df.training),"ColType"= {sapply(df.training,class) %>% unname()},"NumUnique"={sapply(df.training,numUnique)%>% unname()}, "NAs"= {sapply(df.training,numNAs) %>% unname()},"PropNAs"= {sapply(df.training,propNAs) %>% unname()},"NumOutliers"= {sapply(df.training,numOutliers)})

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


# Simple loop for histograms / frequency polygons  - split 
for(i in colnames(df.training)[-1]){
  
  p<- ggplot(data=df.training, aes(x=df.training[paste(i)], colour = SeriousDlqin2yrs)) + geom_freqpoly(aes(y=..density..)) +xlab(paste(i)) 
  print(p)
}

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


# Feature Engineering -----------------------------------------------------

# There are many NAs in MonthlyIncome, but none in DebtRatio
# We work test the assumption that DebtRatio is some combination of Debt and Income. I would ask the client about this and any other unclear columns.

# Visual inspection shows that DebtRatios for unclients with MonthlyIncome=NA are strangely high. 

df.training[is.na(df.training$MonthlyIncome),] %>% View()

df.training$NA_MonthlyIncome <- as.integer(is.na(df.training$MonthlyIncome)) %>% as.factor()

ggplot(data=df.training, aes(x=NA_MonthlyIncome,y=DebtRatio)) + geom_boxplot()

# Bin age and calculate mean defaults per bin
df.training$EF_AgeGroup<- cut(df.training$age, right=FALSE, seq(0,110,by = 10),labels=seq(0,10))

df.training$EF_NumericDefault<- df.training$SeriousDlqin2yrs %>% as.character() %>% as.numeric()


df.training<- df.training %>% group_by(EF_AgeGroup) %>% mutate(EF_Mean_DefaultPerAge=mean(EF_NumericDefault)) 

# Cross-check mutate calculation
df.DefaultPerAge<- df.training %>% group_by(EF_AgeGroup) %>% summarise(EF_Mean_DefaultPerAge=mean(EF_NumericDefault)) 
