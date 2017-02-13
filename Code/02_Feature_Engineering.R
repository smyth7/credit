# Kaggle - Give Me Some Credit
# https://www.kaggle.com/c/GiveMeSomeCredit/data

# Author: Paul Smyth
# email: smyth7+kaggle@gmail.com

# # Required packages (Uncomment to install)
# install.packages("tidyverse")
# install.packages("ReporteRs")
# install.packages("stringr")

# Description -------------------------------------------------------------

# This scripts loads the cleaned dataset data_credit_cleaned.Rda that was prepared in 01_Load_Clean_Explore_Data.R
# Various features are engineered and explored:

# "EF_NumericDefault"  
# "EF_NA_MonthlyIncome"
# "EF_MonthlyDebt"
# "EF_DebtGroup"                        
# "EF_AgeGroup"
# "EF_Total_Num_Times_Late"
# "EF_TotalTimesLateGroup"              
# "EF_NumberOfDependents_Cleaned"
# "EF_DebtPerDependents"
# "EF_DebtPerDependenetsGroup"          
# "EF_UnsecuredLinesGroup"              

# The results dataset is stored in data_credit_modelling.Rda that is used in 03_Modelling.R

# Clear memory
rm(list = ls())

# Load libraries and functions -------------------------------------------------------------

source("Code/etl_functions.R")

library(tidyverse)
library(ReporteRs)
library(stringr)


# Load data ---------------------------------------------------------------

load(file="Data/data_credit_cleaned.Rda")


# Feature Engineering -----------------------------------------------------

# Feature Engineering : Debt -----------------------------------------------------

# There are many NAs in MonthlyIncome, but none in DebtRatio
# Data dictionary: Debt Ratio = Debt/Income. 

# Create numeric column for calculations, ensuring original column stays unchanged
df.credit$EF_NumericDefault<- df.credit$SeriousDlqin2yrs %>% as.character() %>% as.numeric()

# Visual inspection shows that DebtRatios for unclients with MonthlyIncome=NA are strangely high. 
df.credit[is.na(df.credit$MonthlyIncome),] %>% View()
df.credit$EF_NA_MonthlyIncome <- as.integer(is.na(df.credit$MonthlyIncome)) %>% as.factor()
ggplot(data=df.credit, aes(x=EF_NA_MonthlyIncome,y=DebtRatio)) + geom_boxplot()

# Create Monthly Deby Column
df.credit<- df.credit %>%  dplyr::mutate(EF_MonthlyDebt = DebtRatio*MonthlyIncome)  

# Working assumption based on order of magnitude of DebtRatios, to be discussed with client, Monthly Income NAs -> 1 for DebtRatio calculation

tmp.MI_NAs<- is.na(df.credit$EF_MonthlyDebt)
df.credit$EF_MonthlyDebt[tmp.MI_NAs]=df.credit$DebtRatio[tmp.MI_NAs]

summary(df.credit$EF_MonthlyDebt)

# Manual binning based on boxplot. Another option would be to maximize differences in default rate between, bins using library(smbinning) 
boxplot(df.credit$EF_MonthlyDebt)


# Bin age and calculate mean defaults per bin
tmp.stats<- boxplot.stats(df.credit$EF_MonthlyDebt)
df.credit$EF_DebtGroup<- cut(df.credit$EF_MonthlyDebt, right=FALSE, breaks = c(tmp.stats$stats,max(df.credit$EF_MonthlyDebt)+1),labels = c(1:5)) %>% as.numeric()
rm(tmp.stats)

# Recast 0 debt clients in own group 0
df.credit$EF_DebtGroup[df.credit$EF_MonthlyDebt==0]=0

# Recast as factor
df.credit$EF_DebtGroup<- as.factor(df.credit$EF_DebtGroup)

# # Calculate mean default per debt group 
# df.credit<- df.credit %>% group_by(EF_DebtGroup) %>% mutate(EF_Mean_DefaultDebtGroup=mean(EF_NumericDefault)) 

# Plot mean default per debt group 
df.DefaultPerDebtGroup<- df.credit %>% group_by(EF_DebtGroup) %>% summarise(EF_Mean_DefaultDebtGroup=mean(EF_NumericDefault)) 
ggplot(data=df.DefaultPerDebtGroup, aes(x=EF_DebtGroup,y= EF_Mean_DefaultDebtGroup)) + geom_point() +theme_bw()


# Feature Engineering: Age -----------------------------------------------------

# Bin age and calculate mean defaults per bin
df.credit$EF_AgeGroup<- cut(df.credit$age, right=FALSE, seq(0,110,by = 10),labels=seq(0,10))

# # Calculate mean default per debt group 
#df.credit<- df.credit %>% group_by(EF_AgeGroup) %>% mutate(EF_Mean_DefaultPerAge=mean(EF_NumericDefault)) 

# Cross-check mutate calculation
df.DefaultPerAge<- df.credit %>% group_by(EF_AgeGroup) %>% summarise(EF_Mean_DefaultPerAge=mean(EF_NumericDefault)) 

# Plot  - we see low default rate in low and high age range. 
ggplot(data=df.DefaultPerAge, aes(x=EF_AgeGroup,y= EF_Mean_DefaultPerAge)) + geom_point() +theme_bw()

# Data quality issue - ages 0-10?


# Feature Engineering: Total Days Late -----------------------------------------------------

# I believe it should be interesting to look at the total number of times that a client was late

# First clean out codes: 

# Inspection shows a client has the same code across all coulms. If correct, we should only see -3 or -6 in new column
df.credit$NumberOfTime30_59DaysPastDueNotWorse[df.credit$NumberOfTime30_59DaysPastDueNotWorse==96]=-1
df.credit$NumberOfTime60_89DaysPastDueNotWorse[df.credit$NumberOfTime60_89DaysPastDueNotWorse==96]=-1
df.credit$NumberOfTimes90DaysLate[df.credit$NumberOfTimes90DaysLate==96]=-1

df.credit$NumberOfTime30_59DaysPastDueNotWorse[df.credit$NumberOfTime30_59DaysPastDueNotWorse==98]=-2
df.credit$NumberOfTime60_89DaysPastDueNotWorse[df.credit$NumberOfTime60_89DaysPastDueNotWorse==98]=-2
df.credit$NumberOfTimes90DaysLate[df.credit$NumberOfTimes90DaysLate==98]=-2

df.credit<- df.credit %>% mutate( EF_Total_Num_Times_Late = NumberOfTime30_59DaysPastDueNotWorse + NumberOfTime60_89DaysPastDueNotWorse + NumberOfTimes90DaysLate)

# Check 
table(df.credit$EF_Total_Num_Times_Late) # Indeed - only all 96 or all 98

df.DefaultPerTimesLate<- df.credit %>% group_by(EF_Total_Num_Times_Late) %>% summarise(EF_Mean_Default=mean(EF_NumericDefault)) %>% ungroup()

# We see a reasoanable linear relationship between the number of times late and the mean number of defaults and the number of times late
ggplot(data=df.DefaultPerTimesLate, aes(x= EF_Total_Num_Times_Late ,y= EF_Mean_Default)) + geom_point() +theme_bw()

# Check other variables for 96 and 98 codes

df.credit %>% filter(EF_Total_Num_Times_Late< 0) %>% View() # All have 0.99 RevolvingUtilizationOfUnsecuredLines
df.credit %>% ungroup() %>%  filter(EF_Total_Num_Times_Late< 0) %>% select(SeriousDlqin2yrs) %>% table() %>% prop.table() # 54% default 

df.credit %>% filter(RevolvingUtilizationOfUnsecuredLines< 1) %>% View() # All have 0.99 RevolvingUtilizationOfUnsecuredLines

boxplot(df.credit$EF_Total_Num_Times_Late)
table(df.credit$EF_Total_Num_Times_Late,df.credit$SeriousDlqin2yrs)
table(df.credit$EF_Total_Num_Times_Late,df.credit$SeriousDlqin2yrs) %>% prop.table(1)

# We may want to bin this quantity too - take 3 simple bins: -ve, 0, +ve

df.credit$EF_TotalTimesLateGroup<- 1
df.credit$EF_TotalTimesLateGroup[df.credit$EF_Total_Num_Times_Late==0]=0
df.credit$EF_TotalTimesLateGroup[df.credit$EF_Total_Num_Times_Late<0]=-1
df.credit$EF_TotalTimesLateGroup<- as.factor(df.credit$EF_TotalTimesLateGroup)

# Feature Engineering: Debt Per Dependents -----------------------------------------------------

# Note it makes more sense to take a product, rather than a ratio, otherwise large debt and large number of dependents could be the same as small debt and fewer dependents.

# We assume that NumberOfDependents=NA -> 0: check with client

df.credit$EF_NumberOfDependents_Cleaned<- df.credit$NumberOfDependents
df.credit$EF_NumberOfDependents_Cleaned[is.na(df.credit$EF_NumberOfDependents_Cleaned)]=0

# More Dependents or More Debt - larger value
df.credit<- df.credit %>% mutate(EF_DebtPerDependents = EF_MonthlyDebt*(EF_NumberOfDependents_Cleaned+1))

summary(df.credit$EF_DebtPerDependents)

tmp.stats<- boxplot.stats(df.credit$EF_DebtPerDependents)
df.credit$EF_DebtPerDependenetsGroup<- cut(df.credit$EF_DebtPerDependents, right=FALSE, breaks = c(tmp.stats$stats,max(df.credit$EF_DebtPerDependents)+1),labels = c(1:5)) %>% as.numeric()
rm(tmp.stats)

# Recast 0 debt clients in own group 0
df.credit$EF_DebtPerDependenetsGroup[df.credit$EF_DebtPerDependenetsGroup==0]=0

# Recast as factor
df.credit$EF_DebtPerDependenetsGroup<- as.factor(df.credit$EF_DebtPerDependenetsGroup)

# # Calculate mean default per debt group 
# df.credit<- df.credit %>% group_by(EF_DebtPerDependenetsGroup) %>% mutate(EF_Mean_DefaultDebtPerDepedentGroup=mean(EF_NumericDefault)) 

# Plot mean default per debt group 
df.DebtPerDependenetsGroup<- df.credit %>% group_by(EF_DebtPerDependenetsGroup) %>% summarise(EF_Mean_DefaultDebtPerDepedentGroup=mean(EF_NumericDefault)) 
ggplot(data=df.DebtPerDependenetsGroup, aes(x=EF_DebtPerDependenetsGroup,y= EF_Mean_DefaultDebtPerDepedentGroup)) + geom_point() +theme_bw()

# Feature Engineering:  UnsecuredLinesGroup -----------------------------------------------------

# We saw above that 96 and 98 codes all have 0.999 recurring 

summary(df.credit$RevolvingUtilizationOfUnsecuredLines)

tmp.stats<- boxplot.stats(df.credit$RevolvingUtilizationOfUnsecuredLines)
df.credit$EF_UnsecuredLinesGroup<- cut(df.credit$RevolvingUtilizationOfUnsecuredLines, right=FALSE, breaks = c(tmp.stats$stats,max(df.credit$RevolvingUtilizationOfUnsecuredLines)+1),labels = c(1:5)) %>% as.numeric()
rm(tmp.stats)

# Recast 96,98 code clients in own group 0
df.credit$EF_UnsecuredLinesGroup[df.credit$EF_Total_Num_Times_Late< 0]=0

# Recast as factor
df.credit$EF_UnsecuredLinesGroup<- as.factor(df.credit$EF_UnsecuredLinesGroup)

# Check group assigned properly 
df.credit %>% filter(EF_Total_Num_Times_Late< 0) %>% View()

# # Calculate mean default per UnsecuredLinesGroup
# df.credit<- df.credit %>% group_by(EF_UnsecuredLinesGroup) %>% mutate(EF_Mean_Default_UnsecuredLinesGroup=mean(EF_NumericDefault)) 

# Plot mean default per debt group 
df.UnsecuredLinesGroup<- df.credit %>% group_by(EF_UnsecuredLinesGroup) %>% summarise(EF_Mean_Default_UnsecuredLinesGroup=mean(EF_NumericDefault)) 
ggplot(data=df.UnsecuredLinesGroup, aes(x=EF_UnsecuredLinesGroup,y= EF_Mean_Default_UnsecuredLinesGroup)) + geom_point() +theme_bw()

# We see that the mean number of defaults in the unsecured lines group is significantly higher than the other groups. 


# Rename and save
df.credit_modelling<- df.credit

save(df.credit_modelling, file = "Data/data.credit.modelling.Rda")
