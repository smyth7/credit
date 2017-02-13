# Kaggle - Give Me Some Credit
# https://www.kaggle.com/c/GiveMeSomeCredit/data

# Author: Paul Smyth
# email: smyth7+kaggle@gmail.com

# # Required packages (Uncomment to install)
# install.packages("tidyverse")
# install.packages("ReporteRs")
# install.packages("stringr")

# Description -------------------------------------------------------------

# This scripts loads the dataset data_credit_modelling.Rda that was prepared in 02_Feature_Engineering.R

# Clear memory
rm(list = ls())

# Load libraries and functions -------------------------------------------------------------

source("Code/etl_functions.R")

library(tidyverse)
library(ReporteRs)
library(stringr)


# Load data ---------------------------------------------------------------

load(file="Data/data.credit.modelling.Rda")

