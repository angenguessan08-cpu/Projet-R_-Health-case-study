# =====================================================
#   PUBLIC HEALTH PROJECT - COMPLETE PIPELINE
#   Dirty Data → Clean Data → Business Insights
# =====================================================
# Author  : Ange N'GUESSAN - Junior Analyst
# Dataset : health_case_study (consultations, health centers)
# =====================================================


#  Installing packages

install.packages("tidyverse")
install.packages("janitor")
install.packages("naniar")
install.packages("readr")
install.packages("skimr")
install.packages("readxl")
install.packages("shiny")


# Importing libraries


library(tidyverse)   # dplyr, tidyr, ggplot2, stringr, lubridate...
library(janitor)     
library(naniar)      
library(readxl)   
library(readr)
library(shiny)


# Import the file
health_case_study <- read_csv("data/dirty_health_data_subsaharan.csv")

# Verify the import
print(health_case_study)


# Open the Excel-style viewing interface
View(health_case_study)
