# Habitat + Reef data
# Explanation of this script ------------------------------------------------

# combination fo the habitat monitoring and the reef monitoring
# 
#
#


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD/Data/3. Natural habitat monitoring")

# Define the list of packages
packages <- c("sf", "mgcv","tidyverse", "dplyr", "ggplot2","readxl", "writexl","readr")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data sets ---------------------------------------------------------
setwd("~/PhD/Data/3. Natural habitat monitoring")
habitat_Monitoring_CPUE_data <- read_csv("Data_mod/Monitoring_CPUE_data.csv")


Reef_Monitoring_CPUE_data <- read_csv("Data_mod/Monitoring_Reef_data.csv")
