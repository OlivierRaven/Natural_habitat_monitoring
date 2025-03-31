# 3. Analyse GAM
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
packages <- c("mgcv","tidyverse", "dplyr", "ggplot2","readxl", "writexl","readr")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data sets ---------------------------------------------------------



# GAM (Generalized Additive Model) -----------------------------------------------
# Fit the GAM model with adjusted degrees of freedom (k)
gam_model <- gam(Weighted_CPUE_Kōura ~ s(Weighted_CPUE_Kōura, k = 3) + s(Overhanging_trees, k = 3) + 
                   s(Weighted_CPUE_Morihana, k = 4),
                 family = poisson(link = "log"),
                 data = M_C_data)

gam_model <- gam(Weighted_CPUE_Kōura ~ Selected_variables, 
                 family = poisson(link = "log"), 
                 data = M_C_data)

plot(gam_model, pages = 1)

# Summarize the model
summary(gam_model)

plot(gam_model, pages = 1)
