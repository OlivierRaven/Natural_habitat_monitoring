# 2. Analise the data
# Explanation of this script ------------------------------------------------

# 
# 
#
#


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD/Data/3. Natural habitat monitoring")

# Define the list of packages
packages <- c("tidyverse", "dplyr", "ggplot2","readxl", "writexl","readr")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data sets ---------------------------------------------------------
#Monitoring_CPUE_data <- read_excel("Data_mod/Monitoring_CPUE_data.xlsx")

Monitoring_CPUE_data <- read_csv("Data_mod/Monitoring_CPUE_data.csv")

# Start analisis ---------------------------------------------------------------
# View the structure and summary of the dataset
names(Monitoring_CPUE_data)
str(Monitoring_CPUE_data)
summary(Monitoring_CPUE_data)

# Remove rows where Date_Time is NA
M_C_data <- Monitoring_CPUE_data %>% filter(!is.na(Date_Time))

# Ensure Date_Time is in POSIXct format
M_C_data$Date_Time <- ymd_hms(M_C_data$Date_Time)

# Convert Date_Time to numeric (seconds since 1970-01-01)
M_C_data$Date_Time_Numeric <- as.numeric(M_C_data$Date_Time)

# Optional: Check the first few rows to confirm conversion
head(M_C_data[, c("Date_Time", "Date_Time_Numeric")])


### Calculate Substrate Index (s) 
# Define weights for each substrate (this can be adjusted)
substrate_weights <- c(Bedrock= 0.08, Boulders= 0.09, Cobble= 0.10, Gravel = 0.06, Sand = 0.05, Mud = 0.02, Organic_matter = 0.04)

substrate_weights <- c(
  Bedrock = 0.07,  # Neutral influence
  Boulders = 0.15, # Strong positive influence
  Cobble = 0.10,   # Presumed moderate positive
  Gravel = 0.08,   # Weak negative influence, reduced slightly
  Sand = 0.12,     # Positive influence, increased
  Mud = 0.03,      # Negative influence, reduced weight
  Organic_matter = 0.05  # Negative influence, reduced weight
)


substrate_weights_J <- c(Bedrock= 0.08, Boulders= 0.07, Cobble= 0.06, Gravel = 0.05, Sand = 0.035, Mud = 0.035)

substrate_values <- M_C_data[c("Bedrock","Boulders","Cobble", "Gravel", "Sand", "Mud", "Organic_matter")]

# Calculate the substrate index 
M_C_data$Substrate_index <- rowSums(substrate_values * substrate_weights, na.rm = TRUE)

names(M_C_data)
summary(M_C_data)

# List of variables to test
variables <- c("CPUE_Kōura", 
               "Slope","Distance_deep", "Riparian_vegetation","Overhanging_trees",
               "Substrate_index", "Boulders","Gravel","Sand","Mud","Organic_matter","Turf",
               "Temperature","DO_mgl","DO_percent","Connectivity","pH",
               "Emergent_weed","Submerged_weed", "Wood_cover",
               "CPUE_Bullies", "CPUE_Morihana","CPUE_Eel","CPUE_Common_smelt","CPUE_Kōaro","CPUE_Trout"
               )

variables <- c("Weighted_CPUE_Kōura", "Presence_Kōura",
               "Date_Time_Numeric","Mean_depth(m)", "Distance_deep_20m", "Elevation(m)", "Slope", 
               "Temperature", "DO_mgl", "DO_percent", "pH", "TLI", "Water_clarity",
               "Boulders", "Cobble", "Gravel", "Sand", "Mud", "Organic_matter", "Substrate_index", 
               "Riparian_vegetation", "Overhanging_trees", 
               "Emergent_weed", "Submerged_weed", "Wood_cover",
               "Presence_Kōaro", "Presence_Bullies","Presence_Common_smelt", "Presence_Trout", "Presence_Morihana", "Presence_Eel")

# Presence/absence -------------------------------------------------------------

# Step 2: Perform Mann–Whitney U test for each variable
results <- lapply(variables[-1], function(var) {
  wilcox.test(M_C_data[[var]] ~ M_C_data$Presence_Kōura   , data = M_C_data)
})

# Step 3: Format results into a summary
results_summary <- data.frame(
  Variable = variables[-1],
  p_value = sapply(results, function(x) x$p.value),
  W_statistic = sapply(results, function(x) x$statistic)
)

# Step 4: Display significant results (e.g., p < 0.05)
significant_results <- results_summary %>%
  filter(p_value < 0.05)

print(significant_results)

# Check normality --------------------------------------------------------------
# Loop through each variable
for (var in variables) {
  # Check if the variable exists in the dataset and is numeric
  if (var %in% colnames(M_C_data) && is.numeric(M_C_data[[var]])) {
    
    # Create histogram
    hist(M_C_data[[var]], main = paste("Histogram of", var), xlab = var, breaks = 20)
    
    # Q-Q plot
    qqnorm(M_C_data[[var]], main = paste("Q-Q plot of", var))
    qqline(M_C_data[[var]], col = "red")
    
    # Perform Shapiro-Wilk test for normality
    shapiro_result <- shapiro.test(M_C_data[[var]])
    
    # Print the result of the Shapiro-Wilk test
    print(paste("Shapiro-Wilk test for", var, ": p-value =", shapiro_result$p.value))
    } else {print(paste("Variable", var, "is not numeric or does not exist in the dataset"))}}



# Scatter plot -----------------------------------------------------------------
# Create the pair plot
pairs(M_C_data[, variables])

# Correlation matrix -----------------------------------------------------------
library(reshape2)
cor_matrix <- cor(M_C_data[, variables], method = "pearson") #"pearson", "spearman", "kendall"

# Melt the correlation matrix for ggplot2
cor_matrix_melted <- melt(cor_matrix)

# Create a heatmap
ggplot(cor_matrix_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1)) 


# Set threshold for strong correlations
threshold <- 0.5

# Filter for correlations above the threshold (absolute value)
strong_correlations <- cor_matrix_melted %>%
  filter(abs(value) > threshold, Var1 != Var2) %>%
  arrange(desc(abs(value)))

# GAM (Generalized Additive Model) -----------------------------------------------
library(mgcv)

summary(M_C_data)
# Fit a GAM model
gam_model <- gam(Weighted_CPUE_Kōura ~ variables,
                 family = poisson(link = "log"), data = M_C_data)


gam_model <- gam(Weighted_CPUE_Kōura ~ s(Distance_deep_20m, k = 5) + s(Riparian_vegetation, k = 5) + 
                   s(Overhanging_trees, k = 5) + Boulders + Gravel + Sand + 
                   Mud + Organic_matter + Turf + Emergent_weed + 
                   Submerged_weed + Wood_cover, 
                 family = poisson, data = M_C_data)

# Summarize the GAM model
summary(gam_model)

plot(gam_model, pages = 1, main = "GAM Smooth Terms for CPUE_Kōura")



# GLM --------------------------------------------------------------------------
glm_model <- glm(CPUE_Kōura ~ Distance_deep + Riparian_vegetation + Overhanging_trees + 
                   Bedrock + Boulders + Cobble + Gravel + Sand + Mud + Organic_matter + 
                   Temperature + DO_mgl + DO_percent + Connectivity + pH + 
                   Emergent_weed + Submerged_weed + Wood_cover+ CPUE_Bullies +CPUE_Morihana+ CPUE_Eel+CPUE_Common_smelt+CPUE_Kōaro+CPUE_Trout,
                 family = poisson(link = "log"), 
                 data = M_C_data)

summary(glm_model)

library(MASS)
glm_stepwise <- stepAIC(glm_model, direction = "both")


# PCA (Principal Component Analysis) -------------------------------------------
predictor_data <- M_C_data[, variables]
predictor_data_scaled <- scale(predictor_data)
pca_result <- prcomp(predictor_data_scaled, scale. = TRUE)
summary(pca_result)
biplot(pca_result)


# NMDS (Non-Metric Multidimensional Scaling) ---------------------------------------------------------------
library(vegan)

# Prepare data for NMDS: Exclude the response variable CPUE_Kōura if it’s not needed
predictor_data_clean <- predictor_data[complete.cases(predictor_data), ]

# Perform NMDS using Bray-Curtis dissimilarity (use other measures if suitable)
nmds_result <- metaMDS(predictor_data_clean, distance = "bray", k = 2, trymax = 100)

# Summarize and plot NMDS results
print(nmds_result)
plot(nmds_result, type = "t", main = "NMDS Plot")




# CCA (Canonical Correspondence Analysis) ---------------------------------------------------------------
# Separate response and predictor data (assuming CPUE data as response)
response_data <- Data1[, c("CPUE_Kōura", "CPUE_Bullies", "CPUE_Morihana", 
                               "CPUE_Eel", "CPUE_Common_smelt", "CPUE_Kōaro", "CPUE_Trout")]
environmental_data <- Data1[, c("Distance_deep", "Riparian_vegetation","Overhanging_trees",
                                "Boulders","Gravel","Sand","Mud","Organic_matter",
                                "Temperature","DO_mgl","DO_percent","Connectivity","pH",
                                "Emergent_weed","Submerged_weed", "Wood_cover")]

# Ensure no missing values in response or predictor data
response_data <- response_data[complete.cases(response_data), ]
environmental_data <- environmental_data[complete.cases(environmental_data), ]

# Perform CCA
cca_result <- cca(response_data ~ ., data = environmental_data)

# Summarize and plot CCA results
summary(cca_result)
plot(cca_result, main = "Canonical Correspondence Analysis (CCA)")




# RDA (Redundancy Analysis) ---------------------------------------------------------------
rda_result <- rda(response_data ~ ., data = environmental_data)

# Summarize and plot RDA results
summary(rda_result)
plot(rda_result, main = "Redundancy Analysis (RDA)")


# db-RDA (Constrained Ordination) ---------------------------------------------------------------
species_data <- Data1[, c("CPUE_Kōura", "CPUE_Bullies", "CPUE_Morihana", "CPUE_Eel", "CPUE_Common_smelt", "CPUE_Kōaro", "CPUE_Trout")]
environmental_data <- Data1[, c("Distance_deep", "Riparian_vegetation", "Overhanging_trees", 
                                "Bedrock", "Boulders", "Cobble", "Gravel", "Sand", "Mud", 
                                "Organic_matter", "Temperature", "DO_mgl", "DO_percent", 
                                "Connectivity", "pH", "Emergent_weed", "Submerged_weed", 
                                "Wood_cover")]

# Compute the dissimilarity matrix on the response variables (species data)
species_dist <- vegdist(species_data, method = "bray")

# Perform db-RDA using capscale (distance-based constrained ordination)
db_rda_model <- capscale(species_dist ~ ., data = environmental_data)

# Summarize and inspect the model
summary(db_rda_model)

# Visualize the db-RDA results
plot(db_rda_model, main = "db-RDA of Species Data Constrained by Environmental Predictors")




