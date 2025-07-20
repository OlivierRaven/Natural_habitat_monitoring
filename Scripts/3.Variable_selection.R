# 3. Variable selection
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
packages <- c("optimx","glmmLasso","brms","caret","performance","MuMIn","car","lmerTest","lme4","tidyverse", "dplyr", "ggplot2","readxl", "writexl","readr")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data set ----------------------------------------------------------
Monitoring_CPUE_data <- read_csv("Data_mod/Monitoring_CPUE_data.csv")

# Filter to ignor all sites that have been sampled double.
M_C_data <- Monitoring_CPUE_data %>%
  filter(Monitoring==0)

names(M_C_data)
summary(M_C_data)
str(M_C_data)
colSums(is.na(M_C_data))


# Test weighted cpue kōura (abundances)-----------------------------------------
# 1. Keep only relevant variables 
M_C_data_subset <- M_C_data %>%
  select(all_of(c(
    "Weighted_CPUE_Kōura", # response "Weighted_BCUE_Kōura", "Presence_Kōura"
    "LID",                 
    "Mean_depth_m","Elevation_m","TLI",
    "Slope_5m","Riparian_vegetation","Overhanging_trees","Erosion",
    "Wood_cover","Bedrock","Boulders","Cobble","Gravel","Sand","Organic_matter","Mud", "Turf","Presence_rocks",
    "Temperature","DO_mgl","DO_percent","Specific_conductivity","pH",
    "Emergent_Native","Emergent_Non_Native","Submerged_Native","Submerged_Non_Native",
    "Presence_Kōaro","Presence_Common_smelt","Presence_Trout","Presence_Morihana","Presence_Eel","Presence_Catfish","Presence_Mosquitofish"
  ))) %>%  na.omit()

# 2. Scale the data 
M_C_data_scaled <- M_C_data_subset
num_cols <- setdiff(names(M_C_data_scaled), c("Weighted_CPUE_Kōura", "LID"))
num_cols <- num_cols[sapply(M_C_data_scaled[num_cols], is.numeric)]
M_C_data_scaled[num_cols] <- lapply(M_C_data_scaled[num_cols], scale)

# Make sure LID is a factor
M_C_data_scaled$LID <- as.factor(M_C_data_scaled$LID)

# Get predictor names
predictors <- setdiff(names(M_C_data_scaled), c("Weighted_CPUE_Kōura", "LID"))

# Build formula
fixed_formula <- as.formula(
  paste("log1p(Weighted_CPUE_Kōura) ~", paste(predictors, collapse = " + ")))

# Fit LASSO mixed model
lasso_model <- glmmLasso(
  fix = fixed_formula,
  rnd = list(LID = ~1),
  data = M_C_data_scaled,
  lambda = 10,
  family = gaussian(link = "identity"),
  switch.NR = TRUE)

# Extract non-zero fixed effect coefficients
coef_fixed <- lasso_model$coefficients[-1]  # remove intercept
selected_vars <- names(coef_fixed)[coef_fixed != 0]

# Print selected variables
print(selected_vars)

# Build final formula
final_formula <- reformulate(
  selected_vars[selected_vars != "(Intercept)"], response = "log1p(Weighted_CPUE_Kōura)")

# Build the fixed effects part of the formula
fixed_part <- paste(selected_vars, collapse = " + ")

# Build the full formula with random effect
full_formula <- as.formula(
  paste("log1p(Weighted_CPUE_Kōura) ~", fixed_part, "+ (1 | LID)"))

# Fit final LMM
final_model <- lmer(full_formula, data = M_C_data_scaled)

summary(final_model)

check_collinearity(final_model)

# Test random effect
ranova(final_model)

# test the best models
final_model <- lmer(log1p(Weighted_CPUE_Kōura) ~ 
                         Boulders +  Elevation_m + Temperature  + TLI +
                         Slope_5m + Overhanging_trees + Riparian_vegetation +
                         Presence_Morihana+
                         (1 | LID), data = M_C_data_scaled)

best_final_model <- dredge(final_model)

# make model average of the top best models
averaged_model <- model.avg(best_final_model, subset = delta < 4) # average models within delta < 4
summary(averaged_model)

final_model_reduced <- lmer(log1p(Weighted_CPUE_Kōura) ~ 
                         Boulders +  #Elevation_m + Temperature  + TLI +
                         #Slope_5m + Overhanging_trees +   
                        # Presence_Morihana+
                         (1 | LID), data = M_C_data_scaled)

# 1. Refit models with ML (not REML) for valid comparison
final_model_ml <- update(final_model, REML = FALSE)
final_model_reduced_ml <- update(final_model_reduced, REML = FALSE)

# Compare
anova(final_model_ml, final_model_reduced_ml)

r.squaredGLMM(final_model_ml)
r.squaredGLMM(final_model_reduced_ml)

# Test random effect
ranova(final_model)

# Test the LM models
final_model_fixed <- lm(log1p(Weighted_CPUE_Kōura) ~ 
                          Boulders + Elevation_m + Temperature + TLI + Slope_5m +
                          Overhanging_trees + Riparian_vegetation + Presence_Morihana,
                        data = M_C_data_scaled)
summary(final_model_fixed)

best_fixed_model <- dredge(final_model_fixed)
averaged_model <- model.avg(best_fixed_model, subset = delta < 4) # average models within delta < 4
summary(averaged_model)

plot(final_model_fixed)

final_model_fixed_reduced <- lm(log1p(Weighted_CPUE_Kōura) ~ 
                           Boulders, #Elevation_m + Temperature + TLI + Slope_5m +
                          #Overhanging_trees + Riparian_vegetation +Presence_Morihana,
                        data = M_C_data_scaled)
summary(final_model_fixed_reduced)
plot(final_model_fixed_reduced)

anova(final_model_fixed, final_model_fixed_reduced)
r.squaredGLMM(final_model_fixed)
r.squaredGLMM(final_model_fixed_reduced)


model_summary <- summary(final_model_fixed_reduced)

# Extract R^2 and p-value
r2 <- round(model_summary$r.squared, 3)
pval <- signif(coef(model_summary)[2, 4], 3)

# Plot with annotation
ggplot(M_C_data_scaled, aes(Boulders, log1p(Weighted_CPUE_Kōura))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Boulders (scaled)",
    y = "log(Kōura CPUE + 1)") +
  annotate("text", x = 1, y = 1.5,
    label = paste0("R² = ", r2, "\nP = ", pval),
    size = 5, hjust = 0)+
  theme_bw()

# Fit a GLM with Gaussian family (same as LM)
glm_model <- glm(log1p(Weighted_CPUE_Kōura) ~ Boulders, 
                 data = M_C_data_scaled, 
                 family = gaussian())

summary(glm_model)

library(mgcv)
# Fit a GAM with a smooth term for Boulders
gam_model <- gam(log1p(Weighted_CPUE_Kōura) ~ s(Boulders, k = 3), 
                 data = M_C_data_scaled, 
                 method = "REML")

summary(gam_model)
plot(gam_model, residuals = TRUE, pch = 16)


# Step 2: Perform Spearman’s Rank Correlation test for each variable 
results_df <- purrr::map_dfr(variables, function(var) {
  test <- cor.test(M_C_data_subset[[var]], M_C_data_subset$Weighted_CPUE_Kōura, method = "spearman")
  tibble(Variable = var,Spearman_rho = test$estimate,p_value = test$p.value)})

# View sorted results
results_df %>% arrange(p_value)

# 2. Correlation matrix 
#numeric_vars <- M_C_data_subset[, sapply(M_C_data_subset, is.numeric)]
cor_results <- sapply(names(M_C_data_subset), function(var) {
  cor.test(M_C_data_subset[[var]], M_C_data_subset$Weighted_CPUE_Kōura, method = "spearman")$p.value})

adjusted_p <- p.adjust(cor_results, method = "fdr") # or method = "bonferroni"
significant_vars <- names(adjusted_p[adjusted_p < 0.1])
print(significant_vars)

cor_matrix_initial <- cor(M_C_data_subset[, sapply(M_C_data_subset, is.numeric)], use = "complete.obs", method = "spearman")
cor_melted_initial <- reshape2::melt(cor_matrix_initial)
ggplot(cor_melted_initial, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Spearman Correlation Heatmap", x = "Variables", y = "Variables")


hist(M_C_data_scaled$Weighted_CPUE_Kōura)
hist(log1p(M_C_data_scaled$Weighted_CPUE_Kōura))


# make Test mix effect models
# test all variables
glmm_Presence0 <- lmer(log1p(Weighted_CPUE_Kōura) ~ 
                          TLI + Elevation_m + Mean_depth_m + 
                          Slope_5m + Riparian_vegetation + Overhanging_trees + Wood_cover + Erosion +
                          Bedrock + Boulders + Cobble + Gravel + Sand + Organic_matter +  Mud + Presence_rocks + 
                          Temperature + DO_mgl + pH + Specific_conductivity + DO_percent +
                          Emergent_Native + Emergent_Non_Native + Submerged_Native + Submerged_Non_Native + 
                          Presence_Morihana + Presence_Eel + Presence_Catfish +
                          Presence_Kōaro + Presence_Common_smelt + Presence_Trout + Presence_Mosquitofish +
                          (1 | LID), data = M_C_data_scaled)

# remove variables with high Correlation
glmm_Presence1 <- lmer(log1p(Weighted_CPUE_Kōura) ~ 
                         #TLI + 
                         Elevation_m + Mean_depth_m + 
                         Slope_5m + Riparian_vegetation + Overhanging_trees + Wood_cover + Erosion +
                         Bedrock + Boulders + Cobble + Gravel + Sand + Organic_matter +  Mud + Presence_rocks + 
                         Temperature + DO_mgl + pH + Specific_conductivity + #DO_percent +
                         Emergent_Native + Emergent_Non_Native + Submerged_Native + Submerged_Non_Native + 
                         Presence_Morihana + Presence_Eel + Presence_Catfish +
                         Presence_Kōaro + Presence_Common_smelt + Presence_Trout + Presence_Mosquitofish +
                         (1 | LID), data = M_C_data_scaled)


# remove variables with high Correlation
glmm_Presence2 <- lmer(log1p(Weighted_CPUE_Kōura) ~ 
                          TLI + 
                          Elevation_m + 
                          Mean_depth_m + 
                          Slope_5m +  Overhanging_trees + 
                          Boulders + Mud + 
                          Temperature  + 
                          DO_percent + 
                          Presence_Morihana + Presence_Eel + 
                          (1 | LID), data = M_C_data_scaled)

# test the significant results < 0.1
glmm_Presence3 <- lmer(log1p(Weighted_CPUE_Kōura) ~ 
                         Elevation_m + TLI +
                         Slope_5m + Overhanging_trees + Boulders + Temperature  + Presence_Morihana +
                         (1 | LID), data = M_C_data_scaled)

# test the Spearman’s Rank variables 
glmm_Presence4 <- lmer(log1p(Weighted_CPUE_Kōura) ~ 
                         Boulders +  #Elevation_m + 
                         Temperature  + #TLI +
                         #Slope_5m + Overhanging_trees +   
                         Presence_Morihana+
                          (1 | LID), data = M_C_data_scaled)

# test the significant results < 0.1 redefined
glmm_Presence5 <- lmer(log1p(Weighted_CPUE_Kōura) ~ 
                         Boulders +  Elevation_m + Temperature  + TLI +
                         Slope_5m + Overhanging_trees + Riparian_vegetation +
                         Presence_Morihana+
                         (1 | LID), data = M_C_data_scaled)

summary(glmm_Presence0)
summary(glmm_Presence1)
summary(glmm_Presence2)
summary(glmm_Presence3)
summary(glmm_Presence4)
summary(glmm_Presence5)

# check collinearity
library(performance)
check_collinearity(glmm_Presence0)
check_collinearity(glmm_Presence1)
check_collinearity(glmm_Presence2)
check_collinearity(glmm_Presence3)
check_collinearity(glmm_Presence4)
check_collinearity(glmm_Presence5)

# find the most parsimonious model
library(MuMIn)
options(na.action = "na.fail") # required for dredge
best_model0 <- dredge(glmm_Presence0)
best_model1 <- dredge(glmm_Presence1)
best_model2 <- dredge(glmm_Presence2)
best_model3 <- dredge(glmm_Presence3)
best_model4 <- dredge(glmm_Presence4)
best_model5 <- dredge(glmm_Presence5)

# make model average of the top best models
averaged_model <- model.avg(best_model4, subset = delta < 4) # average models within delta < 4
summary(averaged_model)


# Compare
anova(glmm_Presence4, glmm_Presence5)
AIC(glmm_Presence4, glmm_Presence5)

# Fit model without random effect
glmm_no_RE <- lm(log1p(Weighted_CPUE_Kōura) ~ Boulders, data = M_C_data_scaled)
# Fit model with random effect
glmm_with_RE <- lmer(log1p(Weighted_CPUE_Kōura) ~ Boulders + (1 | LID), data = M_C_data_scaled)
# Compare models
anova(glmm_with_RE, glmm_no_RE)

# Bayesian version
library(brms)
brm_Presence3 <- brm(
  formula = log1p(Weighted_CPUE_Kōura) ~ 
    Boulders + #Presence_Morihana+ #Elevation_m + TLI +
    #Slope_5m + Overhanging_trees +  
    Temperature  + #Riparian_vegetation +
    (1 | LID),
  data = M_C_data_scaled,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.9999, max_treedepth = 15)
)


# Compute LOO for each model
loo1 <- brms::loo(brm_Presence1)
loo2 <- brms::loo(brm_Presence2)
loo3 <- brms::loo(brm_Presence3)

# Compare models
loo_compare(loo1, loo2, loo3)



# Load libraries
library(optimx)
library(projpred)

# Fit the full model
full_model <- brm(
  formula = log1p(Weighted_CPUE_Kōura) ~ 
    Boulders + Presence_Morihana + Elevation_m + TLI +
    Slope_5m + Overhanging_trees + Riparian_vegetation + Temperature + 
    (1 | LID),
  data = M_C_data_scaled,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.99999, max_treedepth = 15))

# Run variable selection
vs <- varsel(full_model, method = "forward") # or "backward"

# Inspect selection path
plot(vs)  # Shows how predictive accuracy changes as predictors are added

# Find the suggested model size
best_size <- suggest_size(vs)
print(best_size)

# Project onto the submodel with the suggested size
submodel <- project(vs, nv = best_size)

# Look at summary of reduced model
summary(submodel)

# Get the selected predictor terms
selected_terms <- predictor_terms(submodel)
print(selected_terms)

# Build the reduced formula manually
reduced_formula <- reformulate(
  termlabels = selected_terms,
  response = "log1p(Weighted_CPUE_Kōura)"
)

cat("Reduced model formula:\n")
print(reduced_formula)



brm_reduced <- brm(
  formula = reduced_formula,
  data = M_C_data_scaled,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.9999, max_treedepth = 15)
)

summary(brm_reduced)


# Compare full and reduced models
loo_full <- loo(full_model)
loo_reduced <- loo(brm_reduced)

# Model comparison
loo_compare(loo_full, loo_reduced)








# explanation of dropping mixed effect model:
library(lmerTest)
library(lmtest)
# Fixed-effects model
fixed_model <- lm(log1p(Weighted_CPUE_Kōura) ~ Elevation_m + Boulders + Cobble + Mud, data = M_C_data_scaled)

# Mixed-effects model
mixed_model <- lmer(log1p(Weighted_CPUE_Kōura) ~ Elevation_m + Boulders + Cobble + Mud + (1|LID), data = M_C_data_scaled)

# Check if random effect variance is zero
summary(mixed_model)
isSingular(mixed_model)

# Likelihood ratio test
lrtest(mixed_model, fixed_model)

ranova(mixed_model)


# 3.5 Filter for Multicollinearity Check (VIF) using mixed effect model
fixed_formula <- as.formula(paste("Weighted_CPUE_Kōura ~", paste(num_cols, collapse = " + "), "+ (1 | LID)"))

full_model <- lmer(fixed_formula, data = M_C_data_scaled)
summary(full_model)

vif_values <- vif(full_model)
print(vif_values)

# test several mixed models
model1 <- log1p(Weighted_CPUE_Kōura) ~ 
  LID +
  Elevation_m + 
  Slope_5m + 
  Boulders + 
  Cobble + 
  Gravel +
  Mud + 
  Emergent_Native + 
  Emergent_Non_Native +
  Presence_Morihana + 
  Temperature +
  (1 | LID)

model2 <- log1p(Weighted_CPUE_Kōura) ~ 
  Elevation_m + 
  Slope_5m + 
  Boulders + 
  Cobble + 
  Gravel +
  Mud + 
  Emergent_Native + 
  #Emergent_Non_Native +
  #Presence_Morihana +
  (1 | LID)

model3 <- log1p(Weighted_CPUE_Kōura) ~ 
  Elevation_m + 
  Slope_5m + 
  Boulders + 
  Cobble + 
  Gravel +
  Mud + 
  Emergent_Native + 
  #Emergent_Non_Native +
  #Presence_Morihana +
  Temperature +
  (1|LID)


lmm_CPUE1 <- lmer(model1, data = M_C_data_scaled)
lmm_CPUE2 <- lmer(model2, data = M_C_data_scaled)
lmm_CPUE3 <- lmer(model3, data = M_C_data_scaled)

summary(lmm_CPUE1)
summary(lmm_CPUE2)
summary(lmm_CPUE3)

# Compare models using likelihood ratio test
anova(lmm_CPUE1,lmm_CPUE2, lmm_CPUE3, test = "Chisq")


lm_CPUE <- lm(model1, data = M_C_data_scaled)
summary(lm_CPUE)

par(mfrow=c(2,2))
plot(lm_CPUE)


# 4. PCA
selected_vars
predictors <- M_C_data_scaled[, c("Elevation_m","TLI","Slope_5m", "Riparian_vegetation", 
                                  "Overhanging_trees","Boulders", "Temperature", "Presence_Morihana")]
# Perform PCA
pca_result <- prcomp(predictors, center = TRUE, scale. = TRUE)

# Summary of PCA
summary(pca_result)

# Make plots
library(factoextra)
fviz_eig(pca_result)                 # Variance explained
fviz_pca_biplot(pca_result)         # Biplot
fviz_pca_ind(pca_result)             # Samples on PCA plane
fviz_pca_var(pca_result)



#
# Test for weighted BCUE kōura (biomass) --------------------------------------
# 1. Keep only relevant variables 
M_C_data_subset2 <- M_C_data %>%
  select(all_of(c(
    "Weighted_BCUE_Kōura", # response "Weighted_CPUE_Kōura", "Presence_Kōura"
    "LID",                 
    "Mean_depth_m","Elevation_m","TLI",
    "Slope_5m","Riparian_vegetation","Overhanging_trees","Erosion",
    "Wood_cover","Bedrock","Boulders","Cobble","Gravel","Sand","Organic_matter","Mud","Presence_rocks",
    "Temperature","DO_mgl","DO_percent","Conductivity","Specific_conductivity","pH",
    "Emergent_Native","Emergent_Non_Native","Submerged_Native","Submerged_Non_Native",
    "Presence_Kōaro","Presence_Common_smelt","Presence_Trout","Presence_Morihana","Presence_Eel","Presence_Catfish","Presence_Mosquitofish"
  ))) %>%  na.omit()

variables2<-c("Weighted_BCUE_Kōura","LID",
             "TLI","Elevation_m","Mean_depth_m",
             "Slope_5m","Riparian_vegetation","Overhanging_trees","Wood_cover","Erosion",
             "Bedrock","Boulders","Cobble","Gravel","Sand","Organic_matter","Mud","Presence_rocks",
             "Temperature","DO_mgl", "Specific_conductivity","pH","DO_percent",
             "Emergent_Native","Emergent_Non_Native","Submerged_Native","Submerged_Non_Native",
             "Presence_Kōaro","Presence_Common_smelt","Presence_Trout","Presence_Morihana","Presence_Eel","Presence_Catfish","Presence_Mosquitofish")


# 2. Scale the data 
M_C_data_scaled2 <- M_C_data_subset2
num_cols <- setdiff(names(M_C_data_scaled2), c("Weighted_BCUE_Kōura", "LID"))
num_cols <- num_cols[sapply(M_C_data_scaled2[num_cols], is.numeric)]
M_C_data_scaled2[num_cols] <- lapply(M_C_data_scaled2[num_cols], scale)

# Make sure LID is a factor
M_C_data_scaled2$LID <- as.factor(M_C_data_scaled2$LID)

# Get predictor names
predictors <- setdiff(names(M_C_data_scaled2), c("Weighted_BCUE_Kōura", "LID"))

# Build formula
fixed_formula <- as.formula(
  paste("log1p(Weighted_BCUE_Kōura) ~", paste(predictors, collapse = " + ")))

# Fit LASSO mixed model
lasso_model <- glmmLasso(
  fix = fixed_formula,
  rnd = list(LID = ~1),
  data = M_C_data_scaled2,
  lambda = 10,
  family = gaussian(link = "identity"),
  switch.NR = TRUE)

# Extract non-zero fixed effect coefficients
coef_fixed <- lasso_model$coefficients[-1]  # remove intercept
selected_vars <- names(coef_fixed)[coef_fixed != 0]

# Print selected variables
print(selected_vars)

# Build final formula
final_formula <- reformulate(
  selected_vars[selected_vars != "(Intercept)"], response = "log1p(Weighted_BCUE_Kōura)")

# Build the fixed effects part of the formula
fixed_part <- paste(selected_vars, collapse = " + ")

# Build the full formula with random effect
full_formula <- as.formula(
  paste("log1p(Weighted_BCUE_Kōura) ~", fixed_part, "+ (1 | LID)"))

# Fit final LMM
final_model <- lmer(full_formula, data = M_C_data_scaled2)

summary(final_model)

check_collinearity(final_model)




# test the best models
final_model <- lmer(log1p(Weighted_BCUE_Kōura) ~ 
                      Elevation_m +  Overhanging_trees + Boulders + Cobble +
                      Mud + Presence_Kōaro + Presence_Morihana +
                      Presence_Eel + Presence_Mosquitofish +
                      (1 | LID), data = M_C_data_scaled2)

best_final_model <- dredge(final_model)

# make model average of the top best models
averaged_model <- model.avg(best_final_model, subset = delta < 4) # average models within delta < 4
summary(averaged_model)

final_model_reduced <- lmer(log1p(Weighted_BCUE_Kōura) ~ 
                              Overhanging_trees + Presence_Morihana + Presence_Eel + 
                              (1 | LID), data = M_C_data_scaled2)
summary(final_model_reduced)

# 1. Refit models with ML (not REML) for valid comparison
final_model_ml <- update(final_model, REML = FALSE)
final_model_reduced_ml <- update(final_model_reduced, REML = FALSE)

# Compare
anova(final_model_ml, final_model_reduced_ml)


#
# Test for kōura occupancy (presence/absence) ---------------------------------
# 1. Keep only relevant variables 
M_C_data_subset3 <- M_C_data %>%
  select(all_of(c("Presence_Kōura", "LID",
    "TLI","Elevation_m","Mean_depth_m",
    "Slope_5m","Riparian_vegetation","Overhanging_trees","Wood_cover","Erosion",
    "Bedrock","Boulders","Cobble","Gravel","Sand","Organic_matter","Mud","Presence_rocks",
    "Temperature","DO_mgl", "Specific_conductivity","pH","DO_percent",
    "Emergent_Native","Emergent_Non_Native","Submerged_Native","Submerged_Non_Native",
    "Presence_Kōaro","Presence_Common_smelt","Presence_Trout","Presence_Morihana","Presence_Eel","Presence_Catfish","Presence_Mosquitofish"))) %>%  na.omit()


# 2. Scale the data 
M_C_data_scaled3 <- M_C_data_subset3
num_cols <- setdiff(names(M_C_data_scaled3), c("Presence_Kōura", "LID"))
num_cols <- num_cols[sapply(M_C_data_scaled3[num_cols], is.numeric)]
M_C_data_scaled3[num_cols] <- lapply(M_C_data_scaled3[num_cols], scale)

# Make sure LID is a factor
M_C_data_scaled3$LID <- as.factor(M_C_data_scaled3$LID)

# Get predictor names
predictors <- setdiff(names(M_C_data_scaled3), c("Presence_Kōura", "LID"))

# Build formula
fixed_formula <- as.formula(
  paste("Presence_Kōura ~", paste(predictors, collapse = " + ")))

# Fit LASSO mixed model
lasso_model <- glmmLasso(
  fix = fixed_formula,
  rnd = list(LID = ~1),
  data = M_C_data_scaled3,
  lambda = 10,
  family = binomial(link = "logit"),
  switch.NR = TRUE)

# Extract non-zero fixed effect coefficients
coef_fixed <- lasso_model$coefficients[-1]  # remove intercept
selected_vars <- names(coef_fixed)[coef_fixed != 0]

# Print selected variables
print(selected_vars)


# Build final formula
final_formula <- reformulate(
  selected_vars[selected_vars != "(Intercept)"], response = "Presence_Kōura")

# Build the fixed effects part of the formula
fixed_part <- paste(selected_vars, collapse = " + ")

# Build the full formula with random effect
full_formula <- as.formula(
  paste("Presence_Kōura ~", fixed_part, "+ (1 | LID)"))

# Fit final LMM
final_model <- glmer(full_formula, data = M_C_data_scaled3, family = binomial(link = "logit"))

summary(final_model)

check_collinearity(final_model)










variables3 <-c("Presence_Kōura","LID",
               "TLI","Elevation_m","Mean_depth_m",
               "Slope_5m","Riparian_vegetation","Overhanging_trees","Wood_cover","Erosion",
               "Bedrock","Boulders","Cobble","Gravel","Sand","Organic_matter","Mud","Presence_rocks",
               "Temperature","DO_mgl", "Specific_conductivity","pH","DO_percent",
               "Emergent_Native","Emergent_Non_Native","Submerged_Native","Submerged_Non_Native",
               "Presence_Kōaro","Presence_Common_smelt","Presence_Trout","Presence_Morihana","Presence_Eel","Presence_Catfish","Presence_Mosquitofish")

# Step 2: Perform Mann–Whitney U test for each variable 
results <- lapply(variables3[-1], function(var) {
  wilcox.test(M_C_data_subset3[[var]] ~ M_C_data_subset3$Presence_Kōura   , data = M_C_data_subset3)})

# Format results into a summary
results_summary <- data.frame(
  Variable = variables3[-1],
  p_value = sapply(results, function(x) x$p.value),
  W_statistic = sapply(results, function(x) x$statistic))

# Display significant results (e.g., p < 0.05)
Wilcoxon_results <- results_summary %>%
  filter(p_value < 0.05)

print(Wilcoxon_results)


# scale variables
M_C_data_scaled3 <- M_C_data_subset3
num_cols <- setdiff(names(M_C_data_scaled3), c("Presence_Kōura", "LID"))
num_cols <- num_cols[sapply(M_C_data_scaled3[num_cols], is.numeric)]
M_C_data_scaled3[num_cols] <- lapply(M_C_data_scaled3[num_cols], scale)


# Check for collinearity
num_cols <- names(M_C_data_scaled3)[sapply(M_C_data_scaled3, is.numeric)]
cor_matrix <- cor(M_C_data_scaled3[num_cols], use = "pairwise.complete.obs")
cor_melted <- reshape2::melt(cor_matrix)
high_corr <- findCorrelation(cor_matrix, cutoff = 0.7, names = TRUE)  # From caret package

ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Spearman Correlation Heatmap", x = "Variables", y = "Variables")


# make Test mix effect models
# test all variables
glmm_Presence0 <- glmer(Presence_Kōura ~ 
  TLI + Elevation_m + Mean_depth_m + 
  Slope_5m + Riparian_vegetation + Overhanging_trees + Wood_cover + Erosion +
  Bedrock + Boulders + Cobble + Gravel + Sand + Organic_matter +  Mud + Presence_rocks + 
  Temperature + DO_mgl + pH + Specific_conductivity + DO_percent + Conductivity +
  Emergent_Native + Emergent_Non_Native + Submerged_Native + Submerged_Non_Native + 
  Presence_Morihana + Presence_Eel + Presence_Catfish +
  Presence_Kōaro + Presence_Common_smelt + Presence_Trout + Presence_Mosquitofish +
  (1 | LID), data = M_C_data_scaled3, family = binomial(link = "logit"))

# test the Wilcoxon_results < .1
glmm_Presence1 <- glmer(Presence_Kōura ~ 
  TLI + Elevation_m + Mean_depth_m + 
  Slope_5m +  Overhanging_trees + #Riparian_vegetation +Wood_cover + Erosion +
  Boulders + Mud + #Bedrock +Cobble + Gravel + Sand + Organic_matter + Presence_rocks + 
  Temperature + Conductivity + DO_percent + #DO_mgl + pH + Specific_conductivity +  
  #Emergent_Native + Emergent_Non_Native + Submerged_Native + Submerged_Non_Native + 
  Presence_Morihana + Presence_Eel + #Presence_Catfish + Presence_Kōaro + Presence_Common_smelt + Presence_Trout + Presence_Mosquitofish +
  (1 | LID), data = M_C_data_scaled3, family = binomial(link = "logit"))


# remove variables with high Correlation
glmm_Presence2 <- glmer(Presence_Kōura ~ 
  #TLI + Elevation_m + 
  Mean_depth_m + 
  Slope_5m +  Overhanging_trees + 
  Boulders + Mud + 
  Temperature + #Conductivity + 
  DO_percent + 
  Presence_Morihana + Presence_Eel + 
  (1 | LID), data = M_C_data_scaled3, family = binomial(link = "logit"))

# remove diffrent variables than in model2
glmm_Presence3 <- glmer(Presence_Kōura ~ 
  #TLI + 
  #Elevation_m + 
  #Mean_depth_m + 
  Slope_5m +  Overhanging_trees + 
  Boulders + #Mud + 
  Temperature + 
  #Conductivity + DO_percent + 
  Presence_Morihana + Presence_Eel + 
  (1 | LID), data = M_C_data_scaled3, family = binomial(link = "logit"))

# test the Wilcoxon results < .5
glmm_Presence4 <- glmer(Presence_Kōura ~ 
  #TLI + Elevation_m + 
  Mean_depth_m + 
  Slope_5m +  Boulders +  
  #Temperature +  
  Presence_Morihana + 
  (1 | LID), data = M_C_data_scaled3, family = binomial(link = "logit"))

summary(glmm_Presence0)
summary(glmm_Presence1)
summary(glmm_Presence2)
summary(glmm_Presence3)
summary(glmm_Presence4)
summary(glmm_Presence5)

# check collinearity
library(performance)
check_collinearity(glmm_Presence0)
check_collinearity(glmm_Presence1)
check_collinearity(glmm_Presence2)
check_collinearity(glmm_Presence3)
check_collinearity(glmm_Presence4)
check_collinearity(glmm_Presence5)

# find the most parsimonious model
library(MuMIn)
options(na.action = "na.fail") # required for dredge
best_model0 <- dredge(glmm_Presence0)
best_model1 <- dredge(glmm_Presence1)
best_model2 <- dredge(glmm_Presence2)
best_model3 <- dredge(glmm_Presence3)
best_model4 <- dredge(glmm_Presence4)
best_model5 <- dredge(glmm_Presence5)

# make model average of the top best models
averaged_model <- model.avg(best_model4, subset = delta < 4) # average models within delta < 4
summary(averaged_model)


# Compare models using likelihood ratio test
anova(glmm_Presence0, glmm_Presence1, glmm_Presence2, glmm_Presence3, test = "Chisq")


# Bayesian version
library(brms)
brm_Presence1 <- brm(
  formula = Presence_Kōura| trials(1) ~ 
    TLI + Elevation_m + 
    Mean_depth_m + 
    Slope_5m +  Boulders +  
    Temperature +  
    Presence_Morihana + 
    (1 | LID),
  data = M_C_data_scaled3,
  family = binomial(link = "logit"),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.9999, max_treedepth = 15)
)

plot(brm_Presence1)




for (var in predictors) {
  formula <- as.formula(paste("Presence_Kōura ~", var, "+ (1 | LID)"))
  model <- glmer(formula, data = M_C_data_scaled3, family = binomial)
  cat("\n", var, "\n")
  print(summary(model))
}

(Presence_Kōura ~ TLI + 
  Elevation_m + 
  Riparian_vegetation + 
  Overhanging_trees +  
  Boulders + 
  Temperature + 
  Submerged_Non_Native + 
  Presence_Morihana + 
  Presence_Eel + 
  Slope_5m + (1 | LID),
data = M_C_data_scaled3, family = binomial)






glm_reduced <- glmer(Presence_Kōura ~ #TLI + 
                     # Elevation_m + 
                      Mean_depth_m +
                      #Riparian_vegetation + 
                      #Overhanging_trees +  
                      Boulders + 
                      #Temperature + 
                      #Submerged_Non_Native + 
                      Presence_Morihana + 
                      Presence_Eel + 
                      Slope_5m + (1 | LID),
                    data = M_C_data_scaled3, family = binomial)
summary(glm_reduced)
vif(glm_reduced)


glm_fixed <- glm(Presence_Kōura ~ TLI + Elevation_m + Boulders + 
                   Presence_Morihana + Presence_Eel + Slope_5m,
                 data = M_C_data_scaled3, family = binomial)
anova(glm_fixed, glm_reduced, test = "Chisq")





glm_step <- step(glm_reduced, direction = "both")
summary(glm_step)
vif(glm_step)

# Make diffrent models
modelall <- as.formula(paste("Presence_Kōura ~", paste(num_cols, collapse = " + "), "+ (1 | LID)"))

full_model3 <- glmer(Presence_Kōura ~ 
                     Slope_5m + TLI + #LID + 
                     Elevation_m + Mean_depth_m + 
                     Riparian_vegetation + Overhanging_trees + Wood_cover + Erosion +
                     Bedrock + Boulders + Cobble + Gravel + Sand + Organic_matter +  Mud + #Presence_rocks + 
                     Temperature + DO_mgl + pH + Specific_conductivity + # DO_percent + Conductivity +
                     Emergent_Native + Emergent_Non_Native + Submerged_Native + Submerged_Non_Native + 
                     Presence_Morihana + Presence_Eel + Presence_Catfish +
                     #Presence_Kōaro + Presence_Common_smelt + Presence_Trout + Presence_Mosquitofish,
                     (1 | LID),
                   data = M_C_data_scaled3, family = binomial(link = "logit"))

options(na.action = "na.fail") # required by dredge
model_set <- dredge(full_model3)

# View the top models
head(model_set)

# Get the best model
best_model <- get.models(model_set, subset = 1)[[1]]
summary(best_model)

# 
model0 <- Presence_Kōura ~ 
  TLI + Elevation_m + Mean_depth_m + 
  Slope_5m + Riparian_vegetation + Overhanging_trees + Wood_cover + Erosion +
  Bedrock + Boulders + Cobble + Gravel + Sand + Organic_matter +  Mud + #Presence_rocks + 
  Temperature + DO_mgl + pH + Specific_conductivity + # DO_percent + Conductivity +
  Emergent_Native + Emergent_Non_Native + Submerged_Native + Submerged_Non_Native + 
  Presence_Morihana + Presence_Eel + Presence_Catfish +
  #Presence_Kōaro + Presence_Common_smelt + Presence_Trout + Presence_Mosquitofish,
  (1 | LID)

model1 <- Presence_Kōura ~
  TLI + Elevation_m + Mean_depth_m + 
  Slope_5m + Riparian_vegetation + Overhanging_trees + Wood_cover + Erosion +
  #Bedrock + Boulders + Cobble + Gravel + Sand + Organic_matter +  Mud + 
  #Temperature + DO_mgl + pH + Specific_conductivity + 
  #Emergent_Native + Emergent_Non_Native + Submerged_Native + Submerged_Non_Native + 
  Presence_Morihana + Presence_Eel + Presence_Catfish +
  (1 | LID)

model2 <- Presence_Kōura ~  
  Slope_5m + Overhanging_trees + 
  Temperature + 
  Presence_Morihana + Presence_Eel + 
  (1 | LID)

model3 <- Presence_Kōura ~  
  Slope_5m + Overhanging_trees + 
  Temperature + Boulders +
  Presence_Morihana + Presence_Eel + 
  (1 | LID)

glmm_Presence0 <- glmer(model0, data = M_C_data_scaled3, family = binomial(link = "logit"))
glmm_Presence1 <- glmer(model1, data = M_C_data_scaled3, family = binomial(link = "logit"))
glmm_Presence2 <- glmer(model2, data = M_C_data_scaled3, family = binomial(link = "logit"))
glmm_Presence3 <- glmer(model3, data = M_C_data_scaled3, family = binomial(link = "logit"))

summary(glmm_Presence0)
summary(glmm_Presence1)
summary(glmm_Presence2)
summary(glmm_Presence3)

# Compare models using likelihood ratio test
anova(glmm_Presence0, glmm_Presence1, glmm_Presence2, glmm_Presence3, test = "Chisq")

fixed_formula1 <- Presence_Kōura ~  
  TLI + #Elevation_m + Mean_depth_m + 
  Slope_5m + Overhanging_trees + #Wood_cover + 
  #Bedrock + Boulders + Cobble + Gravel + Sand + Organic_matter + Mud + 
  #Temperature + #DO_mgl + Specific_conductivity + pH + 
  #Emergent_Native + Emergent_Non_Native + Submerged_Native + Submerged_Non_Native  + 
  Presence_Morihana + Presence_Eel + #Presence_Catfish  + 
  (1 | LID)
