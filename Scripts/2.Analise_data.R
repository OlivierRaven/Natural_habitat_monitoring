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
packages <- c("patchwork", "sf", "mgcv","tidyverse", "dplyr", "ggplot2","readxl", "writexl","readr")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data sets ---------------------------------------------------------
Monitoring_CPUE_data <- read_csv("Data_mod/Monitoring_CPUE_data.csv")

M_C_data <- Monitoring_CPUE_data %>%
  filter(Monitoring==0)

names(M_C_data)
str(M_C_data)
summary(M_C_data)

# Make some plots to explore the data ------------------------------------------
# Define the lake order
lake_order <- c("Rotorua", "Rotoiti", "Rotoehu", "Rotomā", "Ōkāreka")

M_C_data$Lake <- factor(M_C_data$Lake, levels = lake_order)

# Define a custom color palette
sediment_colors <- c("Bedrock" = "black","Boulders" = "gray25","Cobble" = "gray55","Gravel" = "gray80","Sand" = "gold","Mud" = "saddlebrown","Organic_matter" = "darkgreen", "Turf" = "lightgreen")
weed_colors <- c("Emergent_Native" = "darkgreen","Emergent_Non_Native" = "limegreen","Submerged_Native" = "skyblue","Submerged_Non_Native" = "royalblue4","Wood_cover" = "saddlebrown")

# Physical parameters ----------------------------------------------------------
#Create the 'presence_rocks' variable
M_C_data <- M_C_data %>%
  mutate(Presence_rocks = if_else(Cobble > 1 | Boulders > 1 | Bedrock > 1, 1, 0))

# Remake Physical parameters
sediment_data <- M_C_data %>%
  select(Site_ID_,DHT,Habitat_Type , Lake, Bedrock, Boulders, Cobble, Gravel, Sand, Mud, Organic_matter, Turf) %>%
  pivot_longer(cols = c(Bedrock, Boulders, Cobble, Gravel, Sand, Mud, Organic_matter, Turf), 
               names_to = "Sediment_Type", values_to = "Percentage") %>%
  mutate(Lake = factor(Lake, levels = lake_order))

weed_data <- M_C_data %>%
  select(Site_ID_,DHT,Habitat_Type , Lake, Emergent_Native,Emergent_Non_Native, Submerged_Native, Submerged_Non_Native,Wood_cover) %>%
  pivot_longer(cols = c(Emergent_Native,Emergent_Non_Native, Submerged_Native, Submerged_Non_Native,Wood_cover), 
               names_to = "Weeds", values_to = "Percentage") %>%
  mutate(Lake = factor(Lake, levels = lake_order))

# Plot Physical parameters
Sediment_plot <- ggplot(sediment_data, aes(factor(Site_ID_), Percentage, fill = Sediment_Type)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.2) +
  scale_fill_manual(values = sediment_colors) +
  facet_grid( ~ Lake, scales = "free_x")+
  labs(x = "Site",y = "Sediment Cover (%)",fill = "Sediment Type") +
  theme_bw()

Weed_plot <- ggplot(weed_data, aes(factor(Site_ID_),Percentage, fill = Weeds)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.2) +
  scale_fill_manual(values = weed_colors) +
  facet_grid( ~ Lake, scales = "free_x")+
  labs(x = "Site",y = "Vegetation Cover (%)",fill = "Weed Type") +
  theme_bw()

Physical_plot <- Sediment_plot/Weed_plot
Physical_plot

ggsave("Figures/Physical_plot.png", Physical_plot, width = 12, height = 6, dpi = 300)


# Chemical parameters ----------------------------------------------------------
# Remake Chemical parameters
Chemical_data <- M_C_data %>%
  select(Site_ID_,DHT,Habitat_Type,Lake,DO_mgl,DO_percent, Conductivity, Specific_conductivity, pH) %>%
  pivot_longer(cols = c(DO_mgl,DO_percent, Specific_conductivity, pH), 
               names_to = "Variable", values_to = "Values") %>%
  mutate(Lake = factor(Lake, levels = lake_order))

# Plot Chemical parameters
CHEM <- ggplot(Chemical_data, aes(Lake, Values, fill = Lake)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free", nrow = 1) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.background = element_rect(fill = "lightgrey"),
    strip.text = element_text(size = 10))

Temp <- ggplot(M_C_data, aes(Date, Temperature, fill = Lake)) +
  geom_point(shape = 21) +
  facet_wrap(~ "Temperature", scales = "free", nrow = 1) + 
  labs(x = "Date", y = NULL) +
  theme(legend.position = "bottom",strip.background = element_rect(fill = "lightgrey"),strip.text = element_text(size = 10))

Chemical_plot <- (CHEM + Temp + plot_layout(widths = c(4, 1))) +
  plot_layout(guides = "collect") & theme(legend.position = "right")

Chemical_plot

ggsave("Figures/Chemical_plot.png", Chemical_plot, width = 12, height = 5, dpi = 300)


# Biological parameters --------------------------------------------------------
# Remake Biological parameters
fish_order <- c("Presence_Kōura","Presence_Eel","Presence_Kōaro","Presence_Common_smelt","Presence_Bullies","Presence_Catfish","Presence_Morihana","Presence_Mosquitofish","Presence_Trout")
fish_order_CPUE <- c("Weighted_CPUE_Kōura", "Weighted_CPUE_Eel", "Weighted_CPUE_Kōaro", "Weighted_CPUE_Common_smelt", "Weighted_CPUE_Bullies", "Weighted_CPUE_Catfish", "Weighted_CPUE_Morihana", "Weighted_CPUE_Mosquitofish", "Weighted_CPUE_Trout")
fish_order_BCUE <- c("Weighted_BCUE_Kōura", "Weighted_BCUE_Eel", "Weighted_BCUE_Kōaro", "Weighted_BCUE_Common_smelt", "Weighted_BCUE_Bullies", "Weighted_BCUE_Catfish", "Weighted_BCUE_Morihana", "Weighted_BCUE_Mosquitofish", "Weighted_BCUE_Trout")

presence_data <- M_C_data %>%
  select(Monitoring_ID, DHT,Habitat_Type, Lake, Richness,Abundance, Presence_Kōura, Presence_Eel, Presence_Kōaro, Presence_Common_smelt, Presence_Bullies, Presence_Catfish, Presence_Morihana, Presence_Mosquitofish, Presence_Trout) %>%
  pivot_longer(cols = c(Presence_Kōura, Presence_Eel, Presence_Kōaro, Presence_Common_smelt, Presence_Bullies, Presence_Catfish, Presence_Morihana, Presence_Mosquitofish, Presence_Trout),names_to = "Fish_Type", values_to = "Presence") %>%
  mutate(Lake = factor(Lake, levels = lake_order),Fish_Type = factor(Fish_Type, levels = fish_order))

CPUE_data <- M_C_data %>%
  select(Monitoring_ID, DHT,Habitat_Type, Lake, Weighted_CPUE_Kōura, Weighted_CPUE_Eel, Weighted_CPUE_Kōaro, Weighted_CPUE_Common_smelt, Weighted_CPUE_Bullies, Weighted_CPUE_Catfish, Weighted_CPUE_Morihana, Weighted_CPUE_Mosquitofish, Weighted_CPUE_Trout) %>%
  pivot_longer(cols = c(Weighted_CPUE_Kōura, Weighted_CPUE_Eel, Weighted_CPUE_Kōaro, Weighted_CPUE_Common_smelt, Weighted_CPUE_Bullies, Weighted_CPUE_Catfish, Weighted_CPUE_Morihana, Weighted_CPUE_Mosquitofish, Weighted_CPUE_Trout),names_to = "Fish_Type", values_to = "CPUE") %>%
  mutate(Lake = factor(Lake, levels = lake_order),Fish_Type = factor(Fish_Type, levels = fish_order_CPUE))

BCUE_data <- M_C_data %>%
  select(Monitoring_ID, DHT,Habitat_Type, Lake, Weighted_BCUE_Kōura, Weighted_BCUE_Eel, Weighted_BCUE_Kōaro, Weighted_BCUE_Common_smelt, Weighted_BCUE_Bullies, Weighted_BCUE_Catfish, Weighted_BCUE_Morihana, Weighted_BCUE_Mosquitofish, Weighted_BCUE_Trout) %>%
  pivot_longer(cols = c(Weighted_BCUE_Kōura, Weighted_BCUE_Eel, Weighted_BCUE_Kōaro, Weighted_BCUE_Common_smelt, Weighted_BCUE_Bullies, Weighted_BCUE_Catfish, Weighted_BCUE_Morihana, Weighted_BCUE_Mosquitofish, Weighted_BCUE_Trout),names_to = "Fish_Type", values_to = "BCUE") %>%
  mutate(Lake = factor(Lake, levels = lake_order),Fish_Type = factor(Fish_Type, levels = fish_order_BCUE))

BCUE_summary <- M_C_data %>%
  select(Monitoring_ID, DHT,Habitat_Type, Lake, Weighted_BCUE_Kōura, Weighted_BCUE_Eel, Weighted_BCUE_Kōaro, Weighted_BCUE_Common_smelt, Weighted_BCUE_Bullies, Weighted_BCUE_Catfish, Weighted_BCUE_Morihana, Weighted_BCUE_Mosquitofish, Weighted_BCUE_Trout) %>%
  pivot_longer(cols = c(Weighted_BCUE_Kōura, Weighted_BCUE_Eel, Weighted_BCUE_Kōaro, Weighted_BCUE_Common_smelt, Weighted_BCUE_Bullies, Weighted_BCUE_Catfish, Weighted_BCUE_Morihana, Weighted_BCUE_Mosquitofish, Weighted_BCUE_Trout),names_to = "Fish_Type", values_to = "BCUE") %>%
  mutate(Lake = factor(Lake, levels = lake_order),Fish_Type = factor(Fish_Type, levels = fish_order_BCUE)) %>%
  group_by(Lake,Habitat_Type, Fish_Type) %>%
  summarise(mean_BCUE = mean(BCUE, na.rm = TRUE),se_BCUE = sd(BCUE, na.rm = TRUE) / sqrt(n()),.groups = "drop")

Total_Weight_summary <- M_C_data %>%
  select(Monitoring_ID, DHT, Habitat_Type, Lake,Total_Weight_Bullies, Total_Weight_Morihana, Total_Weight_Kōura,Total_Weight_Common_smelt, Total_Weight_Eel, Total_Weight_Kōaro,Total_Weight_Trout, Total_Weight_Mosquitofish, Total_Weight_Catfish) %>%
  pivot_longer(cols = c(Total_Weight_Bullies, Total_Weight_Morihana, Total_Weight_Kōura,Total_Weight_Common_smelt, Total_Weight_Eel, Total_Weight_Kōaro,Total_Weight_Trout, Total_Weight_Mosquitofish, Total_Weight_Catfish),
    names_to = "Fish_Type", values_to = "Total_Weight") %>%
  mutate(Lake = factor(Lake, levels = lake_order)) %>%
  group_by(Fish_Type) %>%
  summarise(Total_Weight = sum(Total_Weight, na.rm = TRUE),.groups = "drop")

# plot Richness & Abundance
plot_abundance <-ggplot(presence_data, aes(Lake, Abundance, fill = Lake)) +
  geom_boxplot() +
  facet_grid(~ Habitat_Type, scales = "free")

plot_richness <-ggplot(presence_data, aes(Lake, Richness, fill = Lake)) +
  geom_boxplot() +
  facet_grid(~ Habitat_Type, scales = "free")

plot_abundance / plot_richness
 



  # Plot Biological parameters
ggplot(presence_data, aes(Lake, Presence, fill = Fish_Type)) +
  geom_bar(stat = "identity", position = "stack") 
  #facet_wrap( ~ Lake, scales = "free_x") +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(CPUE_data, aes(Lake, CPUE, fill = Fish_Type)) +
  geom_boxplot()
  #facet_grid( ~ DHT, scales = "free_x") +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(BCUE_data, aes(Lake, BCUE, fill = Fish_Type)) +
  geom_boxplot()
  #facet_grid( ~ DHT, scales = "free_x") +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(BCUE_summary, aes(Lake, mean_BCUE, fill = Fish_Type)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_BCUE - se_BCUE, ymax = mean_BCUE + se_BCUE),position = position_dodge(width = 0.9), width = 0.2) +
  facet_grid( ~ Habitat_Type, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))




# Koura plots
plot1 <- M_C_data %>%
  ggplot(aes(Lake, Total_Individuals_Kōura, fill = Lake)) +
  geom_boxplot() +
  facet_grid(~ Habitat_Type, scales = "free") +
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(),legend.position = "none")

plot2 <- M_C_data %>%
  ggplot(aes(Lake, Weighted_CPUE_Kōura, fill = Lake)) +
  geom_boxplot() +
  facet_grid(~ Habitat_Type, scales = "free") +
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())


plot3 <- M_C_data %>%
  ggplot(aes(Lake, Weighted_BCUE_Kōura, fill = Lake)) +
  geom_boxplot() +
  facet_grid(~ Habitat_Type, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = .3),legend.position = "none")

plot1 / plot2 / plot3


ggplot(M_C_data, aes(Distance_deep,Weighted_CPUE_Kōura, col=Lake)) +
  geom_point()

ggplot(M_C_data, aes(TLI,Weighted_CPUE_Kōura, col=Lake)) +
  geom_point() 

ggplot(M_C_data, aes(Temperature,Weighted_CPUE_Kōura, col=Lake)) +
  geom_point()

ggplot(M_C_data, aes(Presence_Morihana,Weighted_CPUE_Kōura, col=Lake)) +
  geom_point()

ggplot(M_C_data, aes(Presence_Eel,Weighted_CPUE_Kōura, col=Lake)) +
  geom_point()

ggplot(M_C_data, aes(Overhanging_trees,Weighted_CPUE_Kōura, col=Lake)) +
  geom_point()





# Spatial plots
M_C_data_sf <- M_C_data %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

ggplot() +
  geom_sf(data=combined_outlines)+
  geom_sf(data=M_C_data_sf, aes(fill=Weighted_CPUE_Kōura), shape=21, size=4)
 

ggplot() +
  geom_sf(data = Rotorua_outline) +
  geom_sf(data = Rotoiti_outline) +
  geom_sf(data = Rotoehu_outline) +
  geom_sf(data = Rotomā_outline) +
  geom_sf(data = Ōkāreka_outline) +
 # geom_sf(data = combined_outlines) +
  geom_sf(data = M_C_data_sf, aes(fill = Weighted_CPUE_Kōura), shape = 21, size = 4) +
  scale_fill_viridis_c(name = "Weighted CPUE Kōura") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 18, hjust = 0.5), 
        strip.text = element_text(size = 14), 
        panel.grid = element_blank()) +
  facet_grid(~ Lake, ) +
  labs(title = "Kōura Weighted CPUE with Site Locations", x = "Longitude", y = "Latitude")

library(patchwork) # For combining plots

# Create individual plots
plot_rotorua <- ggplot() +
  geom_sf(data = subset(combined_outlines, Lake == "Rotorua")) +
  geom_sf(data = subset(M_C_data_sf, Lake == "Rotorua"), aes(fill = Weighted_CPUE_Kōura), shape = 21, size = 4) +
  scale_fill_viridis_c(name = "Weighted CPUE Kōura") +
  theme_bw() +
  labs(title = "Lake Rotorua", x = "Longitude", y = "Latitude")

plot_rotoiti <- ggplot() +
  geom_sf(data = subset(combined_outlines, Lake == "Rotoiti")) +
  geom_sf(data = subset(M_C_data_sf, Lake == "Rotoiti"), aes(fill = Weighted_CPUE_Kōura), shape = 21, size = 4) +
  scale_fill_viridis_c(name = "Weighted CPUE Kōura") +
  theme_bw() +
  labs(title = "Lake Rotoiti", x = "Longitude", y = "Latitude")

plot_rotoehu <- ggplot() +
  geom_sf(data = subset(combined_outlines, Lake == "Rotoehu")) +
  geom_sf(data = subset(M_C_data_sf, Lake == "Rotoehu"), aes(fill = Weighted_CPUE_Kōura), shape = 21, size = 4) +
  scale_fill_viridis_c(name = "Weighted CPUE Kōura") +
  theme_bw() +
  labs(title = "Lake Rotoehu", x = "Longitude", y = "Latitude")

plot_rotoma <- ggplot() +
  geom_sf(data = subset(combined_outlines, Lake == "Rotomā")) +
  geom_sf(data = subset(M_C_data_sf, Lake == "Rotomā"), aes(fill = Weighted_CPUE_Kōura), shape = 21, size = 4) +
  scale_fill_viridis_c(name = "Weighted CPUE Kōura") +
  theme_bw() +
  labs(title = "Lake Rotomā", x = "Longitude", y = "Latitude")

subset_early <- subset(M_C_data_sf, Lake == "Ōkāreka" & Date_Time <= as.Date("2024-12-31"))
subset_late <- subset(M_C_data_sf, Lake == "Ōkāreka" & Date_Time > as.Date("2024-12-31"))

plot_okareka_early <- ggplot() +
  geom_sf(data = subset(combined_outlines, Lake == "Ōkāreka")) +
  geom_sf(data = subset(subset_early, Lake == "Ōkāreka"), aes(fill = Weighted_CPUE_Kōura), shape = 21, size = 4) +
  scale_fill_viridis_c(name = "Weighted CPUE Kōura") +
  theme_bw() +
  labs(title = "Lake Ōkāreka early", x = "Longitude", y = "Latitude")

plot_okareka_late <- ggplot() +
  geom_sf(data = subset(combined_outlines, Lake == "Ōkāreka")) +
  geom_sf(data = subset(subset_late, Lake == "Ōkāreka"), aes(fill = Weighted_CPUE_Kōura), shape = 21, size = 4) +
  scale_fill_viridis_c(name = "Weighted CPUE Kōura") +
  theme_bw() +
  labs(title = "Lake Ōkāreka late", x = "Longitude", y = "Latitude")

# Combine all plots
(plot_rotorua | plot_rotoiti) / (plot_rotoehu | plot_rotoma | plot_okareka_early| plot_okareka_late)



#
# Lake Parameters --------------------------------------------------------------
plot(M_C_data$TLI, M_C_data$Elevation_m)


# Time Effect on double sampled sites in Ōkāreka -------------------------------
# Plot some primarily data 
ggplot(Monitoring_CPUE_data, aes(x = Date_Time)) +
  geom_point(aes(y = Weighted_CPUE_Kōura, color = "CPUE"), size = 1) +
  geom_point(aes(y = Temperature, color = "Temperature"), size = 1) +
  scale_color_manual(values = c("CPUE" = "blue", "Temperature" = "red")) +  # Set the colors for the lines
  scale_y_continuous(
    name = "CPUE", 
    sec.axis = sec_axis(~ ., name = "Temperature")) 

# Filter the data for Ōkāreka sites with two sampling dates
Changeovertime <- Monitoring_CPUE_data %>%
  filter(Monitoring_ID %in% c("109_0", "109_1", "111_0", "111_1", 
                              "115_0", "115_1", "116_0", "116_1", 
                              "117_0", "117_1", "119_0", "119_1"))

# Paired t-tests
# Temperature
temp_0 <- Changeovertime %>% filter(Monitoring == "0") %>% arrange(Site_ID) %>% pull(Temperature)
temp_1 <- Changeovertime %>% filter(Monitoring == "1") %>% arrange(Site_ID) %>% pull(Temperature)
t_temp <- t.test(temp_0, temp_1, paired = TRUE)
t_temp

# Weighted CPUE Kōura
cpue_0 <- Changeovertime %>% filter(Monitoring == "0") %>% arrange(Site_ID) %>% pull(Weighted_CPUE_Kōura)
cpue_1 <- Changeovertime %>% filter(Monitoring == "1") %>% arrange(Site_ID) %>% pull(Weighted_CPUE_Kōura)
t_cpue <- t.test(cpue_0, cpue_1, paired = TRUE)
t_cpue

# ANOVA with Monitoring_ID as blocking factor
# Temperature
anova_temp <- aov(Temperature ~ Monitoring + Site_ID, data = Changeovertime)
anova_temp
# Weighted CPUE Kōura
anova_cpue <- aov(Weighted_CPUE_Kōura ~ Monitoring + Site_ID, data = Changeovertime)
anova_cpue

# Temperature mixed model
lmer_temp <- lmer(Temperature ~ Monitoring + (1 | Site_ID ), data = Changeovertime)
summary(lmer_temp)

# Weighted CPUE Kōura mixed model
lmer_cpue <- lmer(Weighted_CPUE_Kōura ~ Monitoring + (1 | Site_ID ), data = Changeovertime)
summary(lmer_cpue)


# Create a long-format version for plotting
Changeovertime_long <- Changeovertime %>%
  pivot_longer(cols = c(Temperature, Weighted_CPUE_Kōura),
               names_to = "Variable", values_to = "Value")

# Store t-test results for annotation
temp_label <- paste0("Temp Δ = ", round(t_temp$estimate, 2), 
                     "\np = ", signif(t_temp$p.value, 3))
cpue_label <- paste0("CPUE Δ = ", round(t_cpue$estimate, 2), 
                     "\np = ", signif(t_cpue$p.value, 3))

# Plot
ggplot(Changeovertime_long, aes(x =as.factor(Monitoring), y = Value, fill = Variable)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.8)) +
  annotate("text", x = 1.5, y = 19, 
           label = temp_label, color = "#F8766D", size = 4) +
  annotate("text", x = 1.5, y = 11, 
           label = cpue_label, color = "#00BFC4", size = 4) +
  theme_bw() +
  theme(legend.title = element_blank())

#
# Calculate Substrate Index (s) ------------------------------------------------


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

substrate_weights <- c(
  Bedrock = 0.07,  # Keep neutral influence
  Boulders = 0.10, # Reduce weight (weak relationship)
  Cobble = 0.08,   # Reduce weight (weak relationship)
  Gravel = 0.07,   # Reduce slightly (weak relationship)
  Sand = 0.15,     # Increase due to significant positive relationship
  Mud = 0.05,      # Increase slightly, still negative
  Organic_matter = -0.10  # Significant but negative influence
)


substrate_weights_J <- c(Bedrock= 0.08, Boulders= 0.07, Cobble= 0.06, Gravel = 0.05, Sand = 0.035, Mud = 0.035)

substrate_values <- M_C_data[c("Bedrock","Boulders","Cobble", "Gravel", "Sand", "Mud", "Organic_matter")]

substrate <- c("Bedrock", "Boulders", "Cobble", "Gravel", "Sand", "Mud", "Organic_matter")

# Calculate the substrate index 
# Loop through each variable and generate a boxplot
for (var in substrate) {
  p <- ggplot(M_C_data, aes(x = Presence_Kōura, y = get(var))) +
    geom_boxplot() +
    labs(x = "Presence of Kōura", y = var, title = paste("Boxplot of", var, "by Presence_Kōura"))
  print(p)}

M_C_data$Substrate_index <- rowSums(substrate_values * substrate_weights, na.rm = TRUE)

names(M_C_data)
summary(M_C_data)

#Create the 'presence_rocks' variable
M_C_data <- M_C_data %>%
  mutate(Presence_rocks = if_else(Cobble > 1 | Boulders > 1 | Bedrock > 1, 1, 0))

# List of variables to test
variables <- c("Presence_Kōura","Total_Individuals_Kōura","Weighted_CPUE_Kōura", "Weighted_BCUE_Kōura",#"Total_Weight_Kōura",
               "LID","Date_Time_Numeric","Mean_depth_m", "Elevation_m", "Slope",  "Distance_deep",
               "Temperature", "DO_mgl", "DO_percent", "pH", "TLI", "Lake_water_clarity_m",
               "Bedrock","Boulders", "Cobble", "Gravel", "Sand", "Mud", "Organic_matter","Presence_rocks", #"Substrate_index", 
               "Riparian_vegetation", "Overhanging_trees", 
               "Emergent_weed", "Submerged_weed", "Wood_cover",
               "Presence_Kōaro","Presence_Common_smelt", "Presence_Trout", "Presence_Morihana", "Presence_Eel",
               "Weighted_CPUE_Morihana")


Numeric_variables <- M_C_data %>% select_if(is.numeric) %>% names()

Selected_variables <- c("Distance_deep","TLI","Temperature","Presence_Morihana","Presence_Eel","Overhanging_trees")

