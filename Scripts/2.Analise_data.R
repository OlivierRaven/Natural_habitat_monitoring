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

# Start analisis ---------------------------------------------------------------
# View the structure and summary of the data set
names(Monitoring_CPUE_data)
str(Monitoring_CPUE_data)
summary(Monitoring_CPUE_data)


M_C_data <- Monitoring_CPUE_data 

# Extract additional date and time components
M_C_data <- M_C_data %>%
  mutate(
    Date = as.Date(Date_Time),                      
    Time = format(Date_Time, "%H:%M:%S"),
    Year = year(Date_Time),
    Month = month(Date_Time, label = TRUE),          
    Day = day(Date_Time),                             
    Season = case_when(                               
      Month %in% c("Dec", "Jan", "Feb") ~ "Summer",
      Month %in% c("Mar", "Apr", "May") ~ "Autumn",
      Month %in% c("Jun", "Jul", "Aug") ~ "Winter",
      Month %in% c("Sep", "Oct", "Nov") ~ "Spring",
      TRUE ~ NA_character_),
    Date_Time_Numeric = as.numeric(Date_Time))



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


# Make some plots to explore the data ------------------------------------------
names(M_C_data)
# Define the lake order
lake_order <- c("Rotorua", "Rotoiti", "Rotoehu", "Rotomā", "Ōkāreka")

M_C_data$Lake <- factor(M_C_data$Lake, levels = lake_order)

# Define a custom color palette
sediment_colors <- c("Bedrock" = "gray50","Boulders" = "gray30","Cobble" = "brown","Gravel" = "burlywood","Sand" = "gold","Mud" = "saddlebrown","Organic_matter" = "darkgreen", "Turf" = "forestgreen")

# Remake Physical parameters
sediment_data <- M_C_data %>%
  select(Monitoring_ID,DHT , Lake, Bedrock, Boulders, Cobble, Gravel, Sand, Mud, Organic_matter, Turf) %>%
  pivot_longer(cols = c(Bedrock, Boulders, Cobble, Gravel, Sand, Mud, Organic_matter, Turf), 
               names_to = "Sediment_Type", values_to = "Percentage") %>%
  mutate(Lake = factor(Lake, levels = lake_order))

weed_data <- M_C_data %>%
  select(Monitoring_ID,DHT , Lake, Emergent_weed, Submerged_weed,Wood_cover) %>%
  pivot_longer(cols = c(Emergent_weed, Submerged_weed,Wood_cover), 
               names_to = "Weeds", values_to = "Percentage") %>%
  mutate(Lake = factor(Lake, levels = lake_order))

# Plot Physical parameters
Sediment_plot <- ggplot(sediment_data, aes(factor(Monitoring_ID), Percentage, fill = Sediment_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = sediment_colors) +
  facet_grid( ~ Lake, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0))

Weed_plot <- ggplot(weed_data, aes(factor(Monitoring_ID),Percentage, fill = Weeds)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid( ~ Lake, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0))

Sediment_plot/Weed_plot

ggplot(M_C_data, aes(Distance_deep,Weighted_CPUE_Kōura))+
  geom_point(aes(col=Lake))


# Remake Chemical parameters
Chemical_data <- M_C_data %>%
  select(Monitoring_ID,DHT,Lake,DO_mgl,DO_percent,Connectivity,pH) %>%
  pivot_longer(cols = c(DO_mgl,DO_percent,Connectivity,pH), 
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

(CHEM + Temp + plot_layout(widths = c(4, 1))) +
  plot_layout(guides = "collect") & theme(legend.position = "right")


# Remake Biological parameters
fish_order <- c("Presence_Kōura","Presence_Eel","Presence_Kōaro","Presence_Common_smelt","Presence_Bullies","Presence_Catfish","Presence_Morihana","Presence_Mosquitofish","Presence_Trout")
fish_order_CPUE <- c("Weighted_CPUE_Kōura", "Weighted_CPUE_Eel", "Weighted_CPUE_Kōaro", "Weighted_CPUE_Common_smelt", "Weighted_CPUE_Bullies", "Weighted_CPUE_Catfish", "Weighted_CPUE_Morihana", "Weighted_CPUE_Mosquitofish", "Weighted_CPUE_Trout")
fish_order_BCUE <- c("Weighted_BCUE_Kōura", "Weighted_BCUE_Eel", "Weighted_BCUE_Kōaro", "Weighted_BCUE_Common_smelt", "Weighted_BCUE_Bullies", "Weighted_BCUE_Catfish", "Weighted_BCUE_Morihana", "Weighted_BCUE_Mosquitofish", "Weighted_BCUE_Trout")

presence_data <- M_C_data %>%
  select(Monitoring_ID, DHT, Lake, Presence_Kōura, Presence_Eel, Presence_Kōaro, Presence_Common_smelt, Presence_Bullies, Presence_Catfish, Presence_Morihana, Presence_Mosquitofish, Presence_Trout) %>%
  pivot_longer(cols = c(Presence_Kōura, Presence_Eel, Presence_Kōaro, Presence_Common_smelt, Presence_Bullies, Presence_Catfish, Presence_Morihana, Presence_Mosquitofish, Presence_Trout),names_to = "Fish_Type", values_to = "Presence") %>%
  mutate(Lake = factor(Lake, levels = lake_order),Fish_Type = factor(Fish_Type, levels = fish_order))

CPUE_data <- M_C_data %>%
  select(Monitoring_ID, DHT, Lake, Weighted_CPUE_Kōura, Weighted_CPUE_Eel, Weighted_CPUE_Kōaro, Weighted_CPUE_Common_smelt, Weighted_CPUE_Bullies, Weighted_CPUE_Catfish, Weighted_CPUE_Morihana, Weighted_CPUE_Mosquitofish, Weighted_CPUE_Trout) %>%
  pivot_longer(cols = c(Weighted_CPUE_Kōura, Weighted_CPUE_Eel, Weighted_CPUE_Kōaro, Weighted_CPUE_Common_smelt, Weighted_CPUE_Bullies, Weighted_CPUE_Catfish, Weighted_CPUE_Morihana, Weighted_CPUE_Mosquitofish, Weighted_CPUE_Trout),names_to = "Fish_Type", values_to = "CPUE") %>%
  mutate(Lake = factor(Lake, levels = lake_order),Fish_Type = factor(Fish_Type, levels = fish_order_CPUE))

BCUE_data <- M_C_data %>%
  select(Monitoring_ID, DHT, Lake, Weighted_BCUE_Kōura, Weighted_BCUE_Eel, Weighted_BCUE_Kōaro, Weighted_BCUE_Common_smelt, Weighted_BCUE_Bullies, Weighted_BCUE_Catfish, Weighted_BCUE_Morihana, Weighted_BCUE_Mosquitofish, Weighted_BCUE_Trout) %>%
  pivot_longer(cols = c(Weighted_BCUE_Kōura, Weighted_BCUE_Eel, Weighted_BCUE_Kōaro, Weighted_BCUE_Common_smelt, Weighted_BCUE_Bullies, Weighted_BCUE_Catfish, Weighted_BCUE_Morihana, Weighted_BCUE_Mosquitofish, Weighted_BCUE_Trout),names_to = "Fish_Type", values_to = "BCUE") %>%
  mutate(Lake = factor(Lake, levels = lake_order),Fish_Type = factor(Fish_Type, levels = fish_order_BCUE))

BCUE_summary <- M_C_data %>%
  select(Monitoring_ID, DHT, Lake, Weighted_BCUE_Kōura, Weighted_BCUE_Eel, Weighted_BCUE_Kōaro, Weighted_BCUE_Common_smelt, Weighted_BCUE_Bullies, Weighted_BCUE_Catfish, Weighted_BCUE_Morihana, Weighted_BCUE_Mosquitofish, Weighted_BCUE_Trout) %>%
  pivot_longer(cols = c(Weighted_BCUE_Kōura, Weighted_BCUE_Eel, Weighted_BCUE_Kōaro, Weighted_BCUE_Common_smelt, Weighted_BCUE_Bullies, Weighted_BCUE_Catfish, Weighted_BCUE_Morihana, Weighted_BCUE_Mosquitofish, Weighted_BCUE_Trout),names_to = "Fish_Type", values_to = "BCUE") %>%
  mutate(Lake = factor(Lake, levels = lake_order),Fish_Type = factor(Fish_Type, levels = fish_order_BCUE)) %>%
  group_by(Lake, DHT, Fish_Type) %>%
  summarise(mean_BCUE = mean(BCUE, na.rm = TRUE),se_BCUE = sd(BCUE, na.rm = TRUE) / sqrt(n()),.groups = "drop")

Total_Weight_summary <- M_C_data %>%
  select(Monitoring_ID, DHT, Lake,Total_Weight_Bullies, Total_Weight_Morihana, Total_Weight_Kōura,Total_Weight_Common_smelt, Total_Weight_Eel, Total_Weight_Kōaro,Total_Weight_Trout, Total_Weight_Mosquitofish, Total_Weight_Catfish) %>%
  pivot_longer(cols = c(Total_Weight_Bullies, Total_Weight_Morihana, Total_Weight_Kōura,Total_Weight_Common_smelt, Total_Weight_Eel, Total_Weight_Kōaro,Total_Weight_Trout, Total_Weight_Mosquitofish, Total_Weight_Catfish),
    names_to = "Fish_Type", values_to = "Total_Weight") %>%
  mutate(Lake = factor(Lake, levels = lake_order)) %>%
  group_by(Fish_Type) %>%
  summarise(Total_Weight = sum(Total_Weight, na.rm = TRUE),.groups = "drop")


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
  facet_grid( ~ DHT, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Koura plots
plot1 <- M_C_data %>%
  filter(DHT != "Muddy") %>%
  ggplot(aes(Lake, Total_Individuals_Kōura, fill = Lake)) +
  geom_boxplot() +
  facet_grid(~ DHT, scales = "free") +
  theme(axis.title.x = element_blank())

plot2 <- M_C_data %>%
  filter(DHT != "Muddy") %>%
  ggplot(aes(Lake, Weighted_CPUE_Kōura, fill = Lake)) +
  geom_boxplot() +
  facet_grid(~ DHT, scales = "free") +
  theme(axis.title.x = element_blank())

plot3 <- M_C_data %>%
  filter(DHT != "Muddy") %>%
  ggplot(aes(Lake, Weighted_BCUE_Kōura, fill = Lake)) +
  geom_boxplot() +
  facet_grid(~ DHT, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = .3))

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
# Time Effect of the monitoring on Koura catches -------------------------------
# Plot some primarily data 
ggplot(M_C_data, aes(x = Date_Time)) +
  geom_point(aes(y = Weighted_CPUE_Kōura, color = "CPUE"), size = 1) +
  geom_point(aes(y = Temperature, color = "Temperature"), size = 1) +
  scale_color_manual(values = c("CPUE" = "blue", "Temperature" = "red")) +  # Set the colors for the lines
  scale_y_continuous(
    name = "CPUE", 
    sec.axis = sec_axis(~ ., name = "Temperature")) 


ggplot(M_C_data %>%
         filter(Monitoring_ID %in% c("109_0", "109_1", "111_0", "111_1", "115_0", "115_1","116_0", "116_1", "117_0", "117_1", "119_0", "119_1")),)+
  geom_point(aes(Monitoring_ID, Weighted_CPUE_Kōura), col="red")+
  geom_point(aes(Monitoring_ID, Temperature), col="blue")


# Filter the data for specific Monitoring_IDs
Changeovertime <- M_C_data %>%
  filter(Monitoring_ID %in% c("109_0", "109_1", "111_0", "111_1", "115_0", "115_1",
                              "116_0", "116_1", "117_0", "117_1", "119_0", "119_1"))


Changeovertime <- Changeovertime %>%
  mutate(Site = sub("_.*", "", Monitoring_ID),
         Timepoint = sub(".*_", "", Monitoring_ID))

# Pair the data within each Site by Timepoint
Changeovertime_wide <- Changeovertime %>%
  group_by(Site) %>%
  summarise(
    Temperature_0 = Temperature[Timepoint == "0"],
    Temperature_1 = Temperature[Timepoint == "1"],
    Weighted_CPUE_Kōura_0 = Weighted_CPUE_Kōura[Timepoint == "0"],
    Weighted_CPUE_Kōura_1 = Weighted_CPUE_Kōura[Timepoint == "1"]
  ) %>%
  ungroup()

# Calculate changes in temperature and Weighted CPUE
Changeovertime_wide <- Changeovertime_wide %>%
  mutate(
    Temp_Change = Temperature_1 - Temperature_0,
    CPUE_Change = Weighted_CPUE_Kōura_1 - Weighted_CPUE_Kōura_0
  )

# Calculate average increases
Average_Temp_Increase <- mean(Changeovertime_wide$Temp_Change, na.rm = TRUE)
Average_CPUE_Increase <- mean(Changeovertime_wide$CPUE_Change, na.rm = TRUE)

# Display averages
cat("Average Temperature Increase:", Average_Temp_Increase, "\n")
cat("Average Weighted CPUE Increase:", Average_CPUE_Increase, "\n")

# Plot the data with temperature and Weighted CPUE changes
ggplot(Changeovertime_wide, aes(x = Site)) +
  geom_point(aes(y = Weighted_CPUE_Kōura_0, color = "Weighted CPUE (2024-10-31)"), size = 3) +
  geom_point(aes(y = Weighted_CPUE_Kōura_1, color = "Weighted CPUE (2025-01-22)"), size = 3) +
  geom_point(aes(y = Temperature_0, color = "Temperature (2024-10-31)"), size = 3) +
  geom_point(aes(y = Temperature_1, color = "Temperature (2025-01-22)"), size = 3) +
  labs(x = "Site", y = "Value", color = "Metric") +
  ggtitle(paste("Changes in Temperature and Weighted CPUE for Kōura\n",
                "Avg Temp Increase:", round(Average_Temp_Increase, 2),
                "| Avg CPUE Increase:", round(Average_CPUE_Increase, 2))) +
  theme_bw()




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

ggplot(M_C_data, aes(x = as.factor(Presence_Kōura), y = Sand   )) +
  geom_jitter(width = 0.2, height = 0) 



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

pairs(M_C_data[, Selected_variables])

plot(M_C_data$Weighted_CPUE_Kōura, M_C_data$TLI)

# Correlation matrix -----------------------------------------------------------
library(reshape2)

cor_matrix <- cor(M_C_data[, variables], method = "spearman")  # "pearson", "spearman", "kendall" # spearman chosen as

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

# GLM --------------------------------------------------------------------------
formula_string <- paste("Weighted_CPUE_Kōura ~", paste(variables, collapse = " + "))
glm_model <- glm(as.formula(formula_string),
                 family = poisson(link = "log"), 
                 data = M_C_data)


glm_model <- glm(Weighted_CPUE_Kōura ~ Boulders+Wood_cover+Presence_Morihana+Submerged_weed  ,
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




