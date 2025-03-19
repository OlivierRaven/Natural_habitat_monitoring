# 1. Import_data
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
Natural_habitat <- read_excel("Data_raw/Natural_habitat.xlsx")
Monitoring_data <- read_excel("Data_raw/Natural_habitat.xlsx", sheet = "Monitoring_data")
Fish_data <- read_excel("Data_raw/Natural_habitat.xlsx", sheet = "Fish_data") %>% select(-starts_with("..."))



# Combine monitoring, cpue, and natural habitat data ---------------------------
# Calculate CPUE and BCUE by Site and Species and net_Type
CPUE_BCUE <- Fish_data %>%
  filter(!is.na(Species)) %>%
  group_by(Monitoring_ID, Species,Net_type) %>%
  reframe(
    Total_Individuals = sum(Amount, na.rm = T),
    Total_Weight = sum(Weight_g, na.rm = TRUE),
    Total_Effort = first(Amount_nets),
    CPUE = Total_Individuals / Total_Effort,      
    BCUE = Total_Weight / Total_Effort,
    Mean_Length = mean(Length_mm, na.rm = TRUE),
    Min_Length = ifelse(all(is.na(Length_mm)), NA, min(Length_mm, na.rm = TRUE)),
    Max_Length = ifelse(all(is.na(Length_mm)), NA, max(Length_mm, na.rm = TRUE)),
    Mean_Weight = mean(Weight_g, na.rm = TRUE),
    Min_Weight = ifelse(all(is.na(Weight_g)), NA, min(Weight_g, na.rm = TRUE)),
    Max_Weight = ifelse(all(is.na(Weight_g)), NA, max(Weight_g, na.rm = TRUE)))

CPUE_BCUE_weighted <- CPUE_BCUE %>%
  group_by(Monitoring_ID, Species) %>%
  summarise(
    Total_Individuals = sum(Total_Individuals, na.rm = T),
    Total_Weight = sum(Total_Weight, na.rm = TRUE),
    Weighted_CPUE_numerator = sum(CPUE * Total_Effort, na.rm = TRUE),
    Weighted_BCUE_numerator = sum(BCUE * Total_Effort, na.rm = TRUE),
    Total_Effort_sum = sum(Total_Effort, na.rm = TRUE),
    
    Mean_Length = mean(Mean_Length, na.rm = TRUE),
    Min_Length = ifelse(all(is.na(Min_Length)), NA, min(Min_Length, na.rm = TRUE)),
    Max_Length = ifelse(all(is.na(Max_Length)), NA, max(Max_Length, na.rm = TRUE)),
    
    Mean_Weight = mean(Mean_Weight, na.rm = TRUE),
    Min_Weight = ifelse(all(is.na(Min_Weight)), NA, min(Min_Weight, na.rm = TRUE)),
    Max_Weight = ifelse(all(is.na(Max_Weight)), NA, max(Max_Weight, na.rm = TRUE)) ) %>%
  ungroup() %>%  
  mutate(
    Total_Effort_sum = ifelse(Monitoring_ID %in% c("96_0", "101_0", "117_1", "119_1"), 3, 4),  
    Weighted_CPUE = Weighted_CPUE_numerator / Total_Effort_sum,
    Weighted_BCUE = Weighted_BCUE_numerator / Total_Effort_sum )

# Create Presence/Absence Columns Based on Species Naming
species_presence_absence <- Fish_data %>%
  filter(!is.na(Species)) %>%
  distinct(Monitoring_ID, Species) %>% 
  mutate(Presence = 1) %>%       
  pivot_wider(
    names_from = Species, 
    values_from = Presence, 
    values_fill = list(Presence = 0), 
    names_prefix = "Presence_")


# Create a metadata table with Parameter-Unit mappings
Monitoring_data <- Monitoring_data %>%  select(-Site_ID)
unit_metadata <- Monitoring_data %>%
  select(Parameter, Unit) %>%
  distinct()

# Pivot wider without modifying column names
Monitoring_CPUE_data <- Natural_habitat %>%
  left_join(Monitoring_data %>%
              select(-Group, -Notes, -Unit) %>%
              group_by(Monitoring_ID, Parameter) %>%
              mutate(
                Parameter_num = ifelse(duplicated(Parameter), row_number(), NA)) %>%
              ungroup() %>%
              pivot_wider(
                names_from = c(Parameter, Parameter_num),  # Combine Parameter and its occurrence number
                values_from = Value,
                names_glue = "{Parameter}{ifelse(is.na(Parameter_num), '', paste0('_', Parameter_num))}",  # Remove the "_NA" from column names
                values_fill = list(Value = NA)),
            by = "Monitoring_ID") %>%
  left_join(CPUE_BCUE_weighted %>%
              pivot_wider(
                names_from = Species,
                values_from = c(Total_Individuals, Weighted_CPUE, Weighted_BCUE, Total_Weight, Mean_Length, Mean_Weight, Min_Length, Max_Length, Min_Weight, Max_Weight, Weighted_CPUE_numerator, Weighted_BCUE_numerator, Total_Effort_sum),
                names_sep = "_",
                values_fill = list(Total_Individuals = 0, Weighted_CPUE = 0, Weighted_BCUE = 0)),
            by = "Monitoring_ID")%>%
  left_join(species_presence_absence, by = "Monitoring_ID")



# Save as CSV
write.csv(Monitoring_CPUE_data, "Data_mod/Monitoring_CPUE_data.csv", row.names = FALSE)

# Save as Excel file
write_xlsx(Monitoring_CPUE_data, "Data_mod/Monitoring_CPUE_data.xlsx")

plot(Monitoring_CPUE_data$Total_Individuals_Kōura, Monitoring_CPUE_data$Total_Weight_Kōura)
plot(Monitoring_CPUE_data$Weighted_CPUE_Kōura, Monitoring_CPUE_data$Weighted_BCUE_Kōura)
plot(Monitoring_CPUE_data$Weighted_BCUE_Kōura, Monitoring_CPUE_data$Total_Weight_Kōura)







# Explore the Raw data -----------------------------------------------------------
####### Explore Fish_data & Calculate CPUE
names(Fish_data)

species_summary <- Fish_data %>%
  filter(!is.na(Species)) %>%  # Remove rows where Species is NA
  group_by(Species) %>%
  summarize(
    Total_Records = n(),  
    Sites_Present = n_distinct(Site_ID),
    Avg_Length = mean(Length_mm, na.rm = TRUE),  
    Avg_Weight = mean(Weight_g, na.rm = TRUE),   
    Total_Amount = sum(Amount, na.rm = TRUE),    
    Total_Weight = sum(Weight_g, na.rm = TRUE),  
    Sites_Present = n_distinct(Site_ID),
    .groups = 'drop')
species_summary

ggplot(Fish_data %>% 
         filter(!Species %in% c("Bullies", "Common_smelt")) %>% 
         filter(!is.na(Species)) %>% 
         filter(!is.na(Length_mm)),
       aes(x = Length_mm, fill = Sex)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  facet_wrap(~Species, scales = "free") +
  labs(title = "Histogram of Fish Length by Species and Sex") 

ggplot(Fish_data %>% 
         filter(!Species %in% c("Bullies", "Common_smelt", "Trout", "Catfish", "Eel")) %>% 
         filter(!is.na(Species)) %>% 
         filter(!is.na(Length_mm) & !is.na(Weight_g)),
       aes(Length_mm, Weight_g, col = Sex)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Species, scales = "free")

Fish_data_2 <- Fish_data %>% 
  left_join(Natural_habitat, by = "Monitoring_ID") %>% 
  filter(!is.na(Monitoring_ID))



# Calculate the total count
total_count <- Fish_data_2 %>%
  filter(Species == "Kōura") %>%
  nrow()

total_count_by_lake <- Fish_data_2 %>%
  filter(Species == "Kōura") %>%
  group_by(Lake) %>%
  summarise(total_Kōura = n())

total_count_by_lake_DHT <- Fish_data_2 %>%
  filter(Species == "Kōura") %>%
  group_by(Lake, DHT) %>%
  summarise(total_Kōura = n())

# Create the histogram with the total count in the title
ggplot(Fish_data %>% 
         filter(Species == "Kōura") %>% 
         filter(!is.na(Length_mm)),
       aes(x = Length_mm )) +
  geom_histogram(binwidth = 1, color = "black", na.rm = TRUE) +
  labs(x = "Length (mm)", 
       y = "Count", 
       title = paste("Histogram of Kōura Lengths (Total Count:", total_count,")"))

ggplot(Fish_data %>% 
         filter(Species == "Kōura") %>% 
         filter(!is.na(Length_mm)),
       aes(x = Length_mm )) +
  geom_histogram(binwidth = 1, color = "black", na.rm = TRUE) +
  geom_vline(xintercept = c(22, 28), linetype = "dashed") +
  annotate("text", x = 11, y = 27, label = "Small") +
  annotate("text", x = 25, y = 27, label = "Medium") +
  annotate("text", x = 35, y = 27, label = "Large") +
  labs(x = "Length (mm)", 
       y = "Count",title = paste("Histogram of Kōura Lengths (Total Count:", total_count, ")"))

ggplot(Fish_data %>% 
         filter(Species == "Kōura") %>% 
         filter(!is.na(Length_mm), !is.na(Sex)),
       aes(x = Length_mm, fill = Sex)) +
  geom_histogram(binwidth = 3, color = "black", position = "dodge", na.rm = TRUE) +
  labs(x = "OCL Length (mm)", 
       y = "Count", 
       fill = "Gender", 
       title = paste("Histogram of Kōura Lengths by Gender (Total Count:",total_count,")")) +
  facet_wrap(~Net_type ) + # Create facets for Gender
  theme(legend.position = "top")


names(Fish_data_2)
Fish_data_2_summary <- Fish_data_2 %>%
  filter(!is.na(Lake), !is.na(DHT)) %>%                  
  distinct(Monitoring_ID, Net_type, .keep_all = TRUE) %>% 
  group_by(DHT, Lake) %>%                                
  summarise(
    Sites_Monitored = n_distinct(Monitoring_ID),         
    Total_Nets = sum(Amount_nets, na.rm = TRUE),  
    .groups = "drop")

Fish_data_2_summary <- Fish_data_2_summary %>%
  left_join(total_count_by_lake_DHT, by = c("Lake", "DHT"))

# Create the plot
ggplot(Fish_data_2 %>% 
         filter(Species == "Kōura") %>% 
         filter(!is.na(Length_mm), !is.na(Sex)),
       aes(x = Length_mm, fill = Sex)) +
  geom_histogram(binwidth = 3, color = "black", position = "dodge", na.rm = TRUE) +
  labs(x = "OCL Length (mm)", 
       y = "Count", 
       fill = "Gender", 
       title = "Histogram of Kōura Lengths (Total Count:",total_count,")") +
  facet_grid(DHT ~ Lake) + # Ensure each Lake has its own row/column
  geom_text(data = Fish_data_2_summary, 
            aes(x = Inf, y = Inf, 
                label = paste("Sites:", Sites_Monitored, 
                              "\nTotal Kōura:", total_Kōura)),
            inherit.aes = FALSE,
            hjust = 1.1, vjust = 1.1, size = 3) +  # Position and size adjustments
  theme(legend.position = "top")




# other species
ggplot(Fish_data_2 %>% 
         filter(Species == "Kōaro") %>% 
         filter(!is.na(Length_mm)),
       aes(x = Length_mm)) +
  geom_histogram(binwidth = 3, color = "black", position = "dodge", na.rm = TRUE) 

ggplot(Fish_data_2 %>% 
         filter(Species == "Kōaro") %>% 
         filter(!is.na(Length_mm)),
       aes(x = Length_mm)) +
  geom_histogram(binwidth = 3, color = "black", position = "dodge", na.rm = TRUE) +
  labs(x = "Length (mm)", 
       y = "Count", 
       fill = "Gender", 
       title = paste("Histogram of Kōaro Lengths")) +
 facet_grid(DHT ~ Lake) + # Ensure each Lake has its own column
  geom_text(data = Fish_data_2_summary, 
            aes(x = Inf, y = Inf, label = paste("Sites:", Sites_Monitored)),
            inherit.aes = FALSE,
            hjust = 1.1, vjust = 1.1, size = 3) +
  theme(legend.position = "top")

ggplot(Fish_data_2 %>% 
         filter(Species == "Morihana") %>% 
         filter(!is.na(Length_mm)),
       aes(x = Length_mm)) +
  geom_histogram(binwidth = 3, color = "black", position = "dodge", na.rm = TRUE) +
  labs(x = "Length (mm)", 
       y = "Count", 
       fill = "Gender", 
       title = paste("Histogram of Morihana Lengths")) +
  facet_grid(DHT ~ Lake) + 
  geom_text(data = Fish_data_2_summary, 
            aes(x = Inf, y = Inf, label = paste("Sites:", Sites_Monitored)),
            inherit.aes = FALSE,
            hjust = 1.1, vjust = 1.1, size = 3) +
  theme(legend.position = "top")



# Bullies
Bullies_data <- Fish_data_2 %>% filter(Species == "Bullies") 
summary(Bullies_data)


# Check the rows where both 'Weight_g' and 'Amount' are available
weight_and_amount_available <- Bullies_data %>%
  filter(!is.na(Weight_g) & !is.na(Amount))

# Calculate the weight per bully for each row
weight_and_amount_available <- weight_and_amount_available %>%
  mutate(weight_per_bully = Weight_g / Amount)

# Calculate the average weight per bully across all rows
Average_Weight_per_Bully_g <- mean(weight_and_amount_available$weight_per_bully, na.rm = TRUE)

# Calculate total weight, total amount, total volume, and average weight in a single summarise call
total_bullies_info <- Bullies_data %>%
  group_by(Site_ID) %>%
  summarise(
    Total_Weight_g = sum(Weight_g, na.rm = TRUE),
    Total_Amount = sum(Amount, na.rm = TRUE),
    Total_Cups = sum(Cups, na.rm =TRUE),
    Total_Volume_mL = sum(mL, na.rm = TRUE)) %>%
  left_join(average_weight_per_bully, by = "Site_ID")%>%
  mutate(Average_Weight_per_Cup = Total_Weight_g / Total_Cups)




# bullies check
Bullie_check <- M_C_data %>% select(Site_ID, DHT, Total_Individuals_Bullies, Weighted_CPUE_Bullies, 
                                     Weighted_BCUE_Bullies, Total_Weight_Bullies, Mean_Length_Bullies, 
                                     Mean_Weight_Bullies, Min_Length_Bullies, Max_Length_Bullies, 
                                     Min_Weight_Bullies, Max_Weight_Bullies)







# plot CPUE and BCUE by Site and Species and net_Type
ggplot(CPUE_BCUE_weighted, aes(x = factor(Monitoring_ID), y = Weighted_CPUE , fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") 

ggplot(CPUE_BCUE_weighted, aes(x = factor(Monitoring_ID), y = Weighted_BCUE, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") 


ggplot(CPUE_BCUE_weighted %>% filter(Species %in% c("Kōura", "Kōaro")),
  aes(x = factor(Monitoring_ID), y = Weighted_CPUE, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") 


ggplot(CPUE_BCUE_weighted, aes(Weighted_BCUE, Weighted_CPUE))+
  geom_point()+
  facet_wrap(~Species, scales = "free")





# Make plots of the data -------------------------------------------------------
# Set custom_colors
custom_colors <- c("Muddy" = "tan", "Sandy" = "orange","Rocky" = "gray", "Emergent Macrophyte" = "green", "AR"= "red",  "Cliff"= "black", "Geo"= "blue") 
# Set the order of DHT factor levels in Monitoring_CPUE_data to match custom_colors
Monitoring_CPUE_data$DHT <- factor(Monitoring_CPUE_data$DHT, levels = names(custom_colors))

names(Monitoring_CPUE_data)

ggplot(Monitoring_CPUE_data, aes(Lake, CPUE_Kōura, fill = DHT)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, height = 0) +
  scale_fill_manual(values = custom_colors)

ggplot(Monitoring_CPUE_data, aes(DHT, Mean_Length_Kōaro, fill=DHT))+
  geom_boxplot()+
  geom_jitter(width = 0.1,height = 0)+
  scale_fill_manual(values = custom_colors)

# Make plots of the habitat characters for each site.
Data <- Monitoring_CPUE_data %>% filter(!is.na(Date_Time))
Data_0 <- Data %>% mutate(across(everything(), ~ replace(., is.na(.), 0)))

Sediment_comp <- Data_0 %>%
  select("Site_ID","DHT","Bedrock_%","Boulders_%", "Cobble_%","Gravel_%","Sand_%","Mud_%","Organic_matter_%")

Sediment_comp_long <- melt(Sediment_comp, id.vars = c("Site_ID","DHT") ,
                  measure.vars = c("Bedrock_%", "Boulders_%", "Cobble_%", "Gravel_%", 
                                   "Sand_%", "Mud_%", "Organic_matter_%"), 
                  variable.name = "Substrate", value.name = "Percentage")

Sediment_comp_long <- Sediment_comp_long %>%
  group_by(DHT) %>%
  mutate(Percentage = Percentage / sum(Percentage) * 100) %>%
  ungroup()

ggplot(Sediment_comp_long, aes(DHT, Percentage, fill = Substrate)) +
  geom_bar(stat = "identity") 

Habitat_comp <- Data_0 %>%
  select("Site_ID","DHT","Riparian_vegetation_%", "Overhanging_trees_%","Emergent_weed_%","Submerged_weed_%","Wood_cover_%")

Habitat_comp_long <- melt(Habitat_comp, id.vars = c("Site_ID","DHT") ,
                           measure.vars = c("Riparian_vegetation_%", "Overhanging_trees_%","Emergent_weed_%","Submerged_weed_%","Wood_cover_%"), 
                           variable.name = "Habitat", value.name = "Percentage")

ggplot(Habitat_comp_long, aes(Site_ID, Percentage, fill = Habitat)) +
  geom_bar(stat = "identity") 

# Summarize the total catch by lake
total_catch_by_lake <- All_data %>%
  group_by(Lake) %>%
  summarise(
    Total_Bullies = sum(Total_Catch_Bullies, na.rm = TRUE),
    Total_Kōura = sum(Total_Catch_Kōura, na.rm = TRUE),
    Total_Morihana = sum(Total_Catch_Morihana, na.rm = TRUE),
    Total_Eel = sum(Total_Catch_Eel, na.rm = TRUE),
    Total_Common_smelt = sum(`Total_Catch_Common smelt`, na.rm = TRUE),
    Total_Kōaro = sum(Total_Catch_Kōaro, na.rm = TRUE),
    Total_Trout = sum(Total_Catch_Trout, na.rm = TRUE)
  )

# Plot the total fish caught by lake
ggplot(total_catch_by_lake, aes(Lake, y = Total_Kōura )) +
  geom_bar(stat = "identity", position = "stack") 














