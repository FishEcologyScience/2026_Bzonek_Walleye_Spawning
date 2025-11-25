## --------------------------------------------------------------#
## Script name: Script5-2_Claude_beh_efish_integration
##
## Purpose of script: 
##    Integrate telemetry behavioral characteristics (site fidelity, 
##    spawning metrics) with electrofishing and habitat data
##    Build comprehensive spawning behavior-habitat relationship models
##
## Author: Paul Bzonek [Claude] 
##
## Date Created: 2025-01-11
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#

### Load required packages
#----------------------------#
library(tidyverse)
# sf, randomForest, corrplot, viridis, plotly, GGally, lme4, performance, geosphere will use :: notation

### Load and prepare base datasets
#----------------------------#
# Ensure df_behaviour exists from Script3-2_CombinedBehaviours.R
if(!exists("df_behaviour")) {
  cat("Loading behavioral data from Script3-2_CombinedBehaviours.R...\n")
  source("02 - Scripts/Script3-2_CombinedBehaviours.R")
}

# Load habitat and electrofishing data
if(!exists("data_habfish_pseudo.train")) {
  cat("Loading habitat-electrofishing data from Script1-1_format_data.R...\n")
  source("02 - Scripts/Script1-1_format_data.R")
}

### Define spatial matching parameters
#----------------------------#
param_spatial_buffer <- 100  # meters - optimal from Script3-3 analysis
param_temporal_buffer <- 30   # days - optimal from Script3-3 analysis

### Function: Calculate site-level behavioral metrics
#----------------------------#
calc_site_behavioral_metrics <- function(behavior_data, spawn_data, site_coords) {
  site_metrics <- tibble()
  
  for(i in 1:nrow(site_coords)) {
    site_lon <- site_coords$Start_Longitude[i]
    site_lat <- site_coords$Start_Latitude[i]
    site_year <- site_coords$Year[i]
    
    # Find spawning events near this site
    nearby_spawn <- spawn_data %>% 
      filter(year == site_year) %>% 
      mutate(
        distance = geosphere::distHaversine(
          cbind(deploy_long, deploy_lat),
          cbind(site_lon, site_lat)
        )
      ) %>% 
      filter(distance <= param_spatial_buffer)
    
    if(nrow(nearby_spawn) > 0) {
      # Get behavioral characteristics for fish detected at this site
      site_fish <- unique(nearby_spawn$animal_id)
      
      site_behavior <- behavior_data %>% 
        filter(animal_id %in% site_fish, year == site_year) %>% 
        summarise(
          # Site-level aggregated behavioral metrics
          n_fish = n(),
          
          # Site fidelity metrics
          mean_station_count = mean(station_count, na.rm = TRUE),
          mean_station_ratio = mean(station_count_ratio, na.rm = TRUE),
          sd_station_ratio = sd(station_count_ratio, na.rm = TRUE),
          
          # Home range metrics
          mean_mcp95 = mean(mcp95, na.rm = TRUE),
          mean_mcp95_spawn = mean(mcp95_spawn, na.rm = TRUE),
          mean_mcp95_ratio = mean(mcp95_ratio, na.rm = TRUE),
          
          # Spawning intensity metrics
          mean_residence_sum = mean(residence_sum, na.rm = TRUE),
          mean_detcount_sum = mean(detcount_sum, na.rm = TRUE),
          mean_depth_spawn = mean(depth_mean, na.rm = TRUE),
          
          # Individual consistency metrics
          cv_station_count = sd(station_count, na.rm = TRUE) / mean(station_count, na.rm = TRUE),
          cv_residence = sd(residence_sum, na.rm = TRUE) / mean(residence_sum, na.rm = TRUE),
          
          # Fish size metrics
          mean_length = mean(length_total, na.rm = TRUE),
          sd_length = sd(length_total, na.rm = TRUE),
          
          # Spawning participation
          prop_spawners = mean(SpawnBinary, na.rm = TRUE),
          
          # Detection intensity
          mean_total_detections = mean(detcount_all_sum, na.rm = TRUE),
          
          .groups = "drop"
        ) %>% 
        mutate(
          site_id = i,
          site_lon = site_lon,
          site_lat = site_lat,
          site_year = site_year
        )
      
      site_metrics <- bind_rows(site_metrics, site_behavior)
    } else {
      # No fish detected at this site
      empty_metrics <- tibble(
        site_id = i,
        site_lon = site_lon,
        site_lat = site_lat,
        site_year = site_year,
        n_fish = 0,
        # Set all other metrics to NA or 0
        mean_station_count = 0,
        mean_station_ratio = 0,
        sd_station_ratio = 0,
        mean_mcp95 = 0,
        mean_mcp95_spawn = 0,
        mean_mcp95_ratio = 0,
        mean_residence_sum = 0,
        mean_detcount_sum = 0,
        mean_depth_spawn = NA,
        cv_station_count = NA,
        cv_residence = NA,
        mean_length = NA,
        sd_length = NA,
        prop_spawners = 0,
        mean_total_detections = 0
      )
      
      site_metrics <- bind_rows(site_metrics, empty_metrics)
    }
  }
  
  return(site_metrics)
}

### Calculate site-level behavioral metrics
#----------------------------#
cat("Calculating site-level behavioral metrics...\n")

# Get site coordinates from habitat data
site_coords <- data_habfish_pseudo.train %>% 
  select(Start_Longitude, Start_Latitude, Year) %>% 
  distinct() %>% 
  mutate(site_id = row_number())

# Calculate behavioral metrics for each site
temp_site_behavioral_metrics <- calc_site_behavioral_metrics(
  behavior_data = df_behaviour,
  spawn_data = df_spawn,
  site_coords = site_coords
)

### Integrate with habitat and electrofishing data
#----------------------------#
cat("Integrating behavioral metrics with habitat and electrofishing data...\n")

# Create comprehensive integrated dataset
integrated_data <- data_habfish_pseudo.train %>% 
  mutate(site_id = row_number()) %>% 
  left_join(temp_site_behavioral_metrics, by = "site_id") %>% 
  # Clean up coordinate columns
  mutate(
    Start_Longitude = coalesce(Start_Longitude, site_lon),
    Start_Latitude = coalesce(Start_Latitude, site_lat),
    Year = coalesce(Year, site_year)
  ) %>% 
  select(-site_lon, -site_lat, -site_year) %>% 
  # Replace NAs in behavioral metrics with 0 (sites with no telemetry fish)
  mutate(
    across(c(n_fish, mean_station_count:mean_total_detections), ~ ifelse(is.na(.), 0, .))
  )

### Create behavioral categories
#----------------------------#
cat("Creating behavioral categories...\n")

integrated_data <- integrated_data %>% 
  mutate(
    # Site fidelity categories
    fidelity_category = case_when(
      n_fish == 0 ~ "No telemetry fish",
      mean_station_ratio >= 0.7 ~ "High fidelity",
      mean_station_ratio >= 0.4 ~ "Moderate fidelity", 
      TRUE ~ "Low fidelity"
    ),
    
    # Spawning intensity categories  
    spawning_intensity = case_when(
      n_fish == 0 ~ "No telemetry fish",
      mean_residence_sum >= 100 ~ "High intensity",
      mean_residence_sum >= 50 ~ "Moderate intensity",
      mean_residence_sum > 0 ~ "Low intensity",
      TRUE ~ "No spawning"
    ),
    
    # Home range categories
    home_range_category = case_when(
      n_fish == 0 ~ "No telemetry fish",
      mean_mcp95 >= 500000 ~ "Large range",  # >0.5 km²
      mean_mcp95 >= 100000 ~ "Moderate range",  # 0.1-0.5 km²
      mean_mcp95 > 0 ~ "Small range",
      TRUE ~ "No range data"
    )
  )

### Analysis 1: Behavioral metrics vs habitat characteristics
#----------------------------#
cat("Analyzing behavioral metrics vs habitat characteristics...\n")

# Select key habitat variables for correlation
param_habitat_vars <- c("Slope", "SubstrateDiversity", "Fetch", "Light", 
                  "Sand", "Gravel", "Cobble", "Rubble")

param_behavioral_vars <- c("mean_station_ratio", "mean_residence_sum", "mean_detcount_sum", 
                    "mean_mcp95", "prop_spawners", "n_fish")

# Calculate correlation matrix
temp_sites_with_fish <- integrated_data %>% filter(n_fish > 0)

if(nrow(temp_sites_with_fish) > 5) {
  temp_cor_matrix <- temp_sites_with_fish %>% 
    select(all_of(param_habitat_vars), all_of(param_behavioral_vars)) %>% 
    cor(use = "complete.obs")
  
  # Plot correlation matrix
  png("04 - Outputs/behavior_habitat_correlation.png", 
      width = 12, height = 10, units = "in", res = 300)
  corrplot::corrplot(temp_cor_matrix, 
           method = "color", 
           type = "upper",
           title = "Behavioral Metrics vs Habitat Characteristics",
           mar = c(0,0,2,0),
           tl.col = "black", tl.cex = 0.8,
           col = grDevices::colorRampPalette(c("blue", "white", "red"))(100))
  dev.off()
}

### Analysis 2: Enhanced habitat models with behavioral predictors
#----------------------------#
cat("Building enhanced habitat models with behavioral predictors...\n")

# Model 1: Traditional habitat-only model
temp_model_habitat_only <- randomForest::randomForest(
  Total.Count ~ Slope + SubstrateDiversity + Fetch + Light + 
    Sand + Gravel + Cobble + Rubble + Year,
  data = integrated_data,
  importance = TRUE, ntree = 1000
)

# Model 2: Habitat + behavioral metrics model
temp_model_integrated <- randomForest::randomForest(
  Total.Count ~ Slope + SubstrateDiversity + Fetch + Light + 
    Sand + Gravel + Cobble + Rubble + Year +
    mean_station_ratio + mean_residence_sum + prop_spawners + 
    mean_mcp95_ratio + n_fish,
  data = integrated_data,
  importance = TRUE, ntree = 1000
)

# Model 3: Behavioral-only model
temp_model_behavioral_only <- randomForest::randomForest(
  Total.Count ~ mean_station_ratio + mean_residence_sum + prop_spawners + 
    mean_mcp95_ratio + n_fish + mean_depth_spawn + mean_total_detections,
  data = integrated_data %>% filter(n_fish > 0),
  importance = TRUE, ntree = 1000
)

# Compare model performance
temp_model_comparison <- tibble(
  Model = c("Habitat Only", "Integrated", "Behavioral Only"),
  R2 = c(
    tail(temp_model_habitat_only$rsq, 1),
    tail(temp_model_integrated$rsq, 1),
    tail(temp_model_behavioral_only$rsq, 1)
  ),
  MSE = c(
    tail(temp_model_habitat_only$mse, 1),
    tail(temp_model_integrated$mse, 1),
    tail(temp_model_behavioral_only$mse, 1)
  )
)

print("Model Performance Comparison:")
print(temp_model_comparison)

### Analysis 3: Site fidelity and spawning success relationship
#----------------------------#
cat("Analyzing site fidelity and spawning success relationships...\n")

temp_fidelity_analysis <- integrated_data %>% 
  filter(n_fish > 0) %>% 
  mutate(
    spawning_success = case_when(
      Total.Count >= 3 ~ "High success",
      Total.Count >= 1 ~ "Moderate success",
      TRUE ~ "Low success"
    )
  ) %>% 
  group_by(fidelity_category, spawning_success) %>% 
  summarise(
    n_sites = n(),
    mean_efish_count = mean(Total.Count),
    mean_residence = mean(mean_residence_sum),
    mean_station_ratio = mean(mean_station_ratio),
    .groups = "drop"
  )

# Statistical test for fidelity-success relationship
if(nrow(temp_sites_with_fish) > 10) {
  temp_fidelity_model <- lm(Total.Count ~ mean_station_ratio + mean_residence_sum + 
                      SubstrateDiversity + Gravel, 
                      data = temp_sites_with_fish)
  
  temp_fidelity_summary <- summary(temp_fidelity_model)
  print("Site Fidelity Model Summary:")
  print(temp_fidelity_summary)
}

### Visualization 1: Behavioral metrics by electrofishing success
#----------------------------#
cat("Creating behavioral visualizations...\n")

# Multi-panel plot of behavioral metrics
temp_behavior_plot <- temp_sites_with_fish %>% 
  mutate(efish_category = cut(Total.Count, breaks = c(-1, 0, 1, 2, Inf), 
                             labels = c("None", "Low (1)", "Moderate (2)", "High (3+)"))) %>% 
  select(efish_category, mean_station_ratio, mean_residence_sum, 
         mean_mcp95_ratio, prop_spawners) %>% 
  pivot_longer(cols = -efish_category, names_to = "metric", values_to = "value") %>% 
  mutate(
    metric = recode(metric,
      "mean_station_ratio" = "Site Fidelity Ratio",
      "mean_residence_sum" = "Total Residence Time", 
      "mean_mcp95_ratio" = "Spawning Range Ratio",
      "prop_spawners" = "Proportion Spawners"
    )
  ) %>% 
  ggplot(aes(x = efish_category, y = value, fill = efish_category)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Telemetry Behavioral Metrics by Electrofishing Success",
    subtitle = "Comparison of spawning behaviors across sites with different walleye capture rates",
    x = "Electrofishing Category",
    y = "Behavioral Metric Value",
    fill = "Electrofishing\nCategory"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_viridis_d()

ggsave("04 - Outputs/behavioral_metrics_by_efish_success.png", temp_behavior_plot, 
       width = 12, height = 10, dpi = 300)

### Visualization 2: Variable importance comparison
#----------------------------#
temp_importance_comparison <- tibble(
  Variable = c(rownames(importance(temp_model_habitat_only)), 
               rownames(importance(temp_model_integrated))),
  Importance = c(importance(temp_model_habitat_only)[,1], 
                importance(temp_model_integrated)[,1]),
  Model = c(rep("Habitat Only", nrow(importance(temp_model_habitat_only))),
           rep("Integrated", nrow(importance(temp_model_integrated))))
) %>% 
  arrange(desc(Importance)) %>% 
  mutate(Variable = fct_reorder(Variable, Importance))

temp_importance_plot <- temp_importance_comparison %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Model)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(
    title = "Variable Importance: Habitat vs Integrated Models",
    subtitle = "Comparison of predictor importance in random forest models",
    x = "Variable Importance (%IncMSE)",
    y = "Predictor Variables",
    fill = "Model Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_fill_viridis_d()

ggsave("04 - Outputs/variable_importance_comparison.png", temp_importance_plot, 
       width = 12, height = 8, dpi = 300)

### Visualization 3: Site-level behavioral patterns
#----------------------------#
temp_site_map_plot <- integrated_data %>% 
  filter(n_fish > 0) %>% 
  ggplot(aes(x = Start_Longitude, y = Start_Latitude)) +
  geom_point(aes(size = Total.Count, color = mean_station_ratio), alpha = 0.7) +
  scale_size_continuous(name = "Electrofishing\nCount", range = c(1, 8)) +
  scale_color_viridis_c(name = "Site Fidelity\nRatio") +
  labs(
    title = "Spatial Distribution of Behavioral Patterns",
    subtitle = "Site fidelity and spawning success across Hamilton Harbour",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )

ggsave("04 - Outputs/spatial_behavioral_patterns.png", temp_site_map_plot, 
       width = 12, height = 8, dpi = 300)

### Export integrated dataset and results
#----------------------------#
cat("Exporting integrated dataset and results...\n")

# Export main integrated dataset
write.csv(integrated_data, "04 - Outputs/integrated_behavior_habitat_efish.csv", row.names = FALSE)

# Export behavioral analysis results
write.csv(temp_fidelity_analysis, "04 - Outputs/site_fidelity_analysis.csv", row.names = FALSE)
write.csv(temp_model_comparison, "04 - Outputs/model_performance_comparison.csv", row.names = FALSE)

# Export behavioral summaries by category
temp_behavioral_summaries <- integrated_data %>% 
  group_by(fidelity_category, spawning_intensity) %>% 
  summarise(
    n_sites = n(),
    mean_efish_count = mean(Total.Count),
    mean_gravel = mean(Gravel, na.rm = TRUE),
    mean_substrate_diversity = mean(SubstrateDiversity, na.rm = TRUE),
    mean_slope = mean(Slope, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(temp_behavioral_summaries, "04 - Outputs/behavioral_category_summaries.csv", row.names = FALSE)

# Save model objects
saveRDS(temp_model_habitat_only, "04 - Outputs/model_habitat_only.rds")
saveRDS(temp_model_integrated, "04 - Outputs/model_integrated.rds")
saveRDS(temp_model_behavioral_only, "04 - Outputs/model_behavioral_only.rds")

### Summary statistics
#----------------------------#
cat("\n=== BEHAVIORAL INTEGRATION ANALYSIS SUMMARY ===\n")
cat("Integrated", nrow(integrated_data), "sites with behavioral metrics\n")
cat("Sites with telemetry fish:", sum(integrated_data$n_fish > 0), "\n")
cat("Mean fish per site (telemetry sites only):", round(mean(temp_sites_with_fish$n_fish), 2), "\n")
cat("Sites with spawning behavior:", sum(integrated_data$mean_residence_sum > 0), "\n")

cat("\nModel Performance Improvement:\n")
temp_performance_improvement <- (temp_model_integrated$rsq[length(temp_model_integrated$rsq)] - 
                           temp_model_habitat_only$rsq[length(temp_model_habitat_only$rsq)]) / 
                           temp_model_habitat_only$rsq[length(temp_model_habitat_only$rsq)] * 100
cat("R² improvement with behavioral metrics:", round(temp_performance_improvement, 1), "%\n")

cat("\nTop behavioral predictors:\n")
temp_behavioral_importance <- importance(temp_model_integrated)[grep("mean_|prop_|n_fish", 
                                                           rownames(importance(temp_model_integrated))), 1]
print(sort(temp_behavioral_importance, decreasing = TRUE))

cat("\nAnalysis complete. Check '04 - Outputs/' folder for detailed results and visualizations.\n")

### Clean up temporary objects
#----------------------------#
cat("Cleaning up temporary objects...\n")

# Remove all temp_ objects
rm(list = ls(pattern = "^temp_"))

# Remove other objects created in this script
rm(param_spatial_buffer, param_temporal_buffer, param_habitat_vars, param_behavioral_vars)
rm(site_coords, integrated_data, calc_site_behavioral_metrics)

cat("Cleanup complete.\n")