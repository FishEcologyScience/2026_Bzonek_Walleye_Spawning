## --------------------------------------------------------------#
## Script name: Script10-X_Claude_behavioural_classifications
##
## Purpose of script: 
##    Develop enhanced behavioral classifications for walleye spawning
##    Expand beyond current k-means clustering to include temporal, 
##    environmental, and composite behavioral metrics
##
## Author: Paul Bzonek [Claude] 
##
## Date Created: 2025-08-14
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#

### Load required packages
#----------------------------#
library(tidyverse)
# corrplot, factoextra, GGally, plotly, viridis will use :: notation

### Ensure required data objects exist
#----------------------------#
# Check for behavioral data
if(!exists("df_behaviour")) {
  cat("Loading behavioral data from Script3-2_CombinedBehaviours.R...\n")
  source("02 - Scripts/Script3-2_CombinedBehaviours.R")
}

### 1. TEMPORAL SPAWNING CONSISTENCY CLASSIFICATION
#----------------------------#
cat("Creating temporal spawning consistency classification...\n")

#' Calculate multi-year spawning patterns and consistency
temp_temporal_spawning <- df_behaviour %>%
  group_by(animal_id) %>%
  summarise(
    # Basic temporal metrics
    years_tracked = n(),
    years_spawned = sum(SpawnBinary),
    spawning_consistency = years_spawned / years_tracked,
    
    # Spawning intensity metrics
    mean_spawn_intensity = mean(detcount_sum, na.rm = TRUE),
    max_spawn_intensity = max(detcount_sum, na.rm = TRUE),
    cv_spawn_intensity = if(mean(detcount_sum, na.rm = TRUE) > 0) {
      sd(detcount_sum, na.rm = TRUE) / mean(detcount_sum, na.rm = TRUE)
    } else NA,
    
    # Spawning site metrics
    mean_spawn_stations = mean(station_count_spawn, na.rm = TRUE),
    mean_spawn_residence = mean(residence_sum, na.rm = TRUE),
    
    # Fish characteristics
    mean_length = mean(length_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Primary spawning classification
    spawner_type = case_when(
      spawning_consistency >= 0.8 ~ "Consistent Spawner",
      spawning_consistency >= 0.5 ~ "Periodic Spawner", 
      spawning_consistency > 0 ~ "Occasional Spawner",
      TRUE ~ "Non-Spawner"
    ) %>% factor(levels = c("Non-Spawner", "Occasional Spawner", 
                           "Periodic Spawner", "Consistent Spawner")),
    
    # Secondary intensity classification (for spawners only)
    intensity_type = case_when(
      spawner_type == "Non-Spawner" ~ "Non-Spawner",
      mean_spawn_intensity >= quantile(mean_spawn_intensity, 0.75, na.rm = TRUE) ~ "High Intensity",
      mean_spawn_intensity >= quantile(mean_spawn_intensity, 0.25, na.rm = TRUE) ~ "Moderate Intensity",
      TRUE ~ "Low Intensity"
    ) %>% factor(levels = c("Non-Spawner", "Low Intensity", 
                           "Moderate Intensity", "High Intensity")),
    
    # Combined classification
    temporal_class = paste(spawner_type, intensity_type, sep = " - ")
  )

cat("Temporal spawning classification summary:\n")
print(table(temp_temporal_spawning$spawner_type))
print(table(temp_temporal_spawning$temporal_class))

### VISUALIZATION: Temporal Spawning Results
#----------------------------#
cat("Creating temporal spawning consistency visualization...\n")

# Plot 1: Spawning consistency distribution by type
temp_consistency_plot <- temp_temporal_spawning %>%
  ggplot(aes(x = spawner_type, y = spawning_consistency)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  labs(
    title = "Spawning Consistency by Type",
    x = "Spawner Type",
    y = "Consistency",
    fill = "Spawner Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  )


# Plot 2: Spawning intensity vs consistency
temp_intensity_consistency_plot <- temp_temporal_spawning %>%
  filter(spawner_type != "Non-Spawner") %>%
  ggplot(aes(x = spawning_consistency, y = mean_spawn_intensity, 
             fill = spawner_type, size = years_tracked)) +
  geom_point(alpha = 0.4, shape=21) +
  scale_fill_viridis_d(option = "cividis") +
  scale_size_continuous(range = c(2, 5)) +
  labs(
    title = "Intensity vs Consistency",
    x = "Consistency",
    y = "Mean Intensity",
    fill = "Type",
    size = "Years"
  ) +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

# Plot 3: Fish count by combined temporal classification
temp_classification_counts <- temp_temporal_spawning %>%
  count(spawner_type, intensity_type) %>%
  filter(spawner_type != "Non-Spawner") %>%
  ggplot(aes(x = spawner_type, y = n, fill = fct_rev(intensity_type))) +
  geom_col(alpha = 0.8, color = "black", linewidth = 0.5) +
  scale_fill_viridis_d(option = "rocket", direction=-1, begin=0.2, end = 0.8) +
  labs(
    title = "Fish Count by Classification",
    x = "Spawner Type",
    y = "Count",
    fill = "Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

# Plot 4: Years tracked vs spawning metrics
temp_years_metrics_plot <- temp_temporal_spawning %>%
  filter(years_tracked >= 2) %>%
  ggplot(aes(x = factor(years_tracked), y = cv_spawn_intensity)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA, fill = "lightgray") +
  geom_point(aes(fill = spawner_type), position = position_jitter(width = 0.2), 
             alpha = 0.6, size = 2, shape=21) +
  scale_fill_viridis_d(option = "cividis") +
  labs(
    title = "Variability by Years Tracked",
    x = "Years Tracked",
    y = "CV Intensity",
    color = "Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

# Combine plots using patchwork
temp_temporal_combined_plot <- (temp_consistency_plot + temp_years_metrics_plot) /
                               (temp_classification_counts + temp_intensity_consistency_plot)+
                                plot_layout(guides = "collect")
 

print(temp_temporal_combined_plot)

# ggsave("04 - Outputs/temporal_spawning_analysis.png", temp_temporal_combined_plot,
#        width = 16, height = 12, dpi = 300)




### 2. SITE FIDELITY vs EXPLORATION BEHAVIORAL AXIS
#----------------------------#
cat("Creating site fidelity vs exploration behavioral classification...\n")

#' Create composite behavioral metrics and 2D behavioral space
temp_behavioral_axis <- df_behaviour %>%
  filter(!is.na(station_count), !is.na(mcp95)) %>%
  # Remove extreme MCP outlier if present (value = 0.0273126541997)
  # This outlier skews the exploration index and behavioral classifications
  {if(any(abs(.$mcp95 - 0.0273126541997) < 1e-10, na.rm = TRUE)) {
    cat("Removing MCP outlier (0.0273126541997) for improved behavioral classification accuracy...\n")
    filter(., abs(mcp95 - 0.0273126541997) >= 1e-10)
  } else {
    cat("MCP outlier not found in dataset, proceeding with all data...\n")
    .
  }} %>%
  mutate(
    # Log transform skewed variables before normalizing
    log_residence_sum = log(residence_sum + 1),  # +1 to handle zeros
    log_mcp95 = log(mcp95 + 1),  # +1 to handle zeros
    
    # Normalize metrics to 0-1 scale for composite indices
    station_count_norm = (station_count - min(station_count, na.rm = TRUE)) / 
                        (max(station_count, na.rm = TRUE) - min(station_count, na.rm = TRUE)),
    mcp95_norm = (log_mcp95 - min(log_mcp95, na.rm = TRUE)) / 
                 (max(log_mcp95, na.rm = TRUE) - min(log_mcp95, na.rm = TRUE)),
    residence_sum_norm = (log_residence_sum - min(log_residence_sum, na.rm = TRUE)) / 
                        (max(log_residence_sum, na.rm = TRUE) - min(log_residence_sum, na.rm = TRUE)),
    station_count_ratio_norm = station_count_ratio,  # Already a ratio
    
    # Site fidelity index: high spawning site concentration + high residence
    site_fidelity_index = (station_count_ratio_norm * 2) + residence_sum_norm,
    
    # Exploration index: high total stations + high spatial area
    exploration_index = station_count_norm + mcp95_norm,
    
    # Spawning intensity index (for spawners only)
    spawn_intensity_index = if_else(SpawnBinary, 
                                   (detcount_sum / max(detcount_sum, na.rm = TRUE)) * 
                                   residence_sum_norm, 
                                   0)
  ) %>%
  # Add spawning history for behavioral classification
  group_by(animal_id) %>%
  mutate(ever_spawned = any(SpawnBinary == TRUE, na.rm = TRUE)) %>%
  ungroup() %>%
  # Create 2D behavioral classification
  mutate(
    # Top two-thirds vs median splits for classification
    high_fidelity = site_fidelity_index > quantile(site_fidelity_index, 1/3, na.rm = TRUE),
    high_exploration = exploration_index > median(exploration_index, na.rm = TRUE),
    
    behavioral_type = case_when(
      !ever_spawned ~ "Never Spawn",
      high_fidelity & high_exploration ~ "Faithful Explorer",
      high_fidelity & !high_exploration ~ "Site Specialist", 
      !high_fidelity & high_exploration ~ "Roaming Explorer",
      TRUE ~ "Low Activity"
    ) %>% factor(levels = c("Never Spawn", "Low Activity", "Site Specialist", 
                           "Roaming Explorer", "Faithful Explorer"))
  ) %>%
  # Add tertile-based refined classification
  mutate(
    fidelity_tertile = ntile(site_fidelity_index, 3),
    exploration_tertile = ntile(exploration_index, 3),
    
    refined_behavioral_type = case_when(
      fidelity_tertile == 3 & exploration_tertile == 3 ~ "High Fidelity-High Exploration",
      fidelity_tertile == 3 & exploration_tertile == 2 ~ "High Fidelity-Moderate Exploration", 
      fidelity_tertile == 3 & exploration_tertile == 1 ~ "High Fidelity-Low Exploration",
      fidelity_tertile == 2 & exploration_tertile == 3 ~ "Moderate Fidelity-High Exploration",
      fidelity_tertile == 2 & exploration_tertile == 2 ~ "Moderate Fidelity-Moderate Exploration",
      fidelity_tertile == 2 & exploration_tertile == 1 ~ "Moderate Fidelity-Low Exploration",
      fidelity_tertile == 1 & exploration_tertile == 3 ~ "Low Fidelity-High Exploration", 
      fidelity_tertile == 1 & exploration_tertile == 2 ~ "Low Fidelity-Moderate Exploration",
      TRUE ~ "Low Fidelity-Low Exploration"
    )
  )

cat("Site fidelity vs exploration classification summary:\n")
print(table(temp_behavioral_axis$behavioral_type))

### VISUALIZATION: Site Fidelity vs Exploration Results
#----------------------------#
cat("Creating site fidelity vs exploration behavioral visualizations...\n")

# Plot 1: Behavioral type distribution by site fidelity index
temp_fidelity_plot <- temp_behavioral_axis %>%
  ggplot(aes(x = behavioral_type, y = site_fidelity_index)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA, fill = "lightgray") +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  labs(
    title = "Site Fidelity by Behavioral Type",
    x = "Behavioral Type",
    y = "Site Fidelity Index"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  )

# Plot 2: Exploration vs site fidelity scatter
temp_behavioral_space_plot <- temp_behavioral_axis %>%
  ggplot(aes(x = exploration_index, y = site_fidelity_index, 
             fill = behavioral_type, size = spawn_intensity_index, 
             shape = ever_spawned)) +
  geom_point(alpha = 0.4) +
  scale_fill_viridis_d(option = "viridis") +
  scale_shape_manual(values = c("TRUE" = 21, "FALSE" = 17), 
                     labels = c("TRUE" = "Spawner", "FALSE" = "Never Spawner")) +
  scale_size_continuous(range = c(1, 4)) +
  labs(
    title = "Behavioral Space",
    x = "Exploration Index",
    y = "Site Fidelity Index",
    fill = "Type",
    shape = "Spawn Status",
    size = "Spawn Intensity"
  ) +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

# Plot 3: Fish count by behavioral classification
temp_behavioral_counts <- temp_behavioral_axis %>%
  count(behavioral_type, SpawnBinary) %>%
  mutate(spawn_status = if_else(SpawnBinary, "Spawner", "Non-Spawner")) %>%
  ggplot(aes(x = behavioral_type, y = n, fill = fct_rev(spawn_status))) +
  geom_col(alpha = 0.8, color = "black", linewidth = 0.5) +
  scale_fill_viridis_d(option = "rocket", direction = -1, begin = 0.2, end = 0.8) +
  labs(
    title = "Fish Count by Behavioral Type",
    x = "Behavioral Type",
    y = "Count",
    fill = "Spawn Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

# Plot 4: Spawning intensity by behavioral type
temp_spawn_by_behavior_plot <- temp_behavioral_axis %>%
  filter(SpawnBinary == TRUE) %>%
  ggplot(aes(x = factor(fidelity_tertile), y = spawn_intensity_index)) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA, fill = "lightgray") +
  geom_point(aes(fill = behavioral_type), position = position_jitter(width = 0.2), 
             alpha = 0.6, size = 2, shape = 21) +
  scale_fill_viridis_d(option = "viridis") +
  labs(
    title = "Spawn Intensity by Fidelity Level",
    x = "Site Fidelity Tertile",
    y = "Spawn Intensity Index"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

# Plot 5: Site Fidelity Index Components
temp_fidelity_components_plot <- temp_behavioral_axis %>%
  ggplot(aes(x = station_count_ratio_norm, y = residence_sum_norm, 
             color = site_fidelity_index)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_viridis_c(option = "viridis") +
  labs(
    title = "Site Fidelity Index Components",
    x = "Station Count Ratio (normalized)",
    y = "Residence Sum (normalized)",
    color = "Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

# Plot 6: Exploration Index Components  
temp_exploration_components_plot <- temp_behavioral_axis %>%
  ggplot(aes(x = station_count_norm, y = mcp95_norm, 
             color = exploration_index)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_viridis_c(option = "viridis") +
  labs(
    title = "Exploration Index Components",
    x = "Station Count (normalized)",
    y = "MCP95 Area (normalized)",
    color = "Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

# Main 4-panel combined plot
temp_behavioral_combined_plot <- (temp_fidelity_plot + temp_spawn_by_behavior_plot) /
                                 (temp_behavioral_counts + temp_behavioral_space_plot) +
                                 plot_layout(guides = "collect")

# Components 2-panel plot
temp_behavioral_components_plot <- (temp_fidelity_components_plot + temp_exploration_components_plot) +
                                   plot_layout(guides = "collect")

print(temp_behavioral_combined_plot)
print(temp_behavioral_components_plot)

# ggsave("04 - Outputs/behavioral_axis_analysis.png", temp_behavioral_combined_plot,
#        width = 16, height = 12, dpi = 300)
# ggsave("04 - Outputs/behavioral_index_components.png", temp_behavioral_components_plot,
#        width = 12, height = 6, dpi = 300)

### INTEGRATE ALL CLASSIFICATIONS
#----------------------------#
cat("Integrating all behavioral classifications...\n")

# Combine all classifications into master dataset
temp_integrated_classifications <- df_behaviour %>%
  select(animal_id, year, SpawnBinary, length_total, water_level, station_count:detcount_sum) %>%
  left_join(
    temp_temporal_spawning %>% select(animal_id, spawner_type, intensity_type, temporal_class, spawning_consistency),
    by = "animal_id"
  ) %>%
  left_join(
    temp_behavioral_axis %>% select(animal_id, year, behavioral_type, refined_behavioral_type, 
                                   site_fidelity_index, exploration_index),
    by = c("animal_id", "year")
  )

### VISUALIZATION 1: Temporal Spawning Patterns
#----------------------------#
cat("Creating temporal spawning visualizations...\n")

temp_temporal_plot <- temp_temporal_spawning %>%
  ggplot(aes(x = spawning_consistency, y = mean_spawn_intensity, color = spawner_type, size = years_tracked)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d() +
  labs(
    title = "Temporal Spawning Consistency vs Intensity",
    subtitle = "Each point represents one fish across all tracked years",
    x = "Spawning Consistency (proportion of years spawned)",
    y = "Mean Spawning Intensity (detection count)",
    color = "Spawner Type",
    size = "Years Tracked"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# ggsave("04 - Outputs/temporal_spawning_patterns.png", temp_temporal_plot, 
#        width = 12, height = 8, dpi = 300)

### VISUALIZATION 2: Site Fidelity vs Exploration
#----------------------------#
temp_behavioral_space_plot <- temp_behavioral_axis %>%
  ggplot(aes(x = exploration_index, y = site_fidelity_index, 
             color = behavioral_type, shape = ever_spawned)) +
  geom_point(alpha = 0.7, size = 2) +
  stat_ellipse(level = 0.68) +
  scale_color_viridis_d() +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 17), 
                     labels = c("TRUE" = "Spawner", "FALSE" = "Never Spawner")) +
  labs(
    title = "Behavioral Space: Site Fidelity vs Exploration",
    subtitle = "Each point represents one fish-year combination",
    x = "Exploration Index (stations + spatial range)",
    y = "Site Fidelity Index (spawning site concentration + residence)",
    color = "Behavioral Type",
    shape = "Spawn Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# ggsave("04 - Outputs/behavioral_space_plot.png", temp_behavioral_space_plot,
#        width = 12, height = 8, dpi = 300)

### SUMMARY STATISTICS AND EXPORT
#----------------------------#
cat("Generating summary statistics...\n")

# Summary table of all classifications
temp_classification_summary <- temp_integrated_classifications %>%
  group_by(animal_id) %>%
  slice(1) %>%  # One row per fish
  ungroup() %>%
  summarise(
    total_fish = n(),
    
    # Temporal patterns
    consistent_spawners = sum(spawner_type == "Consistent Spawner", na.rm = TRUE),
    periodic_spawners = sum(spawner_type == "Periodic Spawner", na.rm = TRUE),
    occasional_spawners = sum(spawner_type == "Occasional Spawner", na.rm = TRUE),
    non_spawners = sum(spawner_type == "Non-Spawner", na.rm = TRUE),
    
    # Behavioral types
    never_spawn = sum(behavioral_type == "Never Spawn", na.rm = TRUE),
    low_activity = sum(behavioral_type == "Low Activity", na.rm = TRUE),
    site_specialists = sum(behavioral_type == "Site Specialist", na.rm = TRUE),
    roaming_explorers = sum(behavioral_type == "Roaming Explorer", na.rm = TRUE),
    faithful_explorers = sum(behavioral_type == "Faithful Explorer", na.rm = TRUE)
  )

print("=== ENHANCED BEHAVIORAL CLASSIFICATION SUMMARY ===")
print(temp_classification_summary)

# ### Export enhanced behavioral datasets
# #----------------------------#
# cat("Exporting enhanced behavioral classification results...\n")
# 
# # Main integrated dataset
# write.csv(temp_integrated_classifications, 
#           "04 - Outputs/enhanced_behavioral_classifications.csv", 
#           row.names = FALSE)
# 
# # Temporal spawning patterns
# write.csv(temp_temporal_spawning, 
#           "04 - Outputs/temporal_spawning_patterns.csv", 
#           row.names = FALSE)
# 
# # Behavioral axis data
# write.csv(temp_behavioral_axis, 
#           "04 - Outputs/site_fidelity_exploration_axis.csv", 
#           row.names = FALSE)
# 
# # Summary statistics
# write.csv(temp_classification_summary, 
#           "04 - Outputs/behavioral_classification_summary.csv", 
#           row.names = FALSE)
# 
# cat("\n=== ENHANCED BEHAVIORAL ANALYSIS COMPLETE ===\n")
# cat("Created 2 new behavioral classification systems:\n")
# cat("1. Temporal Spawning Consistency (4 types)\n")
# cat("2. Site Fidelity vs Exploration Axis (5 main types, 9 refined types)\n") 
# cat("\nResults exported to '04 - Outputs/' folder\n")

# ### Clean up temporary objects
# #----------------------------#
# cat("Cleaning up temporary objects...\n")
# 
# # Remove all temp_ objects
# rm(list = ls(pattern = "^temp"))
# 
# cat("Cleanup complete.\n")
