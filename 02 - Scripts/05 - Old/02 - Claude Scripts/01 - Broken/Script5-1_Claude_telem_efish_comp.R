## --------------------------------------------------------------#
## Script name: Script5-1_Claude_telem_efish_comp
##
## Purpose of script: 
##    Compare spawning and all telemetry data to electrofishing data
##    Test correlation across multiple temporal windows
##    Assess spatial-temporal integration of datasets
##
## Author: Paul Bzonek [Claude] 
##
## Date Created: 2025-08-11
##
## --------------------------------------------------------------#  
## Modification Notes:  PB NOTE: VERY POOR CORRELATION VALUES. THIS IS LIEKLY A DEAD-END
##   
## --------------------------------------------------------------#

### Required packages (using :: notation for non-tidyverse)
#----------------------------#
library(tidyverse)
# sf, geosphere, corrplot, viridis, lubridate will use :: notation

### Set parameters for analysis
#----------------------------#
param_spatial_buffers <- c(25, 50, 100, 150, 200)  # meters
param_temporal_windows <- c(14, 30)  # days
param_week_windows <- 1:10  # weeks from April start

### Load processed datasets (assuming they exist from previous scripts)
#----------------------------#
# Load electrofishing data
temp_data_efish_spatial <- data_habfish_pseudo.train %>% 
  select(deploy_long=lon, deploy_lat=lat, Total.Count, Year) %>% 
  rename(efish_count = Total.Count)

# Load telemetry data - all detections
  temp_telem_all <- data_det %>% 
    select(animal_id, Timestamp, deploy_long, deploy_lat, 
           station) %>% 
    mutate(year = lubridate::year(Timestamp))


# Load spawning/staging telemetry data
  temp_telem_spawn <- data_spawn %>% 
    select(animal_id, Timestamp, deploy_long, deploy_lat, 
           station) %>% 
    mutate(year = lubridate::year(Timestamp))

### Function: Calculate distance matrix between datasets
#----------------------------#
calc_spatial_matches <- function(efish_data, telem_data, buffer_m) {
  # Convert to sf objects
  efish_sf <- sf::st_as_sf(efish_data, coords = c("deploy_long", "deploy_lat"), crs = 4326)
  telem_sf <- sf::st_as_sf(telem_data, coords = c("deploy_long", "deploy_lat"), crs = 4326)
  
  # Transform to projected CRS for accurate distance calculation
  efish_proj <- sf::st_transform(efish_sf, crs = 3857)  # Web Mercator
  telem_proj <- sf::st_transform(telem_sf, crs = 3857)
  
  # Create buffer around electrofishing points
  efish_buffer <- sf::st_buffer(efish_proj, dist = buffer_m)
  
  # Find spatial matches
  matches <- sf::st_intersects(telem_proj, efish_buffer)
  
  # Add match information to telemetry data
  telem_data$efish_match_id <- NA
  for(i in 1:length(matches)) {
    if(length(matches[[i]]) > 0) {
      telem_data$efish_match_id[i] <- matches[[i]][1]  # Take first match
    }
  }
  
  return(telem_data)
}

### Function: Calculate temporal matches
#----------------------------#
calc_temporal_matches <- function(efish_data, telem_data, window_days) {
  matched_data <- list()
  
  for(i in 1:nrow(efish_data)) {
    efish_date <- as.Date(paste(efish_data$Year[i], "04-15", sep = "-"))  # Assume mid-April survey
    
    # Find telemetry data within temporal window
    telem_subset <- telem_data %>% 
      filter(year == efish_data$Year[i]) %>% 
      mutate(days_diff = abs(as.numeric(as.Date(Timestamp) - efish_date))) %>% 
      filter(days_diff <= window_days)
    
    if(nrow(telem_subset) > 0) {
      telem_subset$efish_site_id <- i
      telem_subset$efish_count <- efish_data$efish_count[i]
      matched_data[[i]] <- telem_subset
    }
  }
  
  return(bind_rows(matched_data))
}

### Function: Calculate weekly temporal windows
#----------------------------#
calc_weekly_matches <- function(efish_data, telem_data, week_num) {
  matched_data <- list()
  
  for(year in unique(efish_data$Year)) {
    # Define April start date for each year
    april_start <- as.Date(paste(year, "04-01", sep = "-"))
    week_start <- april_start + lubridate::days((week_num - 1) * 7)
    week_end <- week_start + lubridate::days(6)
    
    # Filter telemetry data for this week and year
    telem_week <- telem_data %>% 
      filter(year == !!year,
             as.Date(Timestamp) >= week_start,
             as.Date(Timestamp) <= week_end)
    
    if(nrow(telem_week) > 0) {
      telem_week$week <- week_num
      telem_week$year_week <- paste(year, sprintf("W%02d", week_num), sep = "_")
      matched_data[[paste(year, week_num)]] <- telem_week
    }
  }
  
  return(bind_rows(matched_data))
}

### Analysis 1: Spatial-temporal correlation matrix
#----------------------------#
cat("Calculating spatial-temporal correlation matrix...\n")

correlation_results <- expand_grid(
  buffer = param_spatial_buffers,
  window = param_temporal_windows,
  dataset = c("all_telem", "spawn_telem")
) %>% 
  mutate(
    correlation = NA,
    n_matches = NA,
    p_value = NA
  )

for(i in 1:nrow(correlation_results)) {
  buffer_m <- correlation_results$buffer[i]
  window_d <- correlation_results$window[i]
  dataset_type <- correlation_results$dataset[i]
  
  # Select appropriate telemetry dataset
  temp_telem_data <- if(dataset_type == "spawn_telem") temp_telem_spawn else temp_telem_all
  
  # Calculate spatial matches
  temp_telem_spatial <- calc_spatial_matches(temp_data_efish_spatial, temp_telem_data, buffer_m)
  
  # Calculate temporal matches
  temp_telem_matched <- calc_temporal_matches(temp_data_efish_spatial, 
                                        temp_telem_spatial %>% filter(!is.na(efish_match_id)), 
                                        window_d)
  
  if(nrow(temp_telem_matched) > 5) {  # Minimum sample size
    # Aggregate telemetry detections by site
    temp_telem_summary <- temp_telem_matched %>% 
      group_by(efish_site_id, efish_count) %>% 
      summarise(
        telem_detections = n(),
        unique_fish = n_distinct(animal_id),
        .groups = "drop"
      )
    
    # Calculate correlation
    if(nrow(temp_telem_summary) > 3) {
      temp_cor_test <- cor.test(temp_telem_summary$efish_count, temp_telem_summary$telem_detections)
      correlation_results$correlation[i] <- temp_cor_test$estimate
      correlation_results$n_matches[i] <- nrow(temp_telem_summary)
      correlation_results$p_value[i] <- temp_cor_test$p.value
    }
  }
}

### Analysis 2: Weekly temporal analysis
#----------------------------#
cat("Calculating weekly temporal patterns...\n")

weekly_results <- tibble()

for(week in param_week_windows) {
  # All telemetry
  temp_telem_all_week <- calc_weekly_matches(temp_data_efish_spatial, temp_telem_all, week)
  
  # Spawning telemetry  
  temp_telem_spawn_week <- calc_weekly_matches(temp_data_efish_spatial, temp_telem_spawn, week)
  
  if(nrow(temp_telem_all_week) > 0 | nrow(temp_telem_spawn_week) > 0) {
    temp_week_summary <- tibble(
      week = week,
      all_telem_detections = nrow(temp_telem_all_week),
      spawn_telem_detections = nrow(temp_telem_spawn_week),
      all_telem_fish = ifelse(nrow(temp_telem_all_week) > 0, n_distinct(temp_telem_all_week$animal_id), 0),
      spawn_telem_fish = ifelse(nrow(temp_telem_spawn_week) > 0, n_distinct(temp_telem_spawn_week$animal_id), 0)
    )
    
    weekly_results <- bind_rows(weekly_results, temp_week_summary)
  }
}

### Analysis 3: Site-level comparison
#----------------------------#
cat("Calculating site-level comparisons...\n")

# Use optimal parameters (you can adjust based on correlation results)
param_optimal_buffer <- 100  # meters
param_optimal_window <- 7    # days

# Create comprehensive site-level dataset
temp_site_comparison <- temp_data_efish_spatial %>% 
  mutate(site_id = row_number()) %>% 
  rowwise() %>% 
  mutate(
    # All telemetry matches
    all_telem_spatial = {
      temp_telem_spatial_all <- calc_spatial_matches(
        temp_data_efish_spatial[site_id, ], temp_telem_all, param_optimal_buffer
      )
      temp_telem_temporal_all <- calc_temporal_matches(
        temp_data_efish_spatial[site_id, ], 
        temp_telem_spatial_all %>% filter(!is.na(efish_match_id)), 
        param_optimal_window
      )
      nrow(temp_telem_temporal_all)
    },
    
    # Spawning telemetry matches
    spawn_telem_spatial = {
      temp_telem_spatial_spawn <- calc_spatial_matches(
        temp_data_efish_spatial[site_id, ], temp_telem_spawn, param_optimal_buffer
      )
      temp_telem_temporal_spawn <- calc_temporal_matches(
        temp_data_efish_spatial[site_id, ], 
        temp_telem_spatial_spawn %>% filter(!is.na(efish_match_id)), 
        param_optimal_window
      )
      nrow(temp_telem_temporal_spawn)
    }
  ) %>% 
  ungroup()

### Visualization 1: Correlation heatmap
#----------------------------#
cat("Creating visualizations...\n")

# Reshape correlation results for heatmap
temp_cor_matrix_all <- correlation_results %>% 
  filter(dataset == "all_telem", !is.na(correlation)) %>% 
  select(buffer, window, correlation) %>% 
  pivot_wider(names_from = window, values_from = correlation, names_prefix = "window_") %>% 
  column_to_rownames("buffer")

temp_cor_matrix_spawn <- correlation_results %>% 
  filter(dataset == "spawn_telem", !is.na(correlation)) %>% 
  select(buffer, window, correlation) %>% 
  pivot_wider(names_from = window, values_from = correlation, names_prefix = "window_") %>% 
  column_to_rownames("buffer")

# Plot correlation heatmaps
if(nrow(temp_cor_matrix_all) > 0) {
  #png("04 - Outputs/correlation_heatmap_all_telemetry.png", 
  #    width = 10, height = 8, units = "in", res = 300)
  corrplot::corrplot(as.matrix(temp_cor_matrix_all), 
           method = "color", 
           title = "Correlation: All Telemetry vs Electrofishing",
           mar = c(0,0,2,0),
           tl.col = "black",
           col = grDevices::colorRampPalette(c("blue", "white", "red"))(100))
  #dev.off()
}

if(nrow(temp_cor_matrix_spawn) > 0) {
  #png("04 - Outputs/correlation_heatmap_spawn_telemetry.png", 
  #    width = 10, height = 8, units = "in", res = 300)
  corrplot::corrplot(as.matrix(temp_cor_matrix_spawn), 
           method = "color", 
           title = "Correlation: Spawning Telemetry vs Electrofishing",
           mar = c(0,0,2,0),
           tl.col = "black",
           col = grDevices::colorRampPalette(c("blue", "white", "red"))(100))
  #dev.off()
}

### Visualization 2: Weekly temporal patterns
#----------------------------#
temp_weekly_plot <- weekly_results %>% 
  pivot_longer(cols = c(all_telem_detections, spawn_telem_detections),
               names_to = "dataset", values_to = "detections") %>% 
  mutate(dataset = ifelse(dataset == "all_telem_detections", 
                         "All Telemetry", "Spawning Telemetry")) %>% 
  ggplot(aes(x = week, y = detections, color = dataset, fill = dataset)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_area(alpha = 0.3) +
  labs(
    title = "Weekly Telemetry Activity During April Spawning Period",
    subtitle = "Comparison of all detections vs spawning-specific detections",
    x = "Week of April",
    y = "Number of Detections",
    color = "Dataset",
    fill = "Dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d()

#ggsave("04 - Outputs/weekly_telemetry_patterns.png", temp_weekly_plot, 
#       width = 12, height = 8, dpi = 300)

### Visualization 3: Site-level comparison
#----------------------------#
temp_site_plot <- temp_site_comparison %>% 
  ggplot(aes(x = efish_count)) +
  geom_point(aes(y = all_telem_spatial), color = "blue", alpha = 0.6, size = 2) +
  geom_point(aes(y = spawn_telem_spatial), color = "red", alpha = 0.6, size = 2) +
  geom_smooth(aes(y = all_telem_spatial), method = "lm", color = "blue", se = TRUE) +
  geom_smooth(aes(y = spawn_telem_spatial), method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Site-Level Correlation: Electrofishing vs Telemetry",
    subtitle = paste("Spatial buffer:", param_optimal_buffer, "m, Temporal window:", param_optimal_window, "days"),
    x = "Electrofishing Count",
    y = "Telemetry Detections",
    caption = "Blue = All Telemetry, Red = Spawning Telemetry"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

#ggsave("04 - Outputs/site_level_correlation.png", temp_site_plot, 
#       width = 10, height = 8, dpi = 300)

### Summary statistics and export
#----------------------------#
cat("Generating summary statistics...\n")

# Best correlation results
best_correlations <- correlation_results %>% 
  filter(!is.na(correlation)) %>% 
  arrange(desc(abs(correlation))) %>% 
  slice_head(n = 10)

# Weekly peak activity
peak_weeks <- weekly_results %>% 
  slice_max(order_by = spawn_telem_detections, n = 3)

# Site-level correlations
temp_site_cors <- temp_site_comparison %>% 
  summarise(
    cor_all_telem = cor(efish_count, all_telem_spatial, use = "complete.obs"),
    cor_spawn_telem = cor(efish_count, spawn_telem_spatial, use = "complete.obs"),
    n_sites = n(),
    sites_with_both = sum(efish_count > 0 & spawn_telem_spatial > 0)
  )

# Export results
#write.csv(correlation_results, "04 - Outputs/telemetry_efishing_correlations.csv", row.names = FALSE)
#write.csv(weekly_results, "04 - Outputs/weekly_telemetry_patterns.csv", row.names = FALSE)
#write.csv(temp_site_comparison, "04 - Outputs/site_level_comparison.csv", row.names = FALSE)

### Print summary
#----------------------------#
cat("\n=== TELEMETRY-ELECTROFISHING INTEGRATION ANALYSIS ===\n")
cat("Best correlations found:\n")
print(best_correlations)
cat("\nPeak spawning weeks:\n")
print(peak_weeks)
cat("\nSite-level correlation summary:\n")
print(temp_site_cors)
cat("\nAnalysis complete. Check '04 - Outputs/' folder for detailed results and plots.\n")

### Clean up temporary objects
#----------------------------#
cat("Cleaning up temporary objects...\n")

# Remove all temp_ objects
rm(list = ls(pattern = "^temp_"))

# Remove other objects created in this script
rm(param_spatial_buffers, param_temporal_windows, param_week_windows, param_optimal_buffer, param_optimal_window)
rm(correlation_results, weekly_results, best_correlations, peak_weeks)
rm(calc_spatial_matches, calc_temporal_matches, calc_weekly_matches)

cat("Cleanup complete.\n")
