## --------------------------------------------------------------#
## Script name: Script10-3_Claude_t_spawning_summary.R
##
## Purpose of script:
##    Calculate the proportion of detections that are spawning-related
##    for each fish and year. Based on anti_join logic from
##    Script2-X-1_process_t_m_dataset.R
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2025-01-03
##
## --------------------------------------------------------------#
## Modification Notes:
##   - Simple calculation of spawning proportion per fish per year
##   - Summary statistics only, no individual consistency analysis
## --------------------------------------------------------------#



#####Load Packages and Examine Data #############################----
#-------------------------------------------------------------#

### Load required packages
#----------------------------#
library(tidyverse)

### Examine data structure
#----------------------------#
cat("=== SPAWNING PROPORTION ANALYSIS ===\n")

# Check data availability
cat("data_det dimensions:", dim(data_det), "\n")
cat("df_spawn dimensions:", dim(df_spawn), "\n")
cat("data_spawn dimensions:", dim(data_spawn), "\n")



#####Calculate Spawning Proportions ##############################----
#-------------------------------------------------------------#

### Create spawning proportion dataset
#----------------------------#
cat("\n=== CALCULATING SPAWNING PROPORTIONS ===\n")

# Prepare spawning detection data (individual detections during spawning events)
temp_spawn_detections <- data_spawn %>%
  select(animal_id, detection_timestamp_utc, station_no, moveID) %>%
  mutate(
    year = year(detection_timestamp_utc),
    spawning = 1
  )

# Prepare all detection data
temp_all_detections <- data_det %>%
  select(animal_id, detection_timestamp_utc, station_no) %>%
  mutate(
    year = year(detection_timestamp_utc)
  )

cat("Total detections in data_det:", nrow(temp_all_detections), "\n")
cat("Spawning detections in data_spawn:", nrow(temp_spawn_detections), "\n")

# Join to identify spawning vs non-spawning detections
temp_detection_classification <- temp_all_detections %>%
  left_join(
    select(temp_spawn_detections, animal_id, detection_timestamp_utc, station_no, spawning),
    by = c("animal_id", "detection_timestamp_utc", "station_no")
  ) %>%
  mutate(
    spawning = ifelse(is.na(spawning), 0, spawning)
  )

cat("Total classified detections:", nrow(temp_detection_classification), "\n")
cat("Spawning detections identified:", sum(temp_detection_classification$spawning), "\n")
cat("Non-spawning detections:", sum(temp_detection_classification$spawning == 0), "\n")

### Calculate proportions by fish and year
#----------------------------#
temp_fish_year_proportions <- temp_detection_classification %>%
  group_by(animal_id, year) %>%
  summarise(
    total_detections = n(),
    spawning_detections = sum(spawning),
    non_spawning_detections = sum(spawning == 0),
    spawning_proportion = spawning_detections / total_detections,
    .groups = 'drop'
  ) %>%
  # Filter for adequate sample sizes
  filter(total_detections >= 10) %>%
  # Convert year to factor
  mutate(
    animal_id = as.factor(animal_id),
    year = as.factor(year)
  )

cat("\nSpawning proportion summary:\n")
cat("Fish-year combinations:", nrow(temp_fish_year_proportions), "\n")
cat("Unique fish:", n_distinct(temp_fish_year_proportions$animal_id), "\n")
cat("Years represented:", paste(sort(unique(temp_fish_year_proportions$year)), collapse = ", "), "\n")



#####Summary Statistics ###########################################----
#-------------------------------------------------------------#

### Overall summary statistics
#----------------------------#
temp_proportion_summary <- temp_fish_year_proportions %>%
  summarise(
    n_fish_years = n(),
    n_fish = n_distinct(animal_id),
    mean_total_detections = round(mean(total_detections), 1),
    mean_spawning_detections = round(mean(spawning_detections), 1),
    mean_spawning_proportion = round(mean(spawning_proportion), 3),
    median_spawning_proportion = round(median(spawning_proportion), 3),
    min_spawning_proportion = round(min(spawning_proportion), 3),
    max_spawning_proportion = round(max(spawning_proportion), 3),
    sd_spawning_proportion = round(sd(spawning_proportion), 3)
  )

cat("\n=== SPAWNING PROPORTION SUMMARY STATISTICS ===\n")
print(temp_proportion_summary)

# Summary by year
temp_year_summary <- temp_fish_year_proportions %>%
  group_by(year) %>%
  summarise(
    n_fish = n_distinct(animal_id),
    mean_total_detections = round(mean(total_detections), 1),
    mean_spawning_proportion = round(mean(spawning_proportion), 3),
    median_spawning_proportion = round(median(spawning_proportion), 3),
    .groups = 'drop'
  )

cat("\n=== SUMMARY BY YEAR ===\n")
print(temp_year_summary)

### Summary by fish
#----------------------------#
temp_fish_summary <- temp_fish_year_proportions %>%
  group_by(animal_id) %>%
  summarise(
    n_years = n(),
    mean_spawning_proportion = round(mean(spawning_proportion), 3),
    min_spawning_proportion = round(min(spawning_proportion), 3),
    max_spawning_proportion = round(max(spawning_proportion), 3),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_spawning_proportion))

cat("\n=== SUMMARY BY FISH ===\n")
cat("Fish with multiple years:", sum(temp_fish_summary$n_years > 1), "/", nrow(temp_fish_summary), "\n")
cat("\nTop 10 fish by mean spawning proportion:\n")
print(head(temp_fish_summary, 10))



#####Correlation Analysis #########################################----
#-------------------------------------------------------------#

### Correlation with water level
#----------------------------#
cat("\n=== SPAWNING PROPORTION vs WATER LEVEL ===\n")

# Join year summary with water level data
temp_year_waterlevel <- temp_year_summary %>%
  mutate(year_numeric = as.numeric(as.character(year))) %>%
  left_join(
    data_waterlevel %>% mutate(year_numeric = as.numeric(year)),
    by = "year_numeric"
  )

# Calculate correlation
temp_cor_result <- cor.test(
  temp_year_waterlevel$mean_spawning_proportion,
  temp_year_waterlevel$water_level,
  method = "pearson"
)

# Calculate R²
temp_r_squared <- temp_cor_result$estimate^2

# Print results
cat("Pearson correlation coefficient (r):", round(temp_cor_result$estimate, 3), "\n")
cat("R²:", round(temp_r_squared, 3), "\n")
cat("p-value:", round(temp_cor_result$p.value, 4), "\n")
cat("95% CI for r: [", round(temp_cor_result$conf.int[1], 3), ",",
    round(temp_cor_result$conf.int[2], 3), "]\n")


#####Visualization ################################################----
#-------------------------------------------------------------#

### Distribution plots
#----------------------------#
cat("\n=== CREATING SIMPLE PLOTS ===\n")

# Distribution of spawning proportions
plots$advanced$spawning_summary$proportion_dist <- temp_fish_year_proportions %>%
  ggplot(aes(x = spawning_proportion)) +
  geom_histogram(bins = 20, fill = "skyblue", alpha = 0.7, color = "black") +
  labs(
    title = "Distribution of Spawning Proportions",
    subtitle = "Proportion of detections during spawning events per fish-year",
    x = "Spawning Proportion",
    y = "Count (Fish-Year Combinations)",
    caption = paste("n =", nrow(temp_fish_year_proportions), "fish-year observations")
  ) +
  theme_classic()

print(plots$advanced$spawning_summary$proportion_dist)

 

### Year comparison plots
#----------------------------#

### Year comparison with water level
   # Calculate scaling factor for secondary axis
   temp_waterlevel_range <- range(data_waterlevel$water_level, na.rm = TRUE)
   temp_proportion_range <- c(0, 1)
   temp_scale_factor <- diff(temp_proportion_range) / diff(temp_waterlevel_range)
   temp_offset <- temp_proportion_range[1] - temp_waterlevel_range[1] * temp_scale_factor
   
   # Save supporting data for plotting anywhere
   plots$advanced$spawning_summary$support$proportion_by_year_waterlevel <- list(
     data_plot = temp_fish_year_proportions,
     scale_factor = temp_scale_factor,
     offset = temp_offset
   )

### Year comparison with water level
plots$advanced$spawning_summary$proportion_by_year_waterlevel <-
  with(plots$advanced$spawning_summary$support$proportion_by_year_waterlevel, #Look in new plot support list
     ggplot() +
     # Boxplot for spawning proportion
     geom_boxplot(data = data_plot,
                  aes(x = year, y = spawning_proportion),
                  alpha = 0.7, fill = "lightcoral") +
     geom_jitter(data = data_plot,
                 aes(x = year, y = spawning_proportion),
                 alpha = 0.5, width = 0.2) +
     # Line for water level (scaled to proportion axis)
     geom_line(data = data_waterlevel %>%
                 filter(year %in% unique(data_plot$year)),
               aes(x = as.factor(year),
                   y = water_level * scale_factor + offset,
                   group = 1),
               color = "steelblue", linewidth = 1.2) +
     geom_point(data = data_waterlevel %>%
                  filter(year %in% unique(data_plot$year)),
                aes(x = as.factor(year),
                    y = water_level * scale_factor + offset),
                color = "steelblue", size = 3) +
     # Secondary y-axis for water level
     scale_y_continuous(
       name = "Spawning as Proportion of Detections by Fish",
       sec.axis = sec_axis(
         trans = ~ (. - offset) / scale_factor,
         name = "Water Level (m)"
       )
     ) +
     labs(x = "Year") +
     theme_classic() +
     theme(
       axis.title.y.right = element_text(color = "steelblue"),
       axis.text.y.right = element_text(color = "steelblue")
     )#End ggplot
  ) #End with()

print(plots$advanced$spawning_summary$proportion_by_year_waterlevel)


# Basic year comparison
plots$advanced$spawning_summary$proportion_by_year <- 
 with(plots$advanced$spawning_summary$support$proportion_by_year_waterlevel, #Look in new plot support list 
   ggplot(data = data_plot, aes(x = year, y = spawning_proportion)) +
    geom_boxplot(alpha = 0.7, fill = "lightcoral") +
    geom_jitter(alpha = 0.5, width = 0.2) +
    labs(
      title = "Spawning Proportion by Year",
      x = "Year",
      y = "Spawning Proportion"
    ) +
    theme_classic()
 ) #End with
print(plots$advanced$spawning_summary$proportion_by_year)


#####Cleanup ######################################################----
#-------------------------------------------------------------#

rm(list = ls(pattern = "^temp_"))
cat("\nSpawning proportion analysis complete!\n")
