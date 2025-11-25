## --------------------------------------------------------------#
## Script name: Script5-5_Claude_behavioral_space_plot_modified
##
## Purpose of script: 
##    Modified version of temp_behavioral_space_plot with animal_id ellipses
##    to show individual fish consistency over years
##
## Author: Paul Bzonek [Claude] 
##
## Date Created: 2025-08-15
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   - Copied from Script5-4 to allow safe modification
##   - Adding animal_id ellipses to show fish consistency over years
## --------------------------------------------------------------#

### Load required packages
#----------------------------#
library(tidyverse)

### Ensure required data objects exist from Script5-4
#----------------------------#
# Check for behavioral data
if(!exists("temp_behavioral_axis")) {
  cat("Loading behavioral data from Script5-4_Claude_behaviour.R...\n")
  source("02 - Scripts/Script5-4_Claude_behaviour.R")
}

### MODIFIED BEHAVIORAL SPACE PLOT
#----------------------------#
cat("Creating modified behavioral space plot with animal_id ellipses...\n")

# Filter animals with enough VALID points for ellipses (need at least 3 complete cases)
temp_animals_with_ellipses <- temp_behavioral_axis %>%
  filter(!is.na(exploration_index), !is.na(site_fidelity_index)) %>%  # Remove NA values
  group_by(animal_id) %>%
  filter(n() >= 3) %>%  # Need at least 3 complete observations
  # Additional check: ensure there's actual variation in the data for ellipse calculation
  filter(var(exploration_index, na.rm = TRUE) > 0, var(site_fidelity_index, na.rm = TRUE) > 0) %>%
  ungroup()

cat(sprintf("Total animals: %d\n", length(unique(temp_behavioral_axis$animal_id))))
cat(sprintf("Animals with valid ellipses (≥3 complete cases with variation): %d\n", length(unique(temp_animals_with_ellipses$animal_id))))

temp_behavioral_space_plot_modified <- temp_behavioral_axis %>%
  ggplot(aes(x = exploration_index, y = site_fidelity_index, 
             color = behavioral_type, shape = ever_spawned)) +
  geom_point(alpha = 0.7, size = 2) +
  stat_ellipse(data = temp_animals_with_ellipses, aes(group = animal_id), 
               level = 0.68, alpha = 0.3, linewidth = 0.5, 
               color = "gray60", linetype = "dashed") +
  scale_color_viridis_d() +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 17), 
                     labels = c("TRUE" = "Spawner", "FALSE" = "Never Spawner")) +
  labs(
    title = "Behavioral Space: Site Fidelity vs Exploration",
    subtitle = "Each point represents one fish-year combination; gray ellipses show individual fish consistency",
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

print(temp_behavioral_space_plot_modified)

# Optional: Save the modified plot
# ggsave("04 - Outputs/behavioral_space_plot_with_fish_ellipses.png", 
#        temp_behavioral_space_plot_modified,
#        width = 12, height = 8, dpi = 300, bg = "white")

cat("Modified behavioral space plot complete!\n")





### ALTERNATIVE VISUALIZATION: Fish Centroids with Radiating Lines
#----------------------------#
cat("Creating behavioral space plot with fish centroids and radiating lines...\n")

# Calculate centroids for each fish (mean position across years)
temp_fish_centroids <- temp_behavioral_axis %>%
  filter(!is.na(exploration_index), !is.na(site_fidelity_index)) %>%
  group_by(animal_id) %>%
  summarise(
    centroid_exploration = mean(exploration_index, na.rm = TRUE),
    centroid_fidelity = mean(site_fidelity_index, na.rm = TRUE),
    n_years = n(),
    # Take behavioral type from most common type for this fish
    behavioral_type = names(sort(table(behavioral_type), decreasing = TRUE))[1],
    ever_spawned = any(ever_spawned == TRUE, na.rm = TRUE),
    .groups = "drop"
   ) #%>%
  # # Only include fish with multiple years for meaningful centroids
  # filter(n_years >= 2)

# Create line segments from centroids to individual points
temp_centroid_lines <- temp_behavioral_axis %>%
  filter(!is.na(exploration_index), !is.na(site_fidelity_index)) %>%
  inner_join(temp_fish_centroids %>% select(animal_id, centroid_exploration, centroid_fidelity), 
             by = "animal_id")

temp_behavioral_space_centroid_plot <- ggplot() +
  # Add lines from centroids to individual points
  geom_segment(data = temp_centroid_lines,
               aes(x = centroid_exploration, y = centroid_fidelity,
                   xend = exploration_index, yend = site_fidelity_index,
                   group = animal_id),
               alpha = 0.3, color = "gray50", linewidth = 0.3) +
  # Add individual annual points
  geom_point(data = temp_behavioral_axis,
             aes(x = exploration_index, y = site_fidelity_index, 
                 color = behavioral_type, shape = ever_spawned),
             alpha = 0.6, size = 1.5) +
  # Add centroids (larger points)
  geom_point(data = temp_fish_centroids,
             aes(x = centroid_exploration, y = centroid_fidelity,
                 color = behavioral_type, shape = ever_spawned),
             size = 3, alpha = 0.9, stroke = 1) +
  scale_color_viridis_d() +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 17), 
                     labels = c("TRUE" = "Spawner", "FALSE" = "Never Spawner")) +
  labs(
    title = "Behavioral Space: Fish Centroids with Annual Variation",
    subtitle = "Large points = fish centroids, small points = annual observations, lines show consistency",
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

print(temp_behavioral_space_centroid_plot)

cat(sprintf("Fish with centroids (≥2 years): %d\n", nrow(temp_fish_centroids)))

# # Optional: Save the centroid plot
# ggsave("04 - Outputs/behavioral_space_plot_with_centroids.png",
#        temp_behavioral_space_centroid_plot,
#        width = 12, height = 8, dpi = 300, bg="white")
# 
# cat("Centroid behavioral space plot complete!\n")


















### REPEATABILITY ANALYSIS FOR BIVARIATE BEHAVIORAL SPACE
#----------------------------#
cat("Calculating repeatability of bivariate behavioral space...\n")

# Method 1: Individual dimension repeatability using rptR
if(!require(rptR, quietly = TRUE)) {
  cat("Installing rptR package...\n")
  install.packages("rptR")
  library(rptR)
}

# Prepare data for repeatability analysis (need balanced or at least 2+ observations per individual)
temp_repeatability_data <- temp_behavioral_axis %>%
  filter(!is.na(exploration_index), !is.na(site_fidelity_index)) %>%
  group_by(animal_id) %>%
  filter(n() >= 2) %>%  # Need at least 2 observations per individual
  ungroup() %>%
  mutate(
    animal_id = as.factor(animal_id),
    year = as.factor(year)
  )

cat(sprintf("Animals with ≥2 observations for repeatability: %d\n", 
            length(unique(temp_repeatability_data$animal_id))))

# Repeatability for exploration index
cat("Calculating exploration index repeatability...\n")
temp_rpt_exploration <- rptR::rpt(exploration_index ~ (1 | animal_id), 
                                  grname = "animal_id", 
                                  data = temp_repeatability_data, 
                                  datatype = "Gaussian",
                                  nboot = 1000, 
                                  npermut = 0)

# Repeatability for site fidelity index
cat("Calculating site fidelity index repeatability...\n")
temp_rpt_fidelity <- rptR::rpt(site_fidelity_index ~ (1 | animal_id), 
                               grname = "animal_id", 
                               data = temp_repeatability_data, 
                               datatype = "Gaussian",
                               nboot = 1000, 
                               npermut = 0)

# Method 2: Euclidean distance-based repeatability
cat("Calculating bivariate distance-based repeatability...\n")

# Calculate centroid distances for each fish
temp_distance_repeatability <- temp_repeatability_data %>%
  group_by(animal_id) %>%
  mutate(
    # Calculate centroid for each fish
    centroid_exploration = mean(exploration_index, na.rm = TRUE),
    centroid_fidelity = mean(site_fidelity_index, na.rm = TRUE),
    # Calculate Euclidean distance from centroid
    distance_from_centroid = sqrt((exploration_index - centroid_exploration)^2 + 
                                 (site_fidelity_index - centroid_fidelity)^2)
  ) %>%
  ungroup()

# Calculate mean within-individual distance vs between-individual distance
temp_within_individual_var <- temp_distance_repeatability %>%
  group_by(animal_id) %>%
  summarise(within_var = var(distance_from_centroid, na.rm = TRUE), .groups = "drop") %>%
  summarise(mean_within_var = mean(within_var, na.rm = TRUE)) %>%
  pull(mean_within_var)

temp_between_individual_var <- temp_fish_centroids %>%
  filter(animal_id %in% unique(temp_repeatability_data$animal_id)) %>%
  summarise(
    between_var_exploration = var(centroid_exploration, na.rm = TRUE),
    between_var_fidelity = var(centroid_fidelity, na.rm = TRUE),
    # Combined bivariate between-individual variance
    between_var_combined = between_var_exploration + between_var_fidelity
  ) %>%
  pull(between_var_combined)

# Calculate ICC-like repeatability for bivariate space
temp_bivariate_repeatability <- temp_between_individual_var / 
  (temp_between_individual_var + temp_within_individual_var)

# Method 3: Mahalanobis distance repeatability (accounts for covariance)
if(!require(MASS, quietly = TRUE)) {
  install.packages("MASS")
  library(MASS)
}

# Calculate covariance matrix and Mahalanobis distances
temp_mahalanobis_data <- temp_repeatability_data %>%
  select(animal_id, exploration_index, site_fidelity_index) %>%
  group_by(animal_id) %>%
  filter(n() >= 3) %>%  # Need at least 3 points for covariance
  ungroup()

if(nrow(temp_mahalanobis_data) > 0) {
  temp_overall_cov <- cov(temp_mahalanobis_data[, c("exploration_index", "site_fidelity_index")], 
                          use = "complete.obs")
  
  temp_mahalanobis_distances <- temp_mahalanobis_data %>%
    group_by(animal_id) %>%
    mutate(
      centroid_exploration = mean(exploration_index, na.rm = TRUE),
      centroid_fidelity = mean(site_fidelity_index, na.rm = TRUE)
    ) %>%
    rowwise() %>%
    mutate(
      mahal_distance = tryCatch({
        point_diff <- c(exploration_index - centroid_exploration, 
                       site_fidelity_index - centroid_fidelity)
        sqrt(t(point_diff) %*% solve(temp_overall_cov) %*% point_diff)
      }, error = function(e) NA)
    ) %>%
    ungroup()
  
  # Repeatability based on Mahalanobis distances
  temp_mahal_rpt <- rptR::rpt(mahal_distance ~ (1 | animal_id), 
                              grname = "animal_id", 
                              data = temp_mahalanobis_distances, 
                              datatype = "Gaussian",
                              nboot = 1000, 
                              npermut = 0)
}

### SUMMARY OF REPEATABILITY RESULTS
#----------------------------#
cat("\n=== BEHAVIORAL SPACE REPEATABILITY RESULTS ===\n")

cat("\n1. INDIVIDUAL DIMENSION REPEATABILITY:\n")
cat(sprintf("Exploration Index R = %.3f (95%% CI: %.3f - %.3f)\n", 
            temp_rpt_exploration$R$animal_id[1], 
            temp_rpt_exploration$CI_emp$animal_id[1,1], 
            temp_rpt_exploration$CI_emp$animal_id[1,2]))

cat(sprintf("Site Fidelity Index R = %.3f (95%% CI: %.3f - %.3f)\n", 
            temp_rpt_fidelity$R$animal_id[1], 
            temp_rpt_fidelity$CI_emp$animal_id[1,1], 
            temp_rpt_fidelity$CI_emp$animal_id[1,2]))

cat("\n2. BIVARIATE DISTANCE-BASED REPEATABILITY:\n")
cat(sprintf("Euclidean Distance R = %.3f\n", temp_bivariate_repeatability))

if(exists("temp_mahal_rpt")) {
  cat(sprintf("Mahalanobis Distance R = %.3f (95%% CI: %.3f - %.3f)\n", 
              temp_mahal_rpt$R$animal_id[1], 
              temp_mahal_rpt$CI_emp$animal_id[1,1], 
              temp_mahal_rpt$CI_emp$animal_id[1,2]))
} else {
  cat("Mahalanobis Distance R = Not calculated (insufficient data)\n")
}

cat("\n3. INTERPRETATION:\n")
cat("- Values closer to 1.0 indicate high individual consistency\n")
cat("- Values closer to 0.0 indicate high within-individual variation\n")
cat("- Mahalanobis distance accounts for correlation between dimensions\n")

cat("\nRepeatability analysis complete!\n")