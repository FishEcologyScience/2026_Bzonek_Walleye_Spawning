## --------------------------------------------------------------#
## Script name: ScriptX_plot_ranked_repeatability
##
## Purpose of script:
##    Create repeatability plots with conserved rankings across metrics
##    Allows sorting by any metric while maintaining consistent x-axis
##
## Dependencies:
##    - Script2-4_analysis_c_repeatability.R (df_behaviour)
##    - Script2-5_analysis_t_station_consistency.R (df_rec_specialization)
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2025-11-24
##
## --------------------------------------------------------------#
## Modification Notes:
##   - Ranks fish globally by specified metric
##   - Missing data for sort metric ranked at end
##   - Long format allows flexibility in visualization
## --------------------------------------------------------------#



#####Set Parameters ##############################################----
#-------------------------------------------------------------#

### Specify which metric to sort by
#----------------------------#
param_repeatability_sort <- "station_count"
# Options: "station_count", "station_count_ratio", "residence_mean",
#          "depth_mean", "specialization_index_duration"



#####Prepare Data ################################################----
#-------------------------------------------------------------#

### Join behaviour and specialization data
#----------------------------#
cat("=== PREPARING REPEATABILITY DATA ===\n")

df_repeatability_wide <- df_behaviour %>%
  left_join(
    select(df_rec_specialization, animal_id, year, specialization_index_duration),
    by = c("animal_id", "year")
  )

cat("Combined dataset dimensions:", dim(df_repeatability_wide), "\n")
cat("Unique fish:", n_distinct(df_repeatability_wide$animal_id), "\n")


### Calculate global rank based on param_repeatability_sort
#----------------------------#
cat("\n=== CALCULATING GLOBAL RANKS ===\n")
cat("Sorting by:", param_repeatability_sort, "\n")

df_rank <- df_repeatability_wide %>%
  group_by(animal_id) %>%
  summarise(
    mean_sort_value = mean(.data[[param_repeatability_sort]], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Rank fish with data first, then fish with missing data (NA/NaN) at end
  mutate(
    has_data = !is.na(mean_sort_value) & !is.nan(mean_sort_value),
    global_rank = case_when(
      has_data ~ rank(mean_sort_value, ties.method = "first"),
      TRUE ~ n() + row_number()  # Put missing at end
    )
  ) %>%
  arrange(global_rank)

cat("Fish with data for sort metric:", sum(df_rank$has_data), "\n")
cat("Fish with missing data (ranked at end):", sum(!df_rank$has_data), "\n")


### Convert to long format with rank
#----------------------------#
cat("\n=== CONVERTING TO LONG FORMAT ===\n")

df_repeatability_long <- df_repeatability_wide %>%
  left_join(select(df_rank, animal_id, global_rank), by = "animal_id") %>%
  pivot_longer(
    cols = c(station_count, station_count_ratio, residence_mean,
             depth_mean, specialization_index_duration),
    names_to = "metric_name",
    values_to = "metric_value"
  ) %>%
  # Add nice labels for facets
  mutate(
    metric_label = case_when(
      metric_name == "station_count" ~ "Station Count",
      metric_name == "station_count_ratio" ~ "Station Proportion",
      metric_name == "residence_mean" ~ "Residence Duration (h)",
      metric_name == "depth_mean" ~ "Spawning Depth (m)",
      metric_name == "specialization_index_duration" ~ "Specialization Index",
      TRUE ~ metric_name
    ),
    # Order facets in logical sequence
    metric_label = factor(metric_label, levels = c(
      "Station Count",
      "Station Proportion",
      "Specialization Index",
      "Residence Duration (h)",
      "Spawning Depth (m)"
    ))
  )

cat("Long format dimensions:", dim(df_repeatability_long), "\n")
cat("Metrics included:", paste(unique(df_repeatability_long$metric_label), collapse = ", "), "\n")



#####Create Plots ################################################----
#-------------------------------------------------------------#

### Main ranked repeatability plot
#----------------------------#
cat("\n=== CREATING RANKED REPEATABILITY PLOT ===\n")

# Create nice sort_by label for plot title
temp_sort_label <- case_when(
  param_repeatability_sort == "station_count" ~ "Station Count",
  param_repeatability_sort == "station_count_ratio" ~ "Station Proportion",
  param_repeatability_sort == "residence_mean" ~ "Residence Duration",
  param_repeatability_sort == "depth_mean" ~ "Spawning Depth",
  param_repeatability_sort == "specialization_index_duration" ~ "Specialization Index",
  TRUE ~ param_repeatability_sort
)

plots$behaviour$ranked_repeatability <- df_repeatability_long %>%
  ggplot(aes(x = as.factor(global_rank), y = metric_value)) +
  geom_boxplot(outlier.shape = NA, colour = "darkgrey") +
  geom_jitter(alpha = 0.5, width = 0.2) +
  facet_wrap(~metric_label, scales = "free_y", ncol = 1, strip.position = "left") +
  labs(x =  paste0("Individuals (ranked by mean ", temp_sort_label, ")"),
       y = NULL) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    panel.spacing = unit(1, "lines")
  )

print(plots$behaviour$ranked_repeatability)


### Summary statistics
#----------------------------#
cat("\n=== SUMMARY STATISTICS ===\n")

temp_summary_by_metric <- df_repeatability_long %>%
  group_by(metric_label) %>%
  summarise(
    n_observations = sum(!is.na(metric_value)),
    n_fish = n_distinct(animal_id[!is.na(metric_value)]),
    mean_value = mean(metric_value, na.rm = TRUE),
    sd_value = sd(metric_value, na.rm = TRUE),
    .groups = 'drop'
  )

print(temp_summary_by_metric)



#####Correlation Analysis ########################################----
#-------------------------------------------------------------#

### Calculate correlations between metrics
#----------------------------#
cat("\n=== CORRELATION ANALYSIS ===\n")

# Calculate mean values per fish for correlation (average across years)
temp_fish_means <- df_repeatability_wide %>%
  group_by(animal_id) %>%
  summarise(
    mean_station_count = mean(station_count, na.rm = TRUE),
    mean_station_proportion = mean(station_count_ratio, na.rm = TRUE),
    mean_residence = mean(residence_mean, na.rm = TRUE),
    mean_depth = mean(depth_mean, na.rm = TRUE),
    mean_specialization = mean(specialization_index_duration, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  select(-animal_id)

# Calculate correlation matrix
temp_cor_matrix <- cor(temp_fish_means, use = "pairwise.complete.obs")

# Rename for nice labels
colnames(temp_cor_matrix) <- c("Station\nCount", "Station\nProportion",
                                "Residence\nDuration", "Spawning\nDepth",
                                "Specialization\nIndex")
rownames(temp_cor_matrix) <- colnames(temp_cor_matrix)

cat("\nCorrelation matrix:\n")
print(round(temp_cor_matrix, 3))


### Visualize correlation matrix
#----------------------------#
cat("\n=== CREATING CORRELATION PLOT ===\n")
corrplot::corrplot.mixed(
  temp_cor_matrix,
  order = 'AOE',
  upper = 'shade',
  lower = 'number',
  tl.pos = 'lt',
  tl.col = 'black',
  number.cex = 0.8,
  tl.cex = 0.9
)


### Calculate sample sizes for each pairwise correlation
#----------------------------#
cat("\n=== PAIRWISE SAMPLE SIZES ===\n")

temp_pairwise_n <- temp_fish_means %>%
  summarise(
    across(everything(), ~sum(!is.na(.)))
  ) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "n_fish")

cat("\nFish with complete data per metric:\n")
print(temp_pairwise_n)


#####Cleanup #####################################################----
#-------------------------------------------------------------#

rm(list = ls(pattern = "^temp_"))
cat("\nRanked repeatability analysis complete!\n")
