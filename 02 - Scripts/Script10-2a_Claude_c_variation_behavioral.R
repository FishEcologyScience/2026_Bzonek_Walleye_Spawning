## --------------------------------------------------------------#
## Script name: Script5-5_Claude_variation_depth
##
## Purpose of script: 
##    Analyze daily depth variation patterns in walleye behavior
##    throughout April spawning season using temp_data_daily_var
##    as the base dataset
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2025-01-03
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   - Focus on daily depth variability patterns
##   - Examines changes in depth behavior over the spawning month
##   - Uses filtered data with adequate fish-year sample sizes
## --------------------------------------------------------------#

### Create daily depth variation dataset
#----------------------------#
cat("=== CREATING DAILY DEPTH VARIATION DATASET ===\n")
 
# Create base dataset with daily depth metrics
temp_data_daily_var <- data_det %>%
  mutate(
    year = year(detection_timestamp_utc),
    april_day = day(detection_timestamp_utc),
    date = as.Date(detection_timestamp_utc)
  ) %>%
  # Filter for adequate data per fish-year (>15 days as discussed)
  group_by(animal_id, year) %>%
  mutate(days_per_fish_year = n_distinct(date)) %>%
  ungroup() %>%
  filter(days_per_fish_year > 15) %>%
  # Calculate daily metrics per fish
  group_by(animal_id, year, april_day, date) %>%
  summarise(
    daily_depth_mean = mean(-sensor_value.Cal, na.rm = TRUE),
    daily_depth_sd = sd(-sensor_value.Cal, na.rm = TRUE),
    daily_depth_range = max(-sensor_value.Cal, na.rm = TRUE) - min(-sensor_value.Cal, na.rm = TRUE),
    daily_depth_min = min(-sensor_value.Cal, na.rm = TRUE),
    daily_depth_max = max(-sensor_value.Cal, na.rm = TRUE),
    daily_depth_median = median(-sensor_value.Cal, na.rm = TRUE),
    n_detections_per_day = n(),
    .groups = 'drop'
  ) %>%
  # Filter for days with adequate detections
  filter(n_detections_per_day >= 5) %>%
  # Add fish-level summaries
  group_by(animal_id) %>%
  mutate(
    fish_total_days = n(),
    fish_mean_depth = mean(daily_depth_mean),
    fish_mean_variability = mean(daily_depth_sd, na.rm = TRUE)
  ) %>%
  ungroup()

# Report dataset summary
cat("Dataset summary:\n")
cat("- Total fish-days:", nrow(temp_data_daily_var), "\n")
cat("- Unique fish:", n_distinct(temp_data_daily_var$animal_id), "\n")
cat("- Years:", paste(sort(unique(temp_data_daily_var$year)), collapse = ", "), "\n")
cat("- April days covered:", min(temp_data_daily_var$april_day), "to", max(temp_data_daily_var$april_day), "\n")


### Analysis 1: Individual fish patterns
#----------------------------#
cat("\n=== ANALYSIS 1: INDIVIDUAL FISH PATTERNS ===\n")

# Individual fish depth trajectories
plots$advanced$depth_variation$trajectories_individual <- temp_data_daily_var %>%
  # Sample subset of fish for clarity
  #filter(animal_id %in% sample(unique(animal_id), min(10, n_distinct(animal_id)))) %>%
  ggplot(aes(x = april_day, y = daily_depth_mean, color = factor(animal_id))) +
  geom_line(alpha = 0.7, size = 1) +
  geom_point(alpha = 0.5) +
  facet_wrap(~year, scales = "free_y") +
  labs(
    title = "Individual Fish Depth Patterns",
    x = "Day of April",
    y = "Daily Mean Depth (m)",
    color = "Fish ID"
  ) +
  theme_classic() +
  theme(legend.position = "none")

print(plots$advanced$depth_variation$trajectories_individual)


### Analysis 2: Year comparisons
#----------------------------#
cat("\n=== ANALYSIS 2: YEAR COMPARISONS ===\n")

plots$advanced$depth_variation$variability_by_year <- temp_data_daily_var %>%
  ggplot(aes(x = april_day, y = daily_depth_sd, color = factor(year))) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.3) +
  labs(
    title = "Daily Depth Variability by Year",
    x = "Day of April",
    y = "Daily Depth Variability (SD, m)",
    color = "Year"
  ) +
  theme_classic()

print(plots$advanced$depth_variation$variability_by_year)

### Analysis 3: Between-year variability
#----------------------------#
cat("\n=== ANALYSIS 3: BETWEEN-YEAR VARIABILITY ===\n")

# Calculate year-to-year consistency for fish with multiple years
df_depth_year <- temp_data_daily_var %>%
  group_by(animal_id, year) %>%
  summarise(
    annual_mean_depth = mean(daily_depth_mean),
    annual_top_quantile_depth = quantile(daily_depth_mean, 0.9, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
 left_join(data_waterlevel, by = join_by(year)) %>% 
 left_join(select(data_fish, animal_id, length_total), by = join_by(animal_id))

 #Summarize dataset detections
  temp_detcount <- data_det %>% 
   mutate(year = year(detection_timestamp_utc)) %>% 
   group_by(animal_id, year) %>% 
   reframe(detcount_all_sum = n())
 
  df_depth_year <- left_join(df_depth_year, temp_detcount)
  
  
  
 #Summarize down to fish
 temp_between_year_variability <- df_depth_year %>% 
  group_by(animal_id) %>%
  filter(n() >= 2) %>%  # Only fish with multiple years
  summarise(
    years_observed = n(),
    between_year_depth_sd = sd(annual_mean_depth),
    between_year_depth_cv = sd(annual_mean_depth) / mean(annual_mean_depth),
    between_year_range = max(annual_mean_depth) - min(annual_mean_depth),
    mean_annual_depth = mean(annual_mean_depth),
    # Top quantile depth metrics
    mean_top_quantile_depth = mean(annual_top_quantile_depth),
    between_year_top_quantile_sd = sd(annual_top_quantile_depth),
    between_year_top_quantile_cv = sd(annual_top_quantile_depth) / mean(annual_top_quantile_depth),
    .groups = 'drop'
  ) %>%
  mutate(
    year_consistency_category = case_when(
      between_year_depth_cv < 0.2 ~ "Highly Consistent Across Years",
      between_year_depth_cv < 0.4 ~ "Moderately Consistent Across Years", 
      TRUE ~ "Variable Across Years"
    )
  )

cat("Multi-year fish summary:\n")
cat("- Fish with ≥2 years:", nrow(temp_between_year_variability), "\n")
cat("- Mean between-year CV:", round(mean(temp_between_year_variability$between_year_depth_cv, na.rm=TRUE), 3), "\n")

# Plot between-year consistency distribution
plots$advanced$depth_variation$consistency_between_years <- temp_between_year_variability %>%
  ggplot(aes(x = between_year_depth_cv)) +
  geom_histogram(bins = 15, alpha = 0.7, fill = "lightgreen", color = "black") +
  geom_vline(aes(xintercept = mean(between_year_depth_cv, na.rm=TRUE)),
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Between-Year Consistency Distribution",
    subtitle = "Lower CV = more consistent depth preference across years",
    x = "Between-Year Coefficient of Variation (CV)",
    y = "Number of Fish",
    caption = paste("n =", nrow(temp_between_year_variability), "fish with multiple years")
  ) +
  theme_classic()

print(plots$advanced$depth_variation$consistency_between_years)

# Show consistency categories
cat("\nBetween-year consistency categories:\n")
print(table(temp_between_year_variability$year_consistency_category))

### Analysis 4: Mixed effects model for individual consistency
#----------------------------#
cat("\n=== ANALYSIS 4: INDIVIDUAL CONSISTENCY MODEL ===\n")

tryCatch({
  # Test for individual effects while controlling for time and year
  temp_individual_model <- lme4::lmer(
    daily_depth_mean ~ april_day + I(april_day^2) + factor(year) + (1|animal_id), 
    data = temp_data_daily_var
  )
  
  # Check for singularity
  if(!lme4::isSingular(temp_individual_model)) {
    
    # Extract individual consistency metrics
    temp_icc_depth <- performance::icc(temp_individual_model)
    temp_model_performance <- performance::model_performance(temp_individual_model)
    temp_random_effects <- lme4::VarCorr(temp_individual_model)
    
    cat("Mixed effects model results:\n")
    cat("- Individual ICC (consistency):", round(temp_icc_depth$ICC_adjusted, 3), "\n")
    cat("- Marginal R² (fixed effects):", round(temp_model_performance$R2_marginal, 3), "\n") 
    cat("- Conditional R² (total):", round(temp_model_performance$R2_conditional, 3), "\n")
    
    # Individual variance component
    temp_individual_var <- as.numeric(temp_random_effects$animal_id[1])
    temp_residual_var <- attr(temp_random_effects, "sc")^2
    temp_individual_pct <- temp_individual_var / (temp_individual_var + temp_residual_var) * 100
    
    cat("- Individual variance component:", round(temp_individual_pct, 1), "%\n")
    
    # Interpretation
    if(temp_icc_depth$ICC_adjusted > 0.1) {
      cat("→ STRONG individual consistency detected in depth preferences\n")
    } else if(temp_icc_depth$ICC_adjusted > 0.05) {
      cat("→ MODERATE individual consistency detected\n")
    } else {
      cat("→ LIMITED individual consistency\n")
    }
    
    # Extract individual fish effects (BLUPs)
    temp_fish_effects <- lme4::ranef(temp_individual_model)$animal_id %>%
      tibble::rownames_to_column("animal_id") %>%
      rename(individual_effect = `(Intercept)`) %>%
      arrange(desc(individual_effect))
    
    # Plot individual effects
    plots$advanced$depth_variation$individual_effects <- temp_fish_effects %>%
      mutate(effect_category = case_when(
        individual_effect > 0.5 ~ "Deep Preference",
        individual_effect < -0.5 ~ "Shallow Preference",
        TRUE ~ "Moderate Preference"
      )) %>%
      ggplot(aes(x = reorder(animal_id, individual_effect), y = individual_effect)) +
      geom_point(aes(color = effect_category), size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      coord_flip() +
      labs(
        title = "Individual Fish Depth Preferences",
        subtitle = "Random effects from mixed model (controlling for time and year)",
        x = "Individual Fish",
        y = "Individual Effect (deeper = positive)",
        color = "Depth Preference"
      ) +
      theme_classic() +
      theme(axis.text.y = element_text(size = 8))

    print(plots$advanced$depth_variation$individual_effects)
    
    cat("\nIndividual effect categories:\n")
    print(table(temp_fish_effects %>% 
                mutate(effect_category = case_when(
                  individual_effect > 0.5 ~ "Deep Preference",
                  individual_effect < -0.5 ~ "Shallow Preference", 
                  TRUE ~ "Moderate Preference"
                )) %>% pull(effect_category)))
    
  } else {
    cat("WARNING: Singular fit in mixed model - insufficient data for individual effects\n")
    temp_individual_model <- NULL
    temp_icc_depth <- NULL
    temp_fish_effects <- NULL
    plots$advanced$depth_variation$individual_effects <- NULL
  }
  
}, error = function(e) {
  cat("ERROR in mixed model:", e$message, "\n")
  temp_individual_model <- NULL
  temp_icc_depth <- NULL  
  temp_fish_effects <- NULL
  temp_plot4 <- NULL
})

### Analysis 5: Year-to-year individual trajectories
#----------------------------#
cat("\n=== ANALYSIS 5: INDIVIDUAL YEAR-TO-YEAR TRAJECTORIES ===\n")

# Plot individual fish trajectories across years (for multi-year fish)
if(nrow(temp_between_year_variability) > 0) {
  
  plots$advanced$depth_variation$year_to_year_trajectories <- temp_data_daily_var %>%
    filter(animal_id %in% temp_between_year_variability$animal_id) %>%
    group_by(animal_id, year) %>%
    summarise(annual_mean_depth = mean(daily_depth_mean), .groups = 'drop') %>%
    ggplot(aes(x = factor(year), y = annual_mean_depth, group = animal_id)) +
    geom_line(alpha = 0.7, color = "gray60") +
    geom_point(aes(color = animal_id), size = 2) +
    labs(
      title = "Individual Fish: Year-to-Year Depth Trajectories",
      x = "Year",
      y = "Annual Mean Depth (m)",
      color = "Fish ID"
    ) +
    theme_classic() +
    theme(legend.position = "none")

  print(plots$advanced$depth_variation$year_to_year_trajectories)

} else {
  cat("No multi-year fish available for trajectory analysis\n")
  plots$advanced$depth_variation$year_to_year_trajectories <- NULL
}

### Analysis 6: Individual fish annual variation with boxplots
#----------------------------#
cat("\n=== ANALYSIS 6: INDIVIDUAL ANNUAL VARIATION ===\n")

# Plot individual fish trajectories with boxplots showing daily variation per fish per year
if(nrow(temp_between_year_variability) > 0) {

  plots$advanced$depth_variation$annual_variation_boxplots <- temp_data_daily_var %>%
    filter(animal_id %in% temp_between_year_variability$animal_id) %>%
    ggplot(aes(x = factor(year), y = daily_depth_mean)) +
    # Add boxplots for each fish per year
    geom_boxplot(aes(group = interaction(year, animal_id), fill = animal_id),
                 width = 0.2, alpha = 0.6, outlier.shape = NA,
                 position = position_dodge(width = 0.5)) +
    # Add annual mean points
    stat_summary(aes(group = animal_id, color = animal_id),
                 fun = mean, geom = "point", size = 3,
                 position = position_dodge(width = 0.5)) +
    # Add trajectory lines between annual means
    stat_summary(aes(group = animal_id),
                 fun = mean, geom = "line", alpha = 0.8, color = "black",
                 position = position_dodge(width = 0.5)) +
    labs(
      title = "Individual Fish: Annual Depth Variation and Trajectories",
      subtitle = "Boxplots show daily depth distribution per fish per year, points show annual means",
      x = "Year",
      y = "Daily Mean Depth (m)",
      fill = "Fish ID",
      color = "Fish ID"
    ) +
    theme_classic() +
    theme(legend.position = "none")

  print(plots$advanced$depth_variation$annual_variation_boxplots)

} else {
  cat("No multi-year fish available for annual variation analysis\n")
  plots$advanced$depth_variation$annual_variation_boxplots <- NULL
}



### Repeatability
#----------------------------#
df_depth_year_scaled <- df_depth_year %>%
  mutate(across(annual_mean_depth:detcount_all_sum, ~ scale(.) %>% as.numeric())
         )

rpt_behaviour_depth_full <- rpt(annual_top_quantile_depth ~ water_level + length_total + detcount_all_sum + (1|animal_id) + (1|year),
                             #station_count_spawn ~ length_total + (1|animal_id),
                          grname = "animal_id", data = df_depth_year, datatype = "Gaussian",
                          nboot = 100, npermut = 0)
summary(rpt_behaviour_depth_full)
plot(rpt_behaviour_depth_full)

df_depth_year %>%
  group_by(animal_id) %>%
  summarise(mean_annual_top_quantile_depth = mean(annual_top_quantile_depth, na.rm = TRUE)) %>%
  arrange(mean_annual_top_quantile_depth) %>%
  mutate(rank = row_number()) %>%
  right_join(df_depth_year, by = "animal_id") %>%   # **Add rank back to the original dataframe**
  # Use the numeric rank for the x-axis instead of the animal_id
  ggplot(aes(x = as.factor(rank), y = annual_top_quantile_depth)) +
    geom_boxplot(outlier.shape = NA, colour = "darkgrey") +
    geom_jitter() +
    ylab("mean depth") + 
    xlab("individuals (ranked by mean score per metric)") +
    labs(title = paste("Consistent differences among individuals in:", 
                       "Mean staging depth", sep = "\n"))


### Cleanup
#----------------------------#
rm(list = ls(pattern = "^temp_"))
cat("\nDaily depth variation analysis complete!\n")