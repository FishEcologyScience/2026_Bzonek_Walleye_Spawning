## --------------------------------------------------------------#
## Script name: Script5-7_Claude_station_preference_consistency
##
## Purpose of script: 
##    Analyze consistency of station preferences for individual fish
##    across years using spawning detection data. Combines station 
##    usage proportion analysis with specialization indices.
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2025-01-05
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   - Uses df_spawn as starting point for spawning station preferences
##   - Implements station usage proportions with mixed model repeatability
##   - Calculates Shannon diversity and specialization indices
##   - Analyzes individual consistency in station preference patterns
## --------------------------------------------------------------#


### Examine data structure and prepare dataset
#----------------------------#
cat("=== STATION PREFERENCE CONSISTENCY ANALYSIS ===\n")

# Check data availability
cat("df_spawn dimensions:", dim(df_spawn), "\n")
cat("Unique stations in df_spawn:", n_distinct(df_spawn$station_no), "\n")
cat("Years in df_spawn:", paste(sort(unique(df_spawn$year)), collapse = ", "), "\n")

### Create station usage dataset
#----------------------------#
cat("\n=== CREATING STATION USAGE DATASET ===\n")

# Calculate total detections per fish-year for spawning events
temp_fish_year_totals <- df_spawn %>%
  group_by(animal_id, year) %>%
  summarise(
    total_spawning_detections = sum(detcount),
    total_spawning_duration = sum(residence),
    total_spawning_events = n(),
    .groups = 'drop'
  )

# Calculate station usage proportions per fish-year
temp_station_usage <- df_spawn %>%
  group_by(animal_id, year, station_no) %>%
  summarise(
    station_detections = sum(detcount),
    station_duration = sum(residence),
    station_events = n(),
    .groups = 'drop'
  ) %>%
  left_join(temp_fish_year_totals, by = c("animal_id", "year")) %>%
  mutate(
    station_proportion_detections = station_detections / total_spawning_detections,
    station_proportion_duration = station_duration / total_spawning_duration,
    event_proportion = station_events / total_spawning_events
  ) %>%
  # Filter for fish-years with adequate spawning activity
  filter(total_spawning_events >= 2, total_spawning_detections >= 10)

cat("Station usage summary:\n")
cat("- Fish-year-station combinations:", nrow(temp_station_usage), "\n")
cat("- Unique fish:", n_distinct(temp_station_usage$animal_id), "\n")
cat("- Stations used:", paste(sort(unique(temp_station_usage$station_no)), collapse = ", "), "\n")

 

### Analysis 1: Station-specific repeatability
#----------------------------#
cat("\n=== ANALYSIS 1: STATION-SPECIFIC REPEATABILITY ===\n")

# Get stations with sufficient data
temp_major_stations <- temp_station_usage %>%
  group_by(station_no) %>%
  summarise(
    n_fish_years = n(),
    n_fish = n_distinct(animal_id),
    mean_usage = mean(station_proportion_detections)
  ) %>%
  filter(n_fish_years >= 5, n_fish >= 3) %>%
  arrange(desc(mean_usage))

cat("Major stations for analysis:\n")
print(temp_major_stations)

# Calculate repeatability for each major station
temp_station_repeatability <- list()

for(station in temp_major_stations$station_no) {
  
  cat("\n--- Station", station, "---\n")
  
  # Prepare data for this station (including zeros for fish that didn't use it)
  temp_station_data <- temp_fish_year_totals %>%
    left_join(
      filter(temp_station_usage, station_no == station) %>%
        select(animal_id, year, station_proportion_detections),
      by = c("animal_id", "year")
    ) %>%
    mutate(station_proportion_detections = ifelse(is.na(station_proportion_detections), 0, station_proportion_detections)) %>%
    # Only include fish with multiple years
    group_by(animal_id) %>%
    filter(n() >= 2) %>%
    ungroup() %>%
    # Add covariates
    left_join(data_waterlevel, by = join_by(year)) %>%
    left_join(select(data_fish, animal_id, length_total), by = join_by(animal_id))
  
  if(nrow(temp_station_data) >= 10 && n_distinct(temp_station_data$animal_id) >= 3) {
    
    tryCatch({
      temp_rpt <- rptR::rpt(
        station_proportion_detections ~ water_level + length_total + (1|animal_id) + (1|year),
        grname = "animal_id", 
        data = temp_station_data, 
        datatype = "Gaussian",
        nboot = 50, 
        npermut = 0
      )
      
      temp_station_repeatability[[as.character(station)]] <- list(
        station = station,
        n_observations = nrow(temp_station_data),
        n_fish = n_distinct(temp_station_data$animal_id),
        repeatability = temp_rpt$R,
        ci_lower = temp_rpt$CI_emp[1],
        ci_upper = temp_rpt$CI_emp[2],
        p_value = temp_rpt$P[1]
      )
      
      cat("Station", station, "repeatability:", round(temp_rpt$R, 3), 
          "(", round(temp_rpt$CI_emp[1], 3), "-", round(temp_rpt$CI_emp[2], 3), "), p =", 
          round(temp_rpt$P[1], 4), "\n")
      
    }, error = function(e) {
      cat("Error analyzing station", station, ":", e$message, "\n")
    })
    
  } else {
    cat("Insufficient data for station", station, "\n")
  }
}

# Summarize station repeatability results
if(length(temp_station_repeatability) > 0) {
  temp_repeatability_summary <- bind_rows(temp_station_repeatability) %>%
    arrange(desc(repeatability))
  
  cat("\n=== STATION REPEATABILITY SUMMARY ===\n")
  print(temp_repeatability_summary)
}

 

### Analysis 2: Specialization indices
#----------------------------#
cat("\n=== ANALYSIS 3: SPECIALIZATION ANALYSIS ===\n")

# Calculate Shannon diversity and specialization indices per fish-year
df_rec_specialization <- temp_station_usage %>%
  group_by(animal_id, year) %>%
  summarise(
    n_stations_used = n(),
    ### Detections
    # Shannon diversity index
    shannon_diversity = -sum(station_proportion_detections * log(station_proportion_detections), na.rm = TRUE),
    # Concentration ratio (proportion at most used station)
    max_station_proportion_detections = max(station_proportion_detections),
    # Effective number of stations (exp of Shannon diversity)
    effective_stations = exp(shannon_diversity),
    # Specialization index (1 - normalized Shannon diversity)
    specialization_index = 1 - (shannon_diversity / log(n_stations_used)),
    
    #Duration
    # Shannon diversity index
    shannon_diversity_duration = -sum(station_proportion_duration * log(station_proportion_duration), na.rm = TRUE),
    # Concentration ratio (proportion at most used station)
    max_station_proportion_duration = max(station_proportion_duration),
    # Effective number of stations (exp of Shannon diversity)
    effective_stations_duration = exp(shannon_diversity_duration),
    # Specialization index (1 - normalized Shannon diversity)
    specialization_index_duration = 1 - (shannon_diversity_duration / log(n_stations_used)),
    .groups = 'drop'
  ) %>%
  # Add fish and environmental data
  left_join(data_waterlevel, by = join_by(year)) %>%
  left_join(select(data_fish, animal_id, length_total), by = join_by(animal_id)) %>% 
  left_join(select(df_behaviour, detcount_all_sum, animal_id, year), by = join_by(animal_id, year))
 

cat("Specialization summary:\n")
cat("\n--- Detection-based Metrics ---\n")
cat("  Shannon diversity: mean =", round(mean(df_rec_specialization$shannon_diversity, na.rm = TRUE), 2),
    ", SD =", round(sd(df_rec_specialization$shannon_diversity, na.rm = TRUE), 2),
    " (range:", round(min(df_rec_specialization$shannon_diversity, na.rm = TRUE), 2), "-",
    round(max(df_rec_specialization$shannon_diversity, na.rm = TRUE), 2), ")\n")
cat("  Specialization index: mean =", round(mean(df_rec_specialization$specialization_index, na.rm = TRUE), 2),
    ", SD =", round(sd(df_rec_specialization$specialization_index, na.rm = TRUE), 2),
    " (range:", round(min(df_rec_specialization$specialization_index, na.rm = TRUE), 2), "-",
    round(max(df_rec_specialization$specialization_index, na.rm = TRUE), 2), ")\n")
cat("  Max station proportion: mean =", round(mean(df_rec_specialization$max_station_proportion_detections, na.rm = TRUE), 2),
    ", SD =", round(sd(df_rec_specialization$max_station_proportion_detections, na.rm = TRUE), 2),
    " (range:", round(min(df_rec_specialization$max_station_proportion_detections, na.rm = TRUE), 2), "-",
    round(max(df_rec_specialization$max_station_proportion_detections, na.rm = TRUE), 2), ")\n")
cat("  Effective stations: mean =", round(mean(df_rec_specialization$effective_stations, na.rm = TRUE), 2),
    ", SD =", round(sd(df_rec_specialization$effective_stations, na.rm = TRUE), 2),
    " (range:", round(min(df_rec_specialization$effective_stations, na.rm = TRUE), 2), "-",
    round(max(df_rec_specialization$effective_stations, na.rm = TRUE), 2), ")\n")

cat("\n--- Duration-based Metrics ---\n")
cat("  Shannon diversity: mean =", round(mean(df_rec_specialization$shannon_diversity_duration, na.rm = TRUE), 2),
    ", SD =", round(sd(df_rec_specialization$shannon_diversity_duration, na.rm = TRUE), 2),
    " (range:", round(min(df_rec_specialization$shannon_diversity_duration, na.rm = TRUE), 2), "-",
    round(max(df_rec_specialization$shannon_diversity_duration, na.rm = TRUE), 2), ")\n")
cat("  Specialization index: mean =", round(mean(df_rec_specialization$specialization_index_duration, na.rm = TRUE), 2),
    ", SD =", round(sd(df_rec_specialization$specialization_index_duration, na.rm = TRUE), 2),
    " (range:", round(min(df_rec_specialization$specialization_index_duration, na.rm = TRUE), 2), "-",
    round(max(df_rec_specialization$specialization_index_duration, na.rm = TRUE), 2), ")\n")
cat("  Max station proportion: mean =", round(mean(df_rec_specialization$max_station_proportion_duration, na.rm = TRUE), 2),
    ", SD =", round(sd(df_rec_specialization$max_station_proportion_duration, na.rm = TRUE), 2),
    " (range:", round(min(df_rec_specialization$max_station_proportion_duration, na.rm = TRUE), 2), "-",
    round(max(df_rec_specialization$max_station_proportion_duration, na.rm = TRUE), 2), ")\n")
cat("  Effective stations: mean =", round(mean(df_rec_specialization$effective_stations_duration, na.rm = TRUE), 2),
    ", SD =", round(sd(df_rec_specialization$effective_stations_duration, na.rm = TRUE), 2),
    " (range:", round(min(df_rec_specialization$effective_stations_duration, na.rm = TRUE), 2), "-",
    round(max(df_rec_specialization$effective_stations_duration, na.rm = TRUE), 2), ")\n")



  # Test repeatability of specialization indices
  tryCatch({
    temp_rpt_specialization <- rptR::rpt(
      specialization_index ~ water_level + length_total + (1|animal_id) + (1|year),
      grname = "animal_id",
      data = filter(df_rec_specialization, !is.na(specialization_index), !is.infinite(specialization_index)),
      datatype = "Gaussian",
      nboot = 100,
      npermut = 0
    )
    
    cat("\nSpecialization index repeatability:\n")
    cat("- R =", round(temp_rpt_specialization$R, 3), "\n")
    cat("- 95% CI: (", round(temp_rpt_specialization$CI_emp[1], 3), "-", 
        round(temp_rpt_specialization$CI_emp[2], 3), ")\n")
    cat("- p =", round(temp_rpt_specialization$P[1], 4), "\n")
    
    plot(temp_rpt_specialization)
    
  }, error = function(e) {
    cat("Error in specialization repeatability:", e$message, "\n")
  })
  
  # Visualize specialization patterns
  plots$advanced$station_consistency$specialization_dist <- df_rec_specialization %>%
    ggplot(aes(x = specialization_index)) +
    geom_histogram(bins = 20, alpha = 0.7, fill = "orange", color = "black") +
    labs(
      title = "Distribution of Station Specialization",
      subtitle = "Higher values = more specialized to fewer stations",
      x = "Specialization Index (0 = generalist, 1 = specialist)",
      y = "Count (Fish-Year Combinations)"
    ) +
    theme_classic()
  
  print(plots$advanced$station_consistency$specialization_dist)

  # # Save supporting data
  # plots$advanced$station_consistency$specialization_dist_support <- list(
  #   data = df_rec_specialization
  # )


  # Test repeatability of specialization indices by duration
  tryCatch({
    temp_rpt_specialization_duration <- rptR::rpt(
      specialization_index_duration ~ water_level + length_total + (1|animal_id) + (1|year),
      grname = "animal_id",
      data = filter(df_rec_specialization, !is.na(specialization_index_duration), !is.infinite(specialization_index_duration)),
      datatype = "Gaussian",
      nboot = 100,
      npermut = 0
    )
    
    cat("\nSpecialization index repeatability:\n")
    cat("- R =", round(temp_rpt_specialization_duration$R, 3), "\n")
    cat("- 95% CI: (", round(temp_rpt_specialization_duration$CI_emp[1], 3), "-", 
        round(temp_rpt_specialization_duration$CI_emp[2], 3), ")\n")
    cat("- p =", round(temp_rpt_specialization_duration$P[1], 4), "\n")
    
    plot(temp_rpt_specialization_duration)
    
  }, error = function(e) {
    cat("Error in specialization repeatability:", e$message, "\n")
  })
  
  # Visualize specialization patterns
  plots$advanced$station_consistency$specialization_dist_duration <- df_rec_specialization %>%
    ggplot(aes(x = specialization_index_duration)) +
    geom_histogram(bins = 20, alpha = 0.7, fill = "orange", color = "black") +
    labs(
      title = "Distribution of Station Specialization",
      subtitle = "Higher values = more specialized to fewer stations",
      x = "Specialization Index (0 = generalist, 1 = specialist)",
      y = "Count (Fish-Year Combinations)"
    ) +
    theme_classic()
  
  print(plots$advanced$station_consistency$specialization_dist_duration)

  # # Save supporting data
  # plots$advanced$station_consistency$specialization_dist_duration_support <- list(
  #   data = df_rec_specialization
  # )


### Analysis 3: Individual specialization consistency
#----------------------------#
cat("\n=== ANALYSIS 4: INDIVIDUAL SPECIALIZATION CONSISTENCY ===\n")

# Calculate between-year consistency in specialization for multi-year fish
df_rec_specialization_consistency <- df_rec_specialization %>%
  group_by(animal_id) %>%
  filter(n() >= 2) %>%
  summarise(
    years_observed = n(),
    #Detections
    mean_specialization = mean(specialization_index, na.rm = TRUE),
    sd_specialization = sd(specialization_index, na.rm = TRUE),
    cv_specialization = sd_specialization / mean_specialization,
    range_specialization = max(specialization_index, na.rm = TRUE) - min(specialization_index, na.rm = TRUE),
    #Duration
    mean_specialization_duration = mean(specialization_index_duration, na.rm = TRUE),
    sd_specialization_duration = sd(specialization_index_duration, na.rm = TRUE),
    cv_specialization_duration = sd_specialization_duration / mean_specialization_duration,
    range_specialization_duration = max(specialization_index_duration, na.rm = TRUE) - min(specialization_index_duration, na.rm = TRUE),

    # Consistency category
    consistency_category = case_when(
      cv_specialization < 0.2 ~ "Highly Consistent",
      cv_specialization < 0.4 ~ "Moderately Consistent",
      TRUE ~ "Variable"
    ),
    .groups = 'drop'
  ) %>%
  filter(!is.na(mean_specialization), !is.infinite(cv_specialization))

if(nrow(df_rec_specialization_consistency) > 0) {
  cat("Multi-year fish specialization consistency:\n")
  cat("- Fish with ≥2 years:", nrow(df_rec_specialization_consistency), "\n")
  cat("- Mean CV of specialization:", round(mean(df_rec_specialization_consistency$cv_specialization, na.rm = TRUE), 3), "\n")
  
  # Show consistency categories
  cat("\nSpecialization consistency categories:\n")
  print(table(df_rec_specialization_consistency$consistency_category))
  
  # Plot specialization trajectories with annual boxplots
  plots$advanced$station_consistency$specialization_trajectories <- df_rec_specialization %>%
    filter(animal_id %in% df_rec_specialization_consistency$animal_id) %>%
    ggplot(aes(x = factor(year), y = specialization_index_duration)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7, fill = "lightblue", color = "black") +
    geom_jitter(aes(color = animal_id), width = 0.1, alpha = 0.9, size = 2) +
    geom_line(aes(group = animal_id), alpha = 0.5, color = "gray60") +
    labs(
      title = "Annual Distribution of Specialization Index",
      subtitle = "Boxplots show annual distribution, points show individual fish trajectories",
      x = "Year",
      y = "Specialization Index (Duration-based)",
      color = "Fish ID"
    ) +
    theme_classic() +
    theme(legend.position = "none")

  print(plots$advanced$station_consistency$specialization_trajectories)

  # # Save supporting data
  # plots$advanced$station_consistency$specialization_trajectories_support <- list(
  #   data = df_rec_specialization,
  #   consistency_data = df_rec_specialization_consistency
  # )
}



# Plot repeatablity of specialization 
plots$behaviour$station_specialization <- df_rec_specialization %>%
  group_by(animal_id) %>%
  summarise(mean_specialization_index_duration = mean(specialization_index_duration, na.rm = TRUE)) %>%
  arrange(mean_specialization_index_duration) %>%
  mutate(rank = row_number()) %>%
  right_join(df_rec_specialization, by = "animal_id") %>%   # **Add rank back to the original dataframe**
  # Use the numeric rank for the x-axis instead of the animal_id
  ggplot(aes(x = as.factor(rank), y = specialization_index_duration)) +
    geom_boxplot(outlier.shape = NA, colour = "darkgrey") +
    geom_jitter() +
    ylab("Specialization index\n(normalized Shannon diversity)") +
    xlab("individuals (ranked by mean score per metric)") +
    labs(title = "Specialization index across receivers")
print(plots$behaviour$station_specialization)

# # Save supporting data
# plots$behaviour$station_specialization_support <- list(
#   data = df_rec_specialization
# )


rpt_behaviour_specialization <- rpt(specialization_index_duration ~ water_level + length_total + detcount_all_sum + (1|animal_id) + (1|year),
                          grname = "animal_id", data = df_rec_specialization, datatype = "Gaussian",
                          nboot = 100, npermut = 0)
summary(rpt_behaviour_specialization)
plot(rpt_behaviour_specialization)




# ### Analysis 4: Station preference heatmap
#----------------------------#
cat("\n=== ANALYSIS 5: STATION PREFERENCE VISUALIZATION ===\n")

# Prepare multi-year fish data for heatmap
temp_multi_year_fish <- temp_station_usage %>%
  select(animal_id, year, station_no, station_proportion_detections) %>%
  pivot_wider(
    names_from = station_no,
    values_from = station_proportion_detections,
    values_fill = 0,
    names_prefix = "station_"
  ) %>%
  group_by(animal_id) %>%
  filter(n() >= 2) %>%
  ungroup()

if(nrow(temp_multi_year_fish) > 0) {
  
  # Create heatmap of station usage by multi-year fish
  temp_heatmap_data <- temp_multi_year_fish %>%
    select(animal_id, year, starts_with("station_")) %>%
    pivot_longer(
      cols = starts_with("station_"),
      names_to = "station_no",
      values_to = "proportion"
    ) %>%
    mutate(
      station_no = str_remove(station_no, "station_"),
      fish_year = paste(animal_id, year, sep = "_")
    )
  
  # Only show stations and fish with reasonable usage
  temp_active_stations <- temp_heatmap_data %>%
    group_by(station_no) %>%
    summarise(total_usage = sum(proportion)) %>%
    filter(total_usage > 0.1) %>%
    pull(station_no)
  
  plots$advanced$station_consistency$usage_heatmap <- temp_heatmap_data %>%
    filter(station_no %in% temp_active_stations) %>%
    ggplot(aes(x = station_no, y = reorder(fish_year, as.numeric(animal_id)), fill = proportion)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(name = "Usage\nProportion") +
    labs(
      title = "Station Usage Patterns by Individual Fish-Year",
      x = "Station Number",
      y = "Fish-Year Combinations",
      subtitle = "Multi-year fish only, showing spawning station preferences"
    ) +
    theme_classic() +
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  print(plots$advanced$station_consistency$usage_heatmap)

  # # Save supporting data
  # plots$advanced$station_consistency$usage_heatmap_support <- list(
  #   heatmap_data = temp_heatmap_data,
  #   active_stations = temp_active_stations,
  #   multi_year_fish = temp_multi_year_fish
  # )
}

### Summary and export
#----------------------------#
cat("\n=== SUMMARY AND EXPORT ===\n")

# Create summary objects for export
temp_analysis_summary <- list(
  station_repeatability = if(exists("temp_repeatability_summary")) temp_repeatability_summary else NULL,
  profile_correlations = if(exists("temp_profile_correlations")) temp_profile_correlations else NULL,
  specialization_summary = df_rec_specialization,
  specialization_consistency = if(exists("df_rec_specialization_consistency")) df_rec_specialization_consistency else NULL
)

temp_analysis_summary

# # Save results
# save(temp_analysis_summary, temp_station_usage, df_rec_specialization,
#      file = "04 - Outputs/station_preference_consistency_analysis.RData")

cat("Analysis complete! Results saved to station_preference_consistency_analysis.RData\n")

### Cleanup
#----------------------------#
rm(list = ls(pattern = "^temp_"))
cat("\nStation preference consistency analysis complete!\n")
