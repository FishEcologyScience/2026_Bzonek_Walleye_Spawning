## --------------------------------------------------------------#
## Script name: ScriptX-X_Claude_spawning_proportion_by_hour
##
## Purpose of script:
##    Analyze proportion of detections classified as spawning events by hour
##    Reclassifies spawning events without sun category filter to examine full diel patterns
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2025-10-07
##
## --------------------------------------------------------------#
## Modification Notes:
##   - Temporary exploratory analysis
##   - Recreates spawning classification without dusk/night filter
## --------------------------------------------------------------#


### Set parameters (from Script1-3)
#----------------------------#
param_min_detections <- 3
param_min_residence_hours <- 0.32
param_max_lag_hours <- 2
param_min_spawn_depth <- -2

 

   #####Copied from Script1-3########################################----
   #-------------------------------------------------------------#

   ### Calculate movements (all day, no sun filter)
   #----------------------------#
   cat("Calculating movements from all-day April data...\n")
   
   temp_movement_allday <- as.data.table(data_det_allday_April)[, .(
     detection_timestamp_utc,
     To_animal_id = animal_id,
     To_station = station_no,
     To_lat = deploy_lat,
     To_lon = deploy_long,
     mass, length_total, SunCategory
   )]
   
   # Add year for grouping
   temp_movement_allday[, param_year := year(detection_timestamp_utc)]
   
   # Sort by animal and time
   setorder(temp_movement_allday, To_animal_id, detection_timestamp_utc)
   
   # Create lagged columns
   temp_movement_allday[, c("From_station", "From_animal_id", "From_lat", "From_lon",
                     "From_timestamp", "From_SunCategory") :=
                   shift(.SD, 1, type = "lag"),
                 by = .(To_animal_id, param_year),
                 .SDcols = c("To_station", "To_animal_id", "To_lat", "To_lon",
                            "detection_timestamp_utc", "SunCategory")]

   # Calculate time between consecutive detections
   temp_movement_allday[, lagtime := difftime(detection_timestamp_utc, From_timestamp, units = "hours")]
   
      # Create unique movement ID for each continuous period at a station
      # When fish changes station (or returns to same station later), moveID increments
      # Also splits events when time gap exceeds param_max_lag_hours
      temp_movement_allday[, moveID_temp := paste0(From_animal_id, From_station, To_station)]  # Unique string per movement type
      temp_movement_allday[, station_change := moveID_temp != shift(moveID_temp, 1, fill = first(moveID_temp))]  # Logical: TRUE when station changes
      temp_movement_allday[, long_gap := lagtime >= param_max_lag_hours]  # Logical: TRUE when lag exceeds threshold
      temp_movement_allday[, long_gap := fifelse(is.na(long_gap), FALSE, long_gap)]  # Replace NA with FALSE (first detection per fish-year)
      temp_movement_allday[, moveID := cumsum(station_change | long_gap)]  # Increment when movement changes OR gap is too long
   
   
   # Clean up and convert
   temp_movement_allday[, c("param_year", "moveID_temp") := NULL]
   df_movement_allday <- as.data.frame(temp_movement_allday)
   
   cat("Total movements (all day):", nrow(df_movement_allday), "\n")


   ### Calculate residency (without sun category filter)
   #----------------------------#
   cat("Calculating residency events...\n")
   
   temp_residency_allday <- as.data.table(df_movement_allday)

   # Filter to monitored receivers only
   temp_residency_allday <- temp_residency_allday[To_station %in% df_key$station_no &
                                                   From_station %in% df_key$station_no]

   # Filter by lagtime
   temp_residency_allday <- temp_residency_allday[To_animal_id == From_animal_id &
                                                   lagtime < param_max_lag_hours]

   # Group by movement ID
   temp_residency_allday <- temp_residency_allday[, .(
     detcount = .N,
     time_start = min(detection_timestamp_utc),
     time_end = max(detection_timestamp_utc),
     start_SunCategory = first(SunCategory),
     end_SunCategory = last(SunCategory)
   ), by = .(To_animal_id, To_station, moveID)]
   
   # Calculate residence time
   temp_residency_allday[, residence := difftime(time_end, time_start, units = "hours")]

#----------------------------#
### Apply filters (NO sun category filter here)
#----------------------------#
temp_residency_allday <- temp_residency_allday[detcount >= param_min_detections &
                                               residence >= param_min_residence_hours]

   df_residency_allday <- as.data.frame(temp_residency_allday)
   
   cat("Residency events (all day):", nrow(df_residency_allday), "\n")


   ### Create spawning dataset (with depth filter only)
   #----------------------------#
   cat("Identifying spawning events with depth filter...\n")
   
   data_spawn_allday <- df_movement_allday %>%
     filter(moveID %in% df_residency_allday$moveID) %>%
     select(detection_timestamp_utc, animal_id = To_animal_id, station_no = To_station, moveID) %>%
     left_join(select(df_residency_allday, moveID, residence, time_start, time_end, detcount),
               by = join_by(moveID)) %>%
     left_join(data_det_allday_April, by = join_by(detection_timestamp_utc, animal_id, station_no)) %>%
     group_by(moveID) %>%
     mutate(meanDepth = mean(-sensor_value.Cal, na.rm = TRUE)) %>%
     filter(meanDepth > param_min_spawn_depth) %>%
     ungroup()
   
   cat("Spawning detections (all day):", nrow(data_spawn_allday), "\n")


#####Calculate hourly proportions#################################----
#-------------------------------------------------------------#

cat("\nCalculating spawning proportion by hour...\n")

# Mark spawning detections
temp_summary_spawn_ids <- data_spawn_allday %>%
  select(detection_timestamp_utc, animal_id, station_no) %>%
  mutate(is_spawn = TRUE)

# Calculate hourly proportions (using EST for biological relevance)
df_summary_plot_spawning_hourly <- data_det_allday_April %>%
  mutate(
    detection_timestamp_EST = with_tz(detection_timestamp_utc, tzone = "EST"),
    hour_EST = hour(detection_timestamp_EST)
  ) %>%
  left_join(temp_summary_spawn_ids,
            by = c("detection_timestamp_utc", "animal_id", "station_no")) %>%
  mutate(is_spawn = ifelse(is.na(is_spawn), FALSE, is_spawn)) %>%
  group_by(hour_EST) %>%
  summarise(
    total_detections = n(),
    spawn_detections = sum(is_spawn),
    prop_spawn = spawn_detections / total_detections,
    .groups = "drop"
  ) %>%
  rename(hour = hour_EST)

# Display results
cat("\n--- SPAWNING PROPORTION BY HOUR (EST) ---\n")
print(df_summary_plot_spawning_hourly, n = 24)


### Determine dusk and night periods from df_SunCategory
#----------------------------#
cat("\nDetermining dusk/night periods from sun position data...\n")

# Extract hour from timestamps (convert UTC to EST) and classify by sun category
temp_summary_sun_hours <- df_SunCategory %>%
  mutate(
    Timestamp_EST = with_tz(Timestamp, tzone = "EST"),
    hour_EST = hour(Timestamp_EST)
  ) %>%
  group_by(hour_EST, SunCategory) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(hour_EST) %>%
  mutate(prop = n / sum(n)) %>%
  slice_max(prop, n = 1) %>%  # Most common category per hour
  ungroup() %>%
  rename(hour = hour_EST)

# Identify hour ranges for each category
temp_summary_dusk_hours <- temp_summary_sun_hours %>%
  filter(SunCategory == "dusk") %>%
  pull(hour)

temp_summary_night_hours <- temp_summary_sun_hours %>%
  filter(SunCategory == "night") %>%
  pull(hour)

temp_summary_day_hours <- temp_summary_sun_hours %>%
  filter(SunCategory == "day") %>%
  pull(hour)

cat("Dusk hours (EST):", paste(temp_summary_dusk_hours, collapse = ", "), "\n")
cat("Night hours (EST):", paste(temp_summary_night_hours, collapse = ", "), "\n")
cat("Day hours (EST):", paste(temp_summary_day_hours, collapse = ", "), "\n")



### Calculate movement timing (all receivers)
#----------------------------#
cat("\nCalculating movement timing between all receivers...\n")

df_summary_plot_spawning_movements <- df_movement %>%
  filter(
    !is.na(From_station),
    From_station != To_station
  ) %>%
  mutate(
    detection_timestamp_EST = with_tz(detection_timestamp_utc, tzone = "EST"),
    hour_EST = hour(detection_timestamp_EST)
  ) %>%
  group_by(hour_EST) %>%
  summarise(
    movement_count = n(),
    .groups = "drop"
  ) %>%
  rename(hour = hour_EST)

cat("Total movements between receivers:", sum(df_summary_plot_spawning_movements$movement_count), "\n")


### Visualize hourly pattern with sun category overlays
#----------------------------#
plots$advanced$spawning_hourly$proportion_by_hour <- ggplot() +
  # Add dawn rectangles
   geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
             fill = "#4e7889", alpha = 0.4) +
  # Add day rectangles
    geom_rect(aes(xmin = 5.5, xmax = 17.5, ymin = -Inf, ymax = Inf),
             fill = "#3a8eb2", alpha = 0.3) +
  # Add dusk rectangles
    geom_rect(aes(xmin = 17.5, xmax = 18.5, ymin = -Inf, ymax = Inf),
             fill = "#214e5e", alpha = 0.5) +
  # Add night rectangles
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf),
             fill = "#063039", alpha = 0.5) +
    geom_rect(aes(xmin = 18.5, xmax = 23.5, ymin = -Inf, ymax = Inf),
             fill = "#063039", alpha = 0.5) +
  # Add movement counts as bars on secondary axis
  geom_col(data = df_summary_plot_spawning_movements,
           aes(x = hour, y = movement_count / max(movement_count) * max(df_summary_plot_spawning_hourly$prop_spawn * 100) * 0.5),
           alpha = 0.3, fill = "#008474", width = 0.8) +
  # Add spawning proportion line and points
  geom_line(data = df_summary_plot_spawning_hourly,
            aes(x = hour, y = prop_spawn * 100),
            linewidth = 1) +
  geom_point(data = df_summary_plot_spawning_hourly,
             aes(x = hour, y = prop_spawn * 100, size = total_detections),
             alpha = 0.6) +
  scale_x_continuous(breaks = 0:23, limits = c(0, 23.5)) +
  scale_y_continuous(
    limits = c(0, NA),
    sec.axis = sec_axis(
      ~ . / (max(df_summary_plot_spawning_hourly$prop_spawn * 100) * 0.5) * max(df_summary_plot_spawning_movements$movement_count),
      name = "Movement Count"
    )
  ) +
  labs(
    title = "Proportion of Detections Classified as Spawning Events by Hour",
    subtitle = "Spawning = residency event + depth filter (no dusk/night filter applied)\nBars = movements between receivers",
    x = "Hour of Day (EST, 24-hour)",
    y = "Spawning Event Proportion (%)",
    size = "Total Detections"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0),
    panel.grid.major.y = element_line(color = "gray90")
  )

print(plots$advanced$spawning_hourly$proportion_by_hour)

### Save supporting data for plot recreation
#----------------------------#
# plots$advanced$spawning_hourly$proportion_by_hour_support <- list(
#   hourly_data = df_summary_plot_spawning_hourly,
#   movement_data = df_summary_plot_spawning_movements
# )

# Convert df_ objects to temp_ for cleanup
# temp_summary_plot_spawning_hourly <- df_summary_plot_spawning_hourly
# temp_summary_plot_spawning_movements <- df_summary_plot_spawning_movements
# rm(df_summary_plot_spawning_hourly, df_summary_plot_spawning_movements)


### Summary statistics by sun category
#----------------------------#
cat("\n--- SUMMARY STATISTICS ---\n")
cat("Peak spawning hour:", df_summary_plot_spawning_hourly$hour[which.max(df_summary_plot_spawning_hourly$prop_spawn)], "\n")
cat("Max spawning proportion:",
    round(max(df_summary_plot_spawning_hourly$prop_spawn) * 100, 2), "%\n")
cat("Min spawning proportion:",
    round(min(df_summary_plot_spawning_hourly$prop_spawn) * 100, 2), "%\n")
cat("Overall mean proportion:",
    round(mean(df_summary_plot_spawning_hourly$prop_spawn) * 100, 2), "%\n")

# Calculate by sun category
temp_summary_by_category <- df_summary_plot_spawning_hourly %>%
  mutate(
    category = case_when(
      hour %in% temp_summary_night_hours ~ "Night",
      hour %in% temp_summary_dusk_hours ~ "Dusk",
      hour %in% temp_summary_day_hours ~ "Day",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(category) %>%
  summarise(
    mean_prop = mean(prop_spawn),
    total_detections = sum(total_detections),
    spawn_detections = sum(spawn_detections),
    .groups = "drop"
  )

cat("\nBy sun category:\n")
print(temp_summary_by_category)


### Cleanup
#----------------------------#
rm(list = ls(pattern = "^temp_"))
rm(df_movement_allday, df_residency_allday, data_spawn_allday)
cat("\nAnalysis complete!\n")
