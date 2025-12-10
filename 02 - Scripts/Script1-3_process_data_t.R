## --------------------------------------------------------------#
## Script name: Script1-3_process_data_t
##
## Purpose of script:
##    Process telemetry data: filter, calculate movements, residency, and spawning
##    Apply all analytical parameters and create derived datasets
##    Generate summaries and visualizations
##
##
## Author: Paul Bzonek
##
## Date Created: 2025-10-03
##
## --------------------------------------------------------------#
## Modification Notes:
##   2025-10-03: Created from Script1-2 split - contains all filtering/analysis
## --------------------------------------------------------------#



### Set analysis parameters
#----------------------------#
param_longterm_start <- as.POSIXct("2017-01-01")
param_longterm_end <- as.POSIXct("2023-01-01")
param_min_detections <- 3
param_min_residence_hours <- 0.32
param_max_lag_hours <- 2
param_min_spawn_depth <- -2
param_target_month <- "04"




#####Filter and format detection data##########################----
#-------------------------------------------------------------#

### Filter for long-term receivers
#----------------------------#
# Keep receivers that last for the full duration
temp_df_longterm <- data_rec_locs_raw %>%
  group_by(station_no) %>%
  summarize(firstdeploy = min(deployDate),
            lastrecover = suppressWarnings(max(recoverDate, na.rm = TRUE))) %>%
  filter(firstdeploy < param_longterm_start,
         lastrecover > param_longterm_end) %>%
  ungroup()

cat("Long-term stations identified:", nrow(temp_df_longterm), "\n")

# Keep data from long-term receivers
temp_rec_locs_longterm <- filter(data_rec_locs_raw, station_no %in% temp_df_longterm$station_no) %>%
  ungroup()

data_rec_locs <- filter(data_rec_locs_raw,
                        station_no %in% temp_rec_locs_longterm$station_no)

# Add habitat data (requires df_hab from Script1-1)
if(exists("df_hab")) {
  data_rec_locs <- left_join(data_rec_locs, df_key, by = join_by(station_no)) %>%
    left_join(df_hab, by = c("transect" = "PointID"))
} else {
  warning("df_hab not found - run Script1-1 first or load required habitat data")
  data_rec_locs <- left_join(data_rec_locs, df_key)
}

# Clean up temporary receiver objects
rm(temp_df_longterm, temp_rec_locs_longterm)



#####Format detection data #######################################----
#-------------------------------------------------------------#
# Filter for Walleye only using data.table syntax
data_det_allday_April <- data_det_raw[common_name_e == "Walleye"]

# Filter for target month
data_det_allday_April <- data_det_allday_April[format(detection_timestamp_EST, "%m") == param_target_month]

# Filter for sensor data
data_det_allday_April <- data_det_allday_April[!is.na(sensor_value)]

# Add fish data
data_det_allday_April <- left_join(data_det_allday_April, data_fish, by = join_by(animal_id))


### Add sun position data
#----------------------------#
setDT(data_det_allday_April)  # Convert combined data back to data.table

# Look up each row in data_det_allday_April using its UTC, and join the matching rows from df_SunCategory
data_det_allday_April <- data_det_allday_April[df_SunCategory,
                                on = .(detection_timestamp_utc = Timestamp),
                                mult = "first"]


### Create final filtered data_det (dusk/night only + monitored receivers)
#----------------------------#
# Use data.table filtering for faster operation on large dataset
data_det <- data_det_allday_April[SunCategory %in% c("dusk", "night") & station_no %in% df_key$station_no]
data_det <- as.data.frame(data_det)  # Convert back for downstream compatibility

cat("Final filtered detections (dusk/night):", nrow(data_det), "\n")

# Memory management
gc()


### Plot detection data
#----------------------------#
### Plot depth distribution sample
ggExtra::ggMarginal({
  data_det_allday_April %>%
    sample_n(min(10000, nrow(data_det_allday_April))) %>%
    ggplot(aes(y = -sensor_value.Cal, x = format(detection_timestamp_utc, "%d"))) +
    geom_jitter(shape = 21, alpha = 0.2) +
    geom_hline(yintercept = param_min_spawn_depth) +
    labs(x = "day of month (April)", y = "depth (m)",
         title = "Raw Walleye detections before depth filter")
}, margins = "y", size = 10, colour = "red")

plots$methods$depth_hist <-
ggplot(data_det_allday_April, aes(x=sensor_value.Cal))+
 geom_histogram(binwidth=1)+
 geom_vline(xintercept = -param_min_spawn_depth)+
 labs(x = "depth (m)", y="detections")


plots$methods$depth_hist


### Plot habitat and receiver map
ggmap::ggmap(HHmap, extent = 'normal') +
  geom_sf(data = shapefile, colour = "black", fill = NA, inherit.aes = FALSE) +
  scale_x_continuous(limits = c(min(data_hab$Start_Longitude) - 0.01, max(data_hab$Start_Longitude) + 0.01)) +
  scale_y_continuous(limits = c(min(data_hab$Start_Latitude) - 0.01, max(data_hab$Start_Latitude) + 0.01)) +
  ylab("Latitude") +
  xlab("Longitude") +
  # Receivers
  geom_point(data = data_rec_locs, aes(x = deploy_long, y = deploy_lat, colour = station_no), shape = 21, size = 8) +
  geom_text(data = slice_head(data_rec_locs, n = 1, by = station_no),
            aes(x = deploy_long, y = deploy_lat, label = station_no), colour = "firebrick", size = 3) +
  # Transects
  geom_point(data = data_rec_locs, aes(x = lon, y = lat, colour = station_no), alpha = 0.5) +
  geom_text(data = slice_head(data_rec_locs, n = 1, by = transect),
            aes(x = lon, y = lat, label = transect), colour = "black", size = 3) +
  theme_bw()



#####Calculate fish movements#################################----
#-------------------------------------------------------------#
cat("Processing fish detections into movements...\n")

### Calculate df_movement
#----------------------------#
# Create movement dataset - with data.table for lag operations
# This is the slowest part of the script due to grouped lag operations on large dataset

# Convert to data.table and select columns (equivalent to dplyr select)
temp_movement <- as.data.table(data_det_allday_April)[, .(
  detection_timestamp_utc,
  To_animal_id = animal_id,
  To_station = station_no,
  To_lat = deploy_lat,
  To_lon = deploy_long,
  mass, length_total, SunCategory
)]

# Add year for grouping
temp_movement[, param_year := year(detection_timestamp_utc)]

# Sort by animal and time (equivalent to arrange)
setorder(temp_movement, To_animal_id, detection_timestamp_utc)

# Create lagged columns to identify previous detection for each fish (within same year)
# For each row: From_station = previous station, From_timestamp = previous detection time, etc.
temp_movement[, c("From_station", "From_animal_id", "From_lat", "From_lon",
                  "From_timestamp", "From_SunCategory") :=
                shift(.SD, 1, type = "lag"),  # Shift values by 1 row within each group
              by = .(To_animal_id, param_year),  # Group by fish and year
              .SDcols = c("To_station", "To_animal_id", "To_lat", "To_lon",
                         "detection_timestamp_utc", "SunCategory")]  # Columns to lag

# Calculate time between consecutive detections
temp_movement[, lagtime := difftime(detection_timestamp_utc, From_timestamp, units = "hours")]

#Look at distribution of lagtimes
func_calculate_exceeding(x=as.numeric(temp_movement$lagtime), thresholds = 1:12) #Lagtime is > 2h, for < 1% of spawning events.

# Create unique movement ID for each continuous period at a station
# When fish changes station (or returns to same station later), moveID increments
# Also splits events when time gap exceeds param_max_lag_hours
temp_movement[, moveID_temp := paste0(From_animal_id, From_station, To_station)]  # Unique string per movement type
temp_movement[, station_change := moveID_temp != shift(moveID_temp, 1, fill = first(moveID_temp))]  # Logical: TRUE when station changes
temp_movement[, long_gap := lagtime >= param_max_lag_hours]  # Logical: TRUE when lag exceeds threshold
temp_movement[, long_gap := fifelse(is.na(long_gap), FALSE, long_gap)]  # Replace NA with FALSE (first detection per fish-year)
temp_movement[, moveID := cumsum(station_change | long_gap)]  # Increment when movement changes OR gap is too long

# Clean up temporary columns and convert back to data.frame
temp_movement[, c("param_year", "moveID_temp", "station_change", "long_gap") := NULL]
df_movement <- as.data.frame(temp_movement)

# Clean up temporary movement objects
rm(temp_movement)

cat("Total movement events (all receivers):", nrow(df_movement), "\n")



### Calculate df_residency
#----------------------------#
# Calculate residency with data.table grouping - FILTER to monitored receivers only
temp_residency <- as.data.table(df_movement) # Convert to data.table for faster aggregation

# Filter to monitored receivers only for residency analysis
temp_residency <- temp_residency[To_station %in% df_key$station_no & From_station %in% df_key$station_no]
cat("Movements between longterm shoreline receivers:", nrow(temp_residency), "\n")

# Filter to same animal only (moveID already handles lag splitting)
temp_residency <- temp_residency[To_animal_id == From_animal_id]

# Use data.table grouping - much faster than dplyr group_by + summarize
temp_residency <- temp_residency[, .(
  detcount = .N,                                    # equivalent to n()
  time_start = min(detection_timestamp_utc),        # FIXED: Use actual timestamps
  time_end = max(detection_timestamp_utc),          # FIXED: Use actual timestamps
  start_SunCategory = first(SunCategory),           
  end_SunCategory = last(SunCategory)               
), by = .(To_animal_id, To_station, moveID)]        # FIXED: Use To_station

# Calculate residence time (same difftime calculation)
temp_residency[, residence := difftime(time_end, time_start, units = "hours")]

### Apply final filters (same logic as original)
temp_residency <- temp_residency[detcount >= param_min_detections &
                                 residence >= param_min_residence_hours &
                                 (start_SunCategory %in% c("dusk", "night") |
                                  end_SunCategory %in% c("dusk", "night"))]

# Convert back to data.frame for compatibility
df_residency <- as.data.frame(temp_residency)
rm(temp_residency)  # Clean up

cat("Residency events identified:", nrow(df_residency), "\n")

 

#####Create spawning datasets##################################----
#-------------------------------------------------------------#

### Create spawning dataset with depth filtering
#----------------------------#
data_spawn <- df_movement %>%
  filter(moveID %in% df_residency$moveID) %>%
  select(detection_timestamp_utc, animal_id = To_animal_id, station_no = To_station, moveID) %>%
  left_join(select(df_residency, moveID, residence, time_start, time_end, detcount),
            by = join_by(moveID)) %>%
  left_join(data_det, by = join_by(detection_timestamp_utc, animal_id, station_no)) %>%
  group_by(moveID) %>%
  mutate(meanDepth = mean(-sensor_value.Cal, na.rm = TRUE)) %>%
  filter(meanDepth > param_min_spawn_depth) %>%  # Apply depth filter
  select(-c(glatos_array, transmitter_codespace, tag_model, tag_serial_number,
            sex, release_group, glatos_caught_date, DST, tag_type)) %>%
  ungroup()

cat("Spawning detections after depth filter:", nrow(data_spawn), "\n")



### Plot detection depths
#----------------------------#
temp_depth_plot <- data_spawn %>%
  ggplot(aes(y = -sensor_value.Cal, x = format(detection_timestamp_utc, "%d"))) +
  geom_jitter(shape = 21, alpha = 0.2) +
  geom_hline(yintercept = param_min_spawn_depth) +
  labs(x = "day of month (April)", y = "depth (m)", title = "Spawning Event Depths")
suppressWarnings(print(temp_depth_plot))



### Check proportion of detections that span past 5m depth
#----------------------------#
temp_deep_events <- data_spawn %>%
  group_by(moveID) %>%
  summarise(
    meanDepth = mean(meanDepth),
    max_depth = min(-sensor_value.Cal, na.rm = TRUE),
    has_deep_detection = any(-sensor_value.Cal < -5, na.rm = TRUE),
    deep_detection_count = sum(-sensor_value.Cal < -5, na.rm = TRUE),
    total_detections = n()
  )



### Summarize data to spawning events
#----------------------------#
df_spawn <- data_spawn %>%
  group_by(animal_id, moveID) %>%
  summarize(station_no = first(station_no),
            deploy_lat = first(deploy_lat),
            deploy_long = first(deploy_long),
            meanDepth = first(meanDepth),
            length_total = first(length_total),
            mass = first(mass),
            residence = first(residence),
            time_start = first(time_start),
            time_end = first(time_end),
            detcount = first(detcount),
            year = year(time_start),
            week = week(time_start),
            .groups = "drop")

cat("Final spawning events:", nrow(df_spawn), "\n")



### Summarize spawning events per receiver
#----------------------------#
df_spawn_plot <- df_spawn %>%
  group_by(station_no) %>%
  summarize(deploy_lat = first(deploy_lat),
            deploy_long = first(deploy_long),
            depth = mean(meanDepth),
            residence_mean = mean(residence),
            residence_sum = sum(residence),
            residence_median = median(residence),
            detcount_mean = mean(detcount),
            detcount_sum = sum(detcount),
            detcount_median = median(detcount),
            countspawn = n(),
            countfish = data.table::uniqueN(animal_id),
            .groups = "drop")



### Plot spawning events map
#----------------------------#
if(exists("HHmap") && exists("data_hab")) {
  temp_spawn_map <- ggmap::ggmap(HHmap, extent = 'normal') +
    # scale_x_continuous(limits = c(min(data_hab$Start_Longitude) - 0.01,
    #                               max(data_hab$Start_Longitude) + 0.01)) +
    # scale_y_continuous(limits = c(min(data_hab$Start_Latitude) - 0.01,
    #                               max(data_hab$Start_Latitude) + 0.01)) +
    ylab("Latitude") +
    xlab("Longitude") +
    geom_point(data = df_spawn_plot,
               aes(x = deploy_long, y = deploy_lat, size = countfish,
                   fill = as.numeric(residence_median)),
               shape = 21) +
    scale_fill_viridis_c() +
    labs(title = "Spawning Events by Station")
  print(temp_spawn_map)
}



#####Summary statistics########################################----
#-------------------------------------------------------------#

### 1. Fish characteristics
#----------------------------#
temp_summary_fish <- data.frame(animal_id = unique(df_spawn$animal_id)) %>%
  left_join(data_fish, by = "animal_id") %>%
  summarize(
    fish_count = n(),
    fork_length_mean = mean(length_fork, na.rm = TRUE),
    fork_length_sd = sd(length_fork, na.rm = TRUE),
    mass_mean = mean(mass, na.rm = TRUE),
    mass_sd = sd(mass, na.rm = TRUE)
  )

cat("\n--- FISH CHARACTERISTICS ---\n")
cat("Total spawning fish:", temp_summary_fish$fish_count, "\n")
cat("Fork length (mm): mean =", round(temp_summary_fish$fork_length_mean, 1),
    ", SD =", round(temp_summary_fish$fork_length_sd, 1), "\n")
cat("Mass (g): mean =", round(temp_summary_fish$mass_mean, 1),
    ", SD =", round(temp_summary_fish$mass_sd, 1), "\n")


### 2. Detection data summary
#----------------------------#
cat("\n--- DETECTION DATA SUMMARY ---\n")
cat("Raw detections loaded:", format(nrow(data_det_raw), big.mark = ","), "\n")
cat("After month/sensor filter (all day):", format(nrow(data_det_allday_April), big.mark = ","), "\n")
cat("Final detections (dusk/night):", format(nrow(data_det), big.mark = ","), "\n")
cat("Spawning detections:", format(nrow(data_spawn), big.mark = ","), "\n")
cat("Proportion classified as spawning:",
    round(100 * nrow(data_spawn) / nrow(data_det_allday_April), 2), "%\n")
cat("Unique fish detected:", n_distinct(data_det$animal_id), "\n")
cat("Unique stations:", n_distinct(data_det$station_no), "\n")
cat("Date range:", format(range(data_det$detection_timestamp_utc)[1], "%Y-%m-%d"),
    "to", format(range(data_det$detection_timestamp_utc)[2], "%Y-%m-%d"), "\n")
cat("Years included:", paste(sort(unique(year(data_det$detection_timestamp_utc))), collapse = ", "), "\n")


### 3. Movement and residency summary
#----------------------------#
temp_summary_events_per_fish <- df_spawn %>%
  group_by(animal_id) %>%
  summarise(n_events = n(), .groups = "drop")

temp_summary_events_per_station <- df_spawn %>%
  group_by(station_no) %>%
  summarise(n_events = n(), .groups = "drop")

cat("\n--- MOVEMENT & RESIDENCY SUMMARY ---\n")
cat("Total movement records:", format(nrow(df_movement), big.mark = ","), "\n")
cat("Residency events (before depth filter):", nrow(df_residency), "\n")
cat("Final spawning events:", nrow(df_spawn), "\n")
cat("Unique spawning fish:", n_distinct(df_spawn$animal_id), "\n")
cat("Unique spawning stations:", n_distinct(df_spawn$station_no), "\n")
cat("Events per fish: mean =", round(mean(temp_summary_events_per_fish$n_events), 2),
    ", SD =", round(sd(temp_summary_events_per_fish$n_events), 2), "\n")
cat("Events per station: mean =", round(mean(temp_summary_events_per_station$n_events), 2),
    ", SD =", round(sd(temp_summary_events_per_station$n_events), 2), "\n")


### 4. Residency metrics
#----------------------------#
cat("\n--- RESIDENCY METRICS ---\n")
cat("Detections per event:\n")
cat("  Mean:", round(mean(df_residency$detcount), 2),
    ", SD:", round(sd(df_residency$detcount), 2), "\n")
cat("  Median:", median(df_residency$detcount), "\n")
cat("  Range:", min(df_residency$detcount), "-", max(df_residency$detcount), "\n")

cat("Residence time (hours):\n")
cat("  Mean:", round(mean(as.numeric(df_residency$residence)), 2),
    ", SD:", round(sd(as.numeric(df_residency$residence)), 2), "\n")
cat("  Median:", round(median(as.numeric(df_residency$residence)), 2), "\n")
cat("  Range:", round(min(as.numeric(df_residency$residence)), 2), "-",
    round(max(as.numeric(df_residency$residence)), 2), "\n")

### 5. Spawn metrics
#----------------------------#
temp_summary_spawn_by_fish <- df_spawn %>% 
 group_by(year, animal_id) %>% 
 count() %>% ungroup() 

cat("\n--- SPAWN METRICS ---\n")
cat("Detections per event:\n")
cat("  Mean:", round(mean(df_spawn$detcount), 2),
    ", SD:", round(sd(df_spawn$detcount), 2), 
    "\n")

cat("  Median:", median(df_spawn$detcount), "\n")
cat("  Range:", min(df_spawn$detcount), "-", max(df_spawn$detcount), "\n")

cat("Residence time (hours):\n")
cat("  Mean:", round(mean(as.numeric(df_spawn$residence)), 2),
    ", SD:", round(sd(as.numeric(df_spawn$residence)), 2), "\n")
cat("  Median:", round(median(as.numeric(df_spawn$residence)), 2), "\n")
cat("  Range:", round(min(as.numeric(df_spawn$residence)), 2), "-",
    round(max(as.numeric(df_spawn$residence)), 2), "\n")

cat("Spawning events per fish per year:\n")
cat("  Mean:", round(mean(temp_summary_spawn_by_fish$n)),
    ", SD:", round(sd(temp_summary_spawn_by_fish$n)), 
    "\n")

### 6. Depth patterns
#----------------------------#
# Calculate depth metrics from spawning events
temp_summary_depth <- data_spawn %>%
  group_by(moveID) %>%
  summarise(
    mean_depth = mean(meanDepth),
    depth_variance = var(-sensor_value.Cal, na.rm = TRUE),
    max_depth = min(-sensor_value.Cal, na.rm = TRUE),
    goes_below_cutoff = any(-sensor_value.Cal < param_min_spawn_depth, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- DEPTH PATTERNS ---\n")
cat("Detection depth (m) for data_det_allday_April:\n")
cat("  Mean:", round(mean(data_det_allday_April$sensor_value.Cal, na.rm = TRUE), 2),
    ", SD:", round(sd(data_det_allday_April$sensor_value.Cal, na.rm = TRUE), 2), "\n")
cat("  Range:", round(min(data_det_allday_April$sensor_value.Cal, na.rm = TRUE), 2), "-",
    round(max(data_det_allday_April$sensor_value.Cal, na.rm = TRUE), 2), "\n")
cat("Detections equal or shallower than a given depth:") 
func_calculate_exceeding(x=-data_det_allday_April$sensor_value.Cal, thresholds=c(param_min_spawn_depth, 0, -1, -2, -3, -4, -5))

cat("Spawning depth (m):\n")
cat("  Mean:", round(mean(data_spawn$meanDepth, na.rm = TRUE), 2),
    ", SD:", round(sd(data_spawn$meanDepth, na.rm = TRUE), 2), "\n")
cat("  Range:", round(min(data_spawn$meanDepth, na.rm = TRUE), 2), "-",
    round(max(data_spawn$meanDepth, na.rm = TRUE), 2), "\n")
cat("spawning dets deeper than param_min_spawn:", round(100 * mean(temp_summary_depth$goes_below_cutoff), 1), "%\n")
cat("  Mean depth variance (m²):", round(mean(temp_summary_depth$depth_variance, na.rm = TRUE), 2), "\n")


### 7. Temporal patterns
#----------------------------#
temp_summary_events_by_year <- table(df_spawn$year)
temp_summary_events_by_week <- table(df_spawn$week)
temp_summary_peak_week <- names(sort(temp_summary_events_by_week, decreasing = TRUE))[1]

cat("\n--- TEMPORAL PATTERNS ---\n")
cat("Peak spawning week:", temp_summary_peak_week, "\n")
cat("\nEvents by year:\n")
print(temp_summary_events_by_year)
cat("\nEvents by week:\n")
print(temp_summary_events_by_week)


### 8. Spawning duration and detection gaps
#----------------------------#
temp_summary_timing <- data_spawn %>%
  group_by(moveID) %>%
  arrange(detection_timestamp_utc) %>%
  summarise(
    duration_hours = as.numeric(difftime(max(detection_timestamp_utc),
                                         min(detection_timestamp_utc),
                                         units = "hours")),
    total_detections = n(),
    avg_gap_min = mean(diff(as.numeric(detection_timestamp_utc)) / 60, na.rm = TRUE),
    max_gap_min = max(diff(as.numeric(detection_timestamp_utc)) / 60, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- SPAWNING EVENT TIMING ---\n")
cat("Duration (hours):\n")
cat("  Mean:", round(mean(temp_summary_timing$duration_hours, na.rm = TRUE), 2),
    ", SD:", round(sd(temp_summary_timing$duration_hours, na.rm = TRUE), 2), "\n")
cat("  Range:", round(min(temp_summary_timing$duration_hours, na.rm = TRUE), 2), "-",
    round(max(temp_summary_timing$duration_hours, na.rm = TRUE), 2), "\n")

cat("Detection gaps (minutes):\n")
cat("  Mean gap per event:", round(mean(temp_summary_timing$avg_gap_min, na.rm = TRUE), 1),
    ", SD:", round(sd(temp_summary_timing$avg_gap_min, na.rm = TRUE), 1), "\n")
cat("  Mean max gap per event:", round(mean(temp_summary_timing$max_gap_min, na.rm = TRUE), 1),
    ", SD:", round(sd(temp_summary_timing$max_gap_min, na.rm = TRUE), 1), "\n")
cat("  Max gap across all events:", round(max(temp_summary_timing$max_gap_min, na.rm = TRUE), 1), "\n")


### 9. Diel patterns
#----------------------------#
temp_summary_start_times <- table(df_residency$start_SunCategory)
temp_summary_end_times <- table(df_residency$end_SunCategory)

cat("\n--- DIEL PATTERNS ---\n")
cat("Residency start times:\n")
print(temp_summary_start_times)
cat("\nResidency end times:\n")
print(temp_summary_end_times)

# Calculate proportion of shallow-water detections by diel period
temp_summary_diel_depth <- data_det_allday_April %>%
  mutate(shallow_depth = -sensor_value.Cal < param_min_spawn_depth) %>%
  group_by(SunCategory) %>%
  summarise(
    total_detections = n(),
    shallow_detections = sum(shallow_depth, na.rm = TRUE),
    proportion_shallow = shallow_detections / total_detections,
    .groups = 'drop'
  ) %>%
  arrange(desc(proportion_shallow))

cat("\nShallow-water detections (<2m depth) by diel period:\n")
print(temp_summary_diel_depth)

temp_summary_diel_depth %>% 
 summarize(total_proportion_shallow = stats::weighted.mean(proportion_shallow, w=total_detections))

### Clean up temporary objects
#----------------------------#
rm(list = ls(pattern = "^temp_"))
rm(param_longterm_start, param_longterm_end, param_min_detections,
   param_min_residence_hours, param_max_lag_hours, param_min_spawn_depth, param_target_month)


