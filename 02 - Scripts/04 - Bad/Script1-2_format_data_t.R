## --------------------------------------------------------------#
## Script name: Script1-2_format_data_t
##
## Purpose of script: 
##    Clean raw telemetry data with improved joining operations (telemetry workflow)
##    Fix residency calculation and movement analysis issues
##    
## Dependencies: 
##    - Script0-1_load_packages.R (packages)
##    - Raw telemetry data files in 01 - Data/
##
## Author: Paul Bzonek 
##
## Date Created: 2024-09-07
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   - Fixed residency calculation to use To_station instead of From_station
##   - Corrected time calculation to use actual detection timestamps
##   - Applied depth filter logic more consistently
##   - Improved data validation and error checking
##   2025-01-11: Renamed from Script1-2_format_data_t_v2.R
## --------------------------------------------------------------#

### Required packages (using :: notation for non-tidyverse)
#----------------------------#
# readxl, sf, ggmap, ggExtra, data.table will use :: notation

###Import data
#----------------------------#
# PROGRESS TRACKING: Load large detections dataset with file size info
cat("Loading detections data (", 
    round(file.size("01 - Data/HH_detections_clipped_filtered_2015-2023_good.rds")/1024/1024, 1), 
    "MB) - this may take a moment...\n")
data_det_raw <- readRDS("01 - Data/HH_detections_clipped_filtered_2015-2023_good.rds")
cat("✓ Loaded", format(nrow(data_det_raw), big.mark = ","), "detection records\n")

# Load remaining files
data_rec_locs_raw <- read.csv("01 - Data/GLATOS_receiverLocations_20240122_234632.csv")
data_fish <- readxl::read_excel("01 - Data/HH_Fish_Workbook_Jun2024.xlsx")
df_key <- readxl::read_excel("01 - Data/receiver_transect_key2.xlsx")
df_SunCategory <- readRDS("01 - Data/data_det_raw_SunPosition_2024-07-12.rds")


### Set parameters
#----------------------------#
param_longterm_start <- as.POSIXct("2017-01-01")
param_longterm_end <- as.POSIXct("2023-01-01")
param_min_detections <- 3
param_min_residence_hours <- 0.32
param_max_lag_hours <- 12
param_min_spawn_depth <- -2
param_target_month <- "04"

#####Format data##################################################----
#-------------------------------------------------------------#

### Process misc data
#----------------------------#
# Receiver-Habitat Key
df_key <- df_key %>% 
  mutate(station_no = as.factor(station), 
         transect = as.factor(transect))

# Fish data
data_fish <- data_fish %>% 
  filter(COMMON_NAME_E == "Walleye", !is.na(`Pinger Full ID`)) %>% 
  select(animal_id = `Pinger Full ID`, 
         length_standard = `Standard (mm)`,
         length_fork = `Fork (mm)`,
         length_total = `Total (mm)`,
         mass = `Mass (g)`) %>% 
  mutate(length_standard = as.numeric(length_standard),
         length_fork = as.numeric(length_fork),
         length_total = as.numeric(length_total))

### Format receiver data
#----------------------------#
# Hamilton Array only
data_rec_locs_raw <- data_rec_locs_raw %>% 
  filter(glatos_array == "HAM")

# Format receiver datatable
data_rec_locs_raw <- data_rec_locs_raw %>% 
  rowwise() %>% 
  mutate(ins_serial_no = as.numeric(ins_serial_no),
         ins_model_no = as.factor(ins_model_no),
         station_no = as.factor(station_no),
         receiver_sn = paste(ins_model_no, ins_serial_no, sep = "-"),
         # Fix date formats
         deploy_date_time = as.POSIXct(deploy_date_time, format = "%Y-%m-%d %H:%M:%OS"),
         recover_date_time = as.POSIXct(recover_date_time, format = "%Y-%m-%d %H:%M:%OS"),
         # Overwrite Inf values
         recover_date_time2 = if_else(is.infinite(recover_date_time),
                                     true = as.POSIXct(Sys.time(), format = "%Y-%m-%d %H:%M:%OS"),
                                     false = as.POSIXct(recover_date_time))) %>% 
  # Keep columns of interest
  select(receiver_sn, ins_serial_no, station_no, deploy_lat, deploy_long, ins_model_no, bottom_depth,
         deploy_date_time, recover_date_time, recover_date_time2, comments) %>% 
  filter(!is.na(ins_serial_no)) %>% 
  ungroup()

# Clean-up receiver dataframe with improved date handling
data_rec_locs_raw <- data_rec_locs_raw %>%
  mutate(deployDate = strftime(deploy_date_time, tz = "EST", format = "%Y-%m-%d"),
         deployDate = as.POSIXct(deployDate, tz = "EST", format = "%Y-%m-%d"),
         recoverDate = strftime(recover_date_time, tz = "EST", format = "%Y-%m-%d"),
         recoverDate = as.POSIXct(recoverDate, tz = "EST", format = "%Y-%m-%d")) %>% 
  group_by(ins_serial_no) %>% 
  mutate(receiver_deployment = row_number()) %>% 
  group_by(station_no) %>% 
  mutate(station_rec_count = n_distinct(ins_serial_no)) %>% 
  select(station_no, receiver_sn, deploy_lat, deploy_long, 
         bottom_depth, ins_model_no, ins_serial_no, 
         deploy_date_time, recover_date_time, recover_date_time2, 
         deployDate, recoverDate, receiver_deployment, station_rec_count, comments) %>% 
  ungroup()

# Keep receivers that last for the full duration
temp_df_longterm <- data_rec_locs_raw %>% 
  group_by(station_no) %>% 
  summarize(firstdeploy = min(deployDate), 
            lastrecover = max(recoverDate, na.rm = TRUE)) %>% 
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
  data_rec_locs <- left_join(data_rec_locs, df_key) %>%
    left_join(df_hab, by = c("transect" = "PointID"))
  cat("Habitat data joined successfully\n")
} else {
  warning("df_hab not found - run Script1-1 first")
  data_rec_locs <- left_join(data_rec_locs, df_key)
}

# Clean-up temporary objects
rm(list = ls(pattern = "^temp_"))

### Format raw detection data
#----------------------------#
# Filter data and calculate depth
data_det_raw <- data_det_raw %>% 
  filter(common_name_e == "Walleye") %>% 
  mutate(sensor_value.Cal = (as.numeric(Intercept) + (as.numeric(sensor_value) * as.numeric(Slope))))

cat("Raw detections kept:", nrow(data_det_raw), "\n")



### Format detection data (full dataset before dusk/night filter)
#----------------------------#
data_det_temp <- data_det_raw %>% 
  filter(format(detection_timestamp_EST, "%m") == param_target_month,
         station_no %in% df_key$station_no)

# Add sun position data
data_det_temp <- cbind(data_det_temp, df_SunCategory)

# Keep sensor data (but don't filter by dusk/night yet)
data_det_temp <- data_det_temp %>% 
  filter(!is.na(sensor_value))

# Add fish data
data_det_temp <- left_join(data_det_temp, data_fish, by = join_by(animal_id))

cat("Filtered detections for selected recievers in april:", nrow(data_det_temp), "\n")

# Memory management
gc()

# Plot depth distribution sample
  ggExtra::ggMarginal({
    data_det_temp %>% 
      sample_n(min(10000, nrow(data_det_temp))) %>% 
      ggplot(aes(y = -sensor_value.Cal, x = format(detection_timestamp_utc, "%d"))) +
      geom_jitter(shape = 21, alpha = 0.2) +
      geom_hline(yintercept = param_min_spawn_depth) +
      labs(x = "day of month (April)", y = "depth (m)", title = "Walleye detection depth") 
  }, margins = "y", size = 10, colour = "red")


#####Summarize fish movements##################################----
#-------------------------------------------------------------#
cat("Processing fish movements...\n")

# Create movement dataset using original dplyr approach for correct grouping
df_movement <- data_det_temp %>% 
  select(detection_timestamp_utc, To_animal_id = animal_id, 
         To_station = station_no, To_lat = deploy_lat, To_lon = deploy_long,
         mass, length_total, SunCategory) %>% 
  mutate(param_year = year(detection_timestamp_utc)) %>%
  arrange(To_animal_id, detection_timestamp_utc) %>% 
  group_by(To_animal_id, param_year) %>%
  mutate(From_station = lag(To_station), 
         From_animal_id = lag(To_animal_id), 
         From_lat = lag(To_lat), 
         From_lon = lag(To_lon), 
         From_timestamp = lag(detection_timestamp_utc),
         From_SunCategory = lag(SunCategory),
         lagtime = as.numeric(difftime(detection_timestamp_utc, From_timestamp, units = "hours")),
         moveID = paste0(From_animal_id, From_station, To_station),
         moveID = cumsum(moveID != lag(moveID, default = first(moveID)))) %>% 
  ungroup() %>%
  select(-param_year)

cat("Movement dataset created with", nrow(df_movement), "records\n")


# Convert to data.table for residency calculations
# SPEED OPTIMIZATION: Calculate residency using data.table for faster group operations
# data.table's grouping is significantly faster than dplyr for large datasets
temp_residency_dt <- data.table::as.data.table(df_movement)

# Filter valid movements
# Filter valid movements using data.table's fast filtering
temp_residency_dt <- temp_residency_dt[
  To_animal_id == From_animal_id & lagtime < param_max_lag_hours
]

# Group and summarize residency events
# SPEED OPTIMIZATION: Use data.table's efficient group operations
# .SD operations in data.table are optimized for aggregation tasks
df_residency <- temp_residency_dt[, .(
  detcount = .N,  # data.table's optimized count function
  time_start = min(detection_timestamp_utc),
  time_end = max(detection_timestamp_utc), 
  residence = as.numeric(difftime(max(detection_timestamp_utc), min(detection_timestamp_utc), units = "hours")),
  start_SunCategory = SunCategory[1L],  # data.table's efficient first/last indexing
  end_SunCategory = SunCategory[.N]     # using .N for last element is faster than last()
), by = .(To_animal_id, To_station, moveID)]

# Apply quality filters and convert back
# Apply quality filters using data.table's fast filtering
df_residency <- df_residency[
  detcount >= param_min_detections & 
  residence >= param_min_residence_hours &
  (start_SunCategory %in% c("dusk", "night") | end_SunCategory %in% c("dusk", "night"))
]

# Convert back to tibble for compatibility
df_residency <- dplyr::as_tibble(df_residency)

cat("Residency events identified:", nrow(df_residency), "\n")

# Create final filtered data_det (dusk/night only) for depth calculations
data_det <- data_det_temp %>% 
  filter(SunCategory %in% c("dusk", "night"))

cat("Final filtered detections (dusk/night):", nrow(data_det), "\n")

# Filter movements for spawning analysis
data_spawn <- df_movement %>% 
  filter(moveID %in% df_residency$moveID) %>% 
  select(detection_timestamp_utc, animal_id = To_animal_id, station_no = To_station, moveID)

# Join with residency data
data_spawn <- data_spawn %>% 
  left_join(select(df_residency, moveID, residence, time_start, time_end, detcount), 
            by = join_by(moveID))

# Join with detection data
data_spawn <- data_spawn %>% 
  left_join(data_det, by = join_by(detection_timestamp_utc, animal_id, station_no))

# Calculate depths and apply filters
data_spawn <- data_spawn %>% 
  group_by(moveID) %>% 
  mutate(meanDepth = mean(-sensor_value.Cal, na.rm = TRUE)) %>% 
  filter(meanDepth > param_min_spawn_depth) %>%  # Apply depth filter
  ungroup()

# Clean up columns
data_spawn <- data_spawn %>% 
  select(-c(glatos_array, transmitter_codespace, tag_model, tag_serial_number, 
            sex, release_group, glatos_caught_date, DST, tag_type))

cat("Spawning detections after depth filter:", nrow(data_spawn), "\n")

# Plot detection depths
  temp_depth_plot <- data_spawn %>% 
    ggplot(aes(y = -sensor_value.Cal, x = format(detection_timestamp_utc, "%d"))) +
    geom_jitter(shape = 21, alpha = 0.2) +
    geom_hline(yintercept = param_min_spawn_depth) +
    labs(x = "day of month (April)", y = "depth (m)", title = "Spawning Event Depths")
  print(temp_depth_plot)

# Summarize data to spawning events
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

# Summarize spawning events per receiver
df_spawn_plot <- df_spawn %>% 
  group_by(station_no) %>% 
  summarize(deploy_lat = mean(deploy_lat),
            deploy_long = mean(deploy_long),
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


# Plot spawning events if map data available
if(exists("HHmap") && exists("data_hab")) {
  temp_spawn_map <- ggmap::ggmap(HHmap, extent = 'normal') +
    scale_x_continuous(limits = c(min(data_hab$Start_Longitude) - 0.01, 
                                  max(data_hab$Start_Longitude) + 0.01)) +
    scale_y_continuous(limits = c(min(data_hab$Start_Latitude) - 0.01, 
                                  max(data_hab$Start_Latitude) + 0.01)) +
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

### Summarize fish characteristics
#----------------------------#
temp_fish_summary <- data.frame(animal_id = unique(df_spawn$animal_id)) %>% 
  left_join(data_fish, by = "animal_id") %>% 
  summarize(FL_mean = mean(length_fork, na.rm = TRUE),
            FL_SD = sd(length_fork, na.rm = TRUE),
            Mass_mean = mean(mass, na.rm = TRUE),
            Mass_SD = sd(mass, na.rm = TRUE))

cat("\n=== FISH SUMMARY ===\n")
print(temp_fish_summary)

cat("\n=== RESIDENCY SUMMARY ===\n")
temp_residency_summary <- df_spawn %>% 
  summarize(
    events_total = n(),
    fish_total = n_distinct(animal_id),
    stations_total = n_distinct(station_no),
    years_total = n_distinct(year),
    residence_median = median(residence),
    residence_mean = mean(residence),
    detcount_median = median(detcount),
    detcount_mean = mean(detcount)
  )
print(temp_residency_summary)

### Comprehensive analysis summary
#----------------------------#
cat("COMPREHENSIVE ANALYSIS SUMMARY\n")

# Calculate summary statistics directly
temp_events_by_year <- table(df_spawn$year)
temp_events_by_week <- table(df_spawn$week)
temp_peak_week <- names(sort(temp_events_by_week, decreasing = TRUE))[1]
temp_residency_start_times <- table(df_residency$start_SunCategory)
temp_residency_end_times <- table(df_residency$end_SunCategory)

cat("\n--- DATA OVERVIEW ---\n")
cat("Raw detections loaded:", nrow(data_det_raw), "\n")
cat("After month/station filter:", nrow(data_det_temp), "\n") 
cat("Final detections (dusk/night):", nrow(data_det), "\n")
cat("Unique fish:", n_distinct(data_det$animal_id), "\n")
cat("Unique stations:", n_distinct(data_det$station_no), "\n")
cat("Date range:", as.character(range(data_det$detection_timestamp_utc)[1]), 
    "to", as.character(range(data_det$detection_timestamp_utc)[2]), "\n")
cat("Years included:", paste(sort(unique(year(data_det$detection_timestamp_utc))), collapse = ", "), "\n")

cat("\n--- MOVEMENT ANALYSIS ---\n")
cat("Total movement records:", nrow(df_movement), "\n")
cat("Residency events (before depth filter):", nrow(df_residency), "\n")
cat("Final spawning events:", nrow(df_spawn), "\n")
cat("Unique spawning fish:", n_distinct(df_spawn$animal_id), "\n")
cat("Unique spawning stations:", n_distinct(df_spawn$station_no), "\n")
cat("Events per fish (avg):", round(nrow(df_spawn) / n_distinct(df_spawn$animal_id), 2), "\n")
cat("Events per station (avg):", round(nrow(df_spawn) / n_distinct(df_spawn$station_no), 2), "\n")

cat("\n--- QUALITY METRICS ---\n")
cat("Detections per event (mean):", round(mean(df_residency$detcount), 2), "\n")
cat("Detections per event (median):", median(df_residency$detcount), "\n")
cat("Residence time hours (mean):", round(mean(as.numeric(df_residency$residence)), 2), "\n")
cat("Residence time hours (median):", round(median(as.numeric(df_residency$residence)), 2), "\n")
cat("Residence time range:", range(as.numeric(df_residency$residence))[1], 
    "to", range(as.numeric(df_residency$residence))[2], "hours\n")
cat("Spawning depth range:", range(data_spawn$meanDepth, na.rm = TRUE)[1], 
    "to", range(data_spawn$meanDepth, na.rm = TRUE)[2], "m\n")
cat("Mean spawning depth:", round(mean(data_spawn$meanDepth, na.rm = TRUE), 2), "m\n")

cat("\n--- TEMPORAL PATTERNS ---\n")
cat("Peak spawning week:", temp_peak_week, "\n")
cat("Events by year:\n")
print(temp_events_by_year)
cat("Events by week:\n") 
print(temp_events_by_week)

cat("\n--- SUN CATEGORY PATTERNS ---\n")
cat("Residency start times:\n")
print(temp_residency_start_times)
cat("Residency end times:\n")
print(temp_residency_end_times)


### Clean up temporary objects
#----------------------------#
rm(list = ls(pattern = "^temp_"))
rm(param_longterm_start, param_longterm_end, param_min_detections, 
   param_min_residence_hours, param_max_lag_hours, param_min_spawn_depth, param_target_month)

cat("Cleanup complete.\n")
