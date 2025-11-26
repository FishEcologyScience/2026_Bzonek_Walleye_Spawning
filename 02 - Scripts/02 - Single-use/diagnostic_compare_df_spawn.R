## --------------------------------------------------------------#
## Script name: diagnostic_compare_df_spawn
##
## Purpose of script:
##    Compare old vs new df_spawn to identify meaningful differences
##    Beyond moveID changes, investigate why row counts differ
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2025-10-03
##
## --------------------------------------------------------------#


#####Save and compare datasets ###################################----
#-------------------------------------------------------------#
#Save old df_spawn before changes
#saveRDS(df_spawn, "df_spawn_2025-09-13_1_Base.rds")
saveRDS(df_spawn, "df_spawn_2025-10-03_1.rds")

#Load old df_spawn after changes
#df_spawn_original <- readRDS("04 - Outputs/df_spawn/df_spawn_2025-09-13_1_Base.rds")
df_spawn_original <- readRDS("04 - Outputs/df_spawn/df_spawn_2025-10-03_1.rds")

#Test for changes
identical(df_spawn, df_spawn_original)
df_spawn %>% 
 mutate(mass=mass+1) %>% 
 identical(df_spawn_original)

anti_join(df_spawn, df_spawn_original)

#check df_movement
df_movement_original <- df_movement
identical(df_movement, as.data.frame(df_movement_original))
str(df_movement); str(df_movement_original)



#####Run diagnostics comparing datasets ##########################----
#-------------------------------------------------------------#

# Assumes df_spawn_original and df_spawn (new) exist in environment

### Basic dimension comparison
#----------------------------#
cat("=== DIMENSION COMPARISON ===\n")
cat("Old df_spawn rows:", nrow(df_spawn_original), "\n")
cat("New df_spawn rows:", nrow(df_spawn), "\n")
cat("Difference:", nrow(df_spawn) - nrow(df_spawn_original), "rows\n\n")

cat("Old df_spawn columns:", ncol(df_spawn_original), "\n")
cat("New df_spawn columns:", ncol(df_spawn), "\n\n")

### Compare by animal_id
#----------------------------#
cat("=== ANIMAL_ID COMPARISON ===\n")
temp_old_fish <- unique(df_spawn_original$animal_id)
temp_new_fish <- unique(df_spawn$animal_id)

cat("Unique fish in old:", length(temp_old_fish), "\n")
cat("Unique fish in new:", length(temp_new_fish), "\n")
cat("Fish only in old:", sum(!temp_old_fish %in% temp_new_fish), "\n")
cat("Fish only in new:", sum(!temp_new_fish %in% temp_old_fish), "\n\n")

### Compare by station
#----------------------------#
cat("=== STATION COMPARISON ===\n")
temp_old_stations <- table(df_spawn_original$station_no)
temp_new_stations <- table(df_spawn$station_no)

cat("Stations in old:", length(temp_old_stations), "\n")
cat("Stations in new:", length(temp_new_stations), "\n\n")

cat("Event count changes by station:\n")
temp_station_comparison <- data.frame(
  station = names(temp_new_stations),
  old_count = as.numeric(temp_old_stations[names(temp_new_stations)]),
  new_count = as.numeric(temp_new_stations),
  difference = as.numeric(temp_new_stations) - as.numeric(temp_old_stations[names(temp_new_stations)])
)
temp_station_comparison[order(-abs(temp_station_comparison$difference)), ]
print(temp_station_comparison)

### Compare key metrics (using time_start for unique matching)
#----------------------------#
cat("\n=== KEY METRIC COMPARISON ===\n")

# Create unique event identifiers using time_start (should be unique per event)
temp_old_events <- df_spawn_original %>%
  mutate(event_key = paste(animal_id, station_no, time_start, sep = "_")) %>%
  select(event_key, animal_id, station_no, year, week, time_start, time_end,
         meanDepth_old = meanDepth, residence_old = residence,
         detcount_old = detcount, moveID_old = moveID)

temp_new_events <- df_spawn %>%
  mutate(event_key = paste(animal_id, station_no, time_start, sep = "_")) %>%
  select(event_key, animal_id, station_no, year, week, time_start, time_end,
         meanDepth_new = meanDepth, residence_new = residence,
         detcount_new = detcount, moveID_new = moveID)

# Full join on event_key
temp_comparison <- full_join(temp_old_events, temp_new_events, by = "event_key")

cat("Total unique event_keys in comparison:", nrow(temp_comparison), "\n")
cat("Events in both (matched on animal_id/station/time_start):",
    sum(!is.na(temp_comparison$meanDepth_old) & !is.na(temp_comparison$meanDepth_new)), "\n")
cat("Events only in old:", sum(!is.na(temp_comparison$meanDepth_old) & is.na(temp_comparison$meanDepth_new)), "\n")
cat("Events only in new:", sum(is.na(temp_comparison$meanDepth_old) & !is.na(temp_comparison$meanDepth_new)), "\n\n")

# Analyze matched events
temp_matched <- temp_comparison %>%
  filter(!is.na(meanDepth_old) & !is.na(meanDepth_new))

if(nrow(temp_matched) > 0) {
  cat("Among", nrow(temp_matched), "matched events:\n")
  cat("  Identical moveID:", sum(temp_matched$moveID_old == temp_matched$moveID_new, na.rm = TRUE), "\n")
  cat("  Different moveID:", sum(temp_matched$moveID_old != temp_matched$moveID_new, na.rm = TRUE), "\n")
  cat("  Different meanDepth:", sum(abs(temp_matched$meanDepth_old - temp_matched$meanDepth_new) > 0.01, na.rm = TRUE), "\n")
  cat("  Different residence:", sum(abs(as.numeric(temp_matched$residence_old) - as.numeric(temp_matched$residence_new)) > 0.01, na.rm = TRUE), "\n")
  cat("  Different detcount:", sum(temp_matched$detcount_old != temp_matched$detcount_new, na.rm = TRUE), "\n\n")
}

# Show events only in new
temp_only_new <- temp_comparison %>%
  filter(is.na(meanDepth_old) & !is.na(meanDepth_new))

if(nrow(temp_only_new) > 0) {
  cat("Events only in NEW df_spawn (first 10):\n")
  print(temp_only_new %>%
          select(animal_id = animal_id.y, station_no = station_no.y, time_start = time_start.y,
                 time_end = time_end.y, moveID = moveID_new, detcount = detcount_new) %>%
          head(10))
  cat("\n")
}

# Show events only in old
temp_only_old <- temp_comparison %>%
  filter(!is.na(meanDepth_old) & is.na(meanDepth_new))

if(nrow(temp_only_old) > 0) {
  cat("Events only in OLD df_spawn (first 10):\n")
  print(temp_only_old %>%
          select(animal_id = animal_id.x, station_no = station_no.x, time_start = time_start.x,
                 time_end = time_end.x, moveID = moveID_old, detcount = detcount_old) %>%
          head(10))
  cat("\n")
}

### Check for duplicate events (same fish, station, time)
#----------------------------#
cat("\n=== DUPLICATE CHECK ===\n")
temp_old_duplicates <- df_spawn_original %>%
  group_by(animal_id, station_no, year, week) %>%
  filter(n() > 1) %>%
  ungroup()

temp_new_duplicates <- df_spawn %>%
  group_by(animal_id, station_no, year, week) %>%
  filter(n() > 1) %>%
  ungroup()

cat("Old df_spawn duplicate events (same fish/station/year/week):", nrow(temp_old_duplicates), "\n")
cat("New df_spawn duplicate events (same fish/station/year/week):", nrow(temp_new_duplicates), "\n\n")

if(nrow(temp_new_duplicates) > 0) {
  cat("Example new duplicates:\n")
  print(head(temp_new_duplicates %>%
               select(animal_id, station_no, year, week, time_start, time_end, moveID) %>%
               arrange(animal_id, station_no, time_start), 20))
}

### Summary statistics comparison
#----------------------------#
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Old summary:\n")
cat("  Mean residence:", round(mean(df_spawn_original$residence), 2), "hours\n")
cat("  Mean detcount:", round(mean(df_spawn_original$detcount), 2), "\n")
cat("  Mean depth:", round(mean(df_spawn_original$meanDepth, na.rm = TRUE), 2), "m\n\n")

cat("New summary:\n")
cat("  Mean residence:", round(mean(df_spawn$residence), 2), "hours\n")
cat("  Mean detcount:", round(mean(df_spawn$detcount), 2), "\n")
cat("  Mean depth:", round(mean(df_spawn$meanDepth, na.rm = TRUE), 2), "m\n")

# Clean up
rm(list = ls(pattern = "^temp_"))
cat("\nDiagnostic complete.\n")

