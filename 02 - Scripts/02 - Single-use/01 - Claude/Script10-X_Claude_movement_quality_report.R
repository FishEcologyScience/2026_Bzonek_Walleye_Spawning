## --------------------------------------------------------------#
## Script name: Script10-X_Claude_movement_quality_report
##
## Purpose of script:
##    Analyze movement data quality and receiver usage patterns
##    Validate improvements from comprehensive receiver monitoring
##    Create movement completeness metrics and receiver usage summaries
##
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2025-10-02
##
## --------------------------------------------------------------#
## Modification Notes:
##   - Step 2: Movement completeness metrics
##   - Step 3: Receiver usage summary
##   - Step 4: Validation checks (old vs new results)
## --------------------------------------------------------------#

### Required packages (using :: notation for non-tidyverse)
#----------------------------#
library(tidyverse)
# Additional packages will use :: notation

### Step 2: Movement Completeness Metrics
#----------------------------#
cat("=== STEP 2: Movement Completeness Metrics ===\n")

# Analyze receiver coverage and data quality
temp_receiver_stats <- df_movement %>%
  # Remove rows with missing station data
  filter(!is.na(To_station), !is.na(From_station)) %>%
  group_by(To_station) %>%
  summarise(
    total_movements_to = n(),
    unique_fish_to = n_distinct(To_animal_id),
    .groups = "drop"
  ) %>%
  full_join(
    df_movement %>%
      filter(!is.na(To_station), !is.na(From_station)) %>%
      group_by(From_station) %>%
      summarise(
        total_movements_from = n(),
        unique_fish_from = n_distinct(From_animal_id),
        .groups = "drop"
      ),
    by = c("To_station" = "From_station")
  ) %>%
  mutate(
    total_movements_to = replace_na(total_movements_to, 0),
    total_movements_from = replace_na(total_movements_from, 0),
    unique_fish_to = replace_na(unique_fish_to, 0),
    unique_fish_from = replace_na(unique_fish_from, 0),
    total_movements = total_movements_to + total_movements_from,
    unique_fish = pmax(unique_fish_to, unique_fish_from, na.rm = TRUE),
    is_monitored = To_station %in% df_key$station_no
  ) %>%
  arrange(desc(total_movements))

# Movement completeness by fish
temp_fish_completeness <- df_movement %>%
  filter(!is.na(To_animal_id)) %>%
  group_by(To_animal_id) %>%
  summarise(
    total_movements = n(),
    unique_stations_visited = n_distinct(c(To_station, From_station), na.rm = TRUE),
    monitored_movements = sum(To_station %in% df_key$station_no & From_station %in% df_key$station_no, na.rm = TRUE),
    unmonitored_movements = total_movements - monitored_movements,
    completeness_ratio = monitored_movements / total_movements,
    .groups = "drop"
  ) %>%
  arrange(desc(total_movements))

cat("Receiver usage statistics:\n")
print(head(temp_receiver_stats, 15))

cat("\nFish movement completeness (top 10 most active):\n")
print(head(temp_fish_completeness, 10))

cat("\nOverall movement completeness:\n")
cat("Total movements:", nrow(df_movement), "\n")
cat("Movements between monitored receivers:", sum(temp_fish_completeness$monitored_movements), "\n")
cat("Movements involving unmonitored receivers:", sum(temp_fish_completeness$unmonitored_movements), "\n")
cat("Overall completeness ratio:",
    round(sum(temp_fish_completeness$monitored_movements) / nrow(df_movement), 3), "\n")

### Step 3: Receiver Usage Summary
#----------------------------#
cat("\n=== STEP 3: Receiver Usage Summary ===\n")

# Identify heavily used unmonitored receivers
temp_unmonitored_usage <- temp_receiver_stats %>%
  filter(!is_monitored, total_movements > 0) %>%
  arrange(desc(total_movements))

cat("Top unmonitored receivers by movement activity:\n")
print(head(temp_unmonitored_usage, 10))

# Monthly movement patterns
if("detection_timestamp_utc" %in% names(df_movement)) {
  temp_monthly_patterns <- df_movement %>%
    filter(!is.na(detection_timestamp_utc)) %>%
    mutate(
      month = format(detection_timestamp_utc, "%Y-%m"),
      movement_type = case_when(
        To_station %in% df_key$station_no & From_station %in% df_key$station_no ~ "monitored",
        !To_station %in% df_key$station_no | !From_station %in% df_key$station_no ~ "partial_unmonitored",
        TRUE ~ "other"
      )
    ) %>%
    group_by(month, movement_type) %>%
    summarise(movement_count = n(), .groups = "drop") %>%
    pivot_wider(names_from = movement_type, values_from = movement_count, values_fill = 0)

  cat("\nMonthly movement patterns:\n")
  print(head(temp_monthly_patterns, 12))
}

# Network connectivity analysis
temp_network_summary <- df_movement %>%
  filter(!is.na(To_station), !is.na(From_station)) %>%
  group_by(From_station, To_station) %>%
  summarise(
    movement_count = n(),
    unique_fish = n_distinct(To_animal_id),
    .groups = "drop"
  ) %>%
  mutate(
    from_monitored = From_station %in% df_key$station_no,
    to_monitored = To_station %in% df_key$station_no,
    connection_type = case_when(
      from_monitored & to_monitored ~ "monitored_to_monitored",
      from_monitored & !to_monitored ~ "monitored_to_unmonitored",
      !from_monitored & to_monitored ~ "unmonitored_to_monitored",
      TRUE ~ "unmonitored_to_unmonitored"
    )
  ) %>%
  arrange(desc(movement_count))

cat("\nNetwork connectivity summary:\n")
temp_connection_summary <- temp_network_summary %>%
  group_by(connection_type) %>%
  summarise(
    unique_connections = n(),
    total_movements = sum(movement_count),
    total_fish = sum(unique_fish),
    .groups = "drop"
  )
print(temp_connection_summary)

### Step 4: Validation Checks
#----------------------------#
cat("\n=== STEP 4: Validation Checks ===\n")

# Compare with previous results if available
if(exists("df_residency")) {
  cat("Current residency analysis:\n")
  cat("Total residency events:", nrow(df_residency), "\n")

  if("To_station" %in% names(df_residency)) {
    temp_residency_stations <- df_residency %>%
      group_by(To_station) %>%
      summarise(
        residency_events = n(),
        unique_fish = n_distinct(To_animal_id),
        .groups = "drop"
      ) %>%
      arrange(desc(residency_events))

    cat("Residency events by station (top 10):\n")
    print(head(temp_residency_stations, 10))
  }
}

# Data quality checks
temp_quality_checks <- list(
  "Missing station data in movements" = sum(is.na(df_movement$To_station) | is.na(df_movement$From_station)),
  "Self-movements (same station)" = sum(df_movement$To_station == df_movement$From_station, na.rm = TRUE),
  "Movements with missing timestamps" = sum(is.na(df_movement$detection_timestamp_utc)),
  "Movements with invalid lag times" = sum(df_movement$lagtime < 0 | df_movement$lagtime > 24*7, na.rm = TRUE)
)

cat("\nData quality checks:\n")
for(check_name in names(temp_quality_checks)) {
  cat(paste0(check_name, ": ", temp_quality_checks[[check_name]], "\n"))
}

# Summary recommendations
cat("\n=== RECOMMENDATIONS ===\n")

if(nrow(temp_unmonitored_usage) > 0) {
  cat("Consider adding monitoring to these high-activity unmonitored receivers:\n")
  high_activity_stations <- temp_unmonitored_usage %>%
    filter(total_movements >= quantile(temp_unmonitored_usage$total_movements, 0.75)) %>%
    pull(To_station)
  cat(paste(head(high_activity_stations, 5), collapse = ", "), "\n")
}

temp_monitoring_gaps <- temp_connection_summary %>%
  filter(connection_type %in% c("monitored_to_unmonitored", "unmonitored_to_monitored")) %>%
  pull(total_movements) %>%
  sum()

cat("Monitoring gap impact:", temp_monitoring_gaps, "movements involve unmonitored receivers\n")
cat("This represents", round(temp_monitoring_gaps/nrow(df_movement)*100, 1), "% of all movements\n")

### Cleanup
#----------------------------#
rm(list = ls(pattern = "^temp_"))
cat("\nCleanup complete.\n")
