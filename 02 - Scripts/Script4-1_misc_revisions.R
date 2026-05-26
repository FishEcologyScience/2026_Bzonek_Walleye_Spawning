## --------------------------------------------------------------#
## Script name: Script4-1_misc_revisions
##
## Purpose of script:
##    Stand-alone code additions addressing peer review comments.
##    Sections correspond to specific reviewer comment numbers.
##    All sections print to console; no files are auto-exported.
##
## Author: Paul Bzonek 
##
## Date Created: 2026-04-22
##
## Requires: full pipeline sourced via Script0-0_UserInterface.R
##    - data_habfish_raw        (Script1-1)
##    - data_efish_fish         (Script1-1-1)
##    - data_spawn              (Script1-3)
##    - df_summary_plot_spawning_hourly, df_SunCategory (Script2-7)
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#


# #####  Comment 36 Part A: Habitat Parameter Rationale  ##########----
# #----------------------------#
# # Generates a plain-text summary of habitat predictor selection.
# # Rebuild correlation matrix inline (does not require Script3-2 to
# # have been run in current session).
# #-------------------------------------------------------------#
# 
# cat("\n=== COMMENT 36: HABITAT PARAMETER SELECTION RATIONALE ===\n\n")
# 
# temp_cor_vars <- select(data_habfish_raw,
#                                 Silt, Sand, Gravel, Cobble, Rubble, Boulder,
#                                 SubstrateDiversity, Fetch, Slope, Light)
# 
# temp_cor_matrix <- cor(temp_cor_vars, use = "complete.obs")
# 
# temp_rationale <- c(
#   "Habitat Parameter Selection Rationale",
#   paste0("Generated: ", Sys.Date()),
#   "",
#   "CORRELATION EXCLUSIONS:",
#   "  Substrates excluded due to collinearity with retained predictors:",
#   "  Fine, Silt, Boulder, Armourstone, Concrete, Terrestrial, Substrate_Siltation",
#   "",
#   "  Pairwise Pearson correlation matrix (retained covariates + key excluded):",
#   capture.output(print(round(temp_cor_matrix, 2))),
#   "",
#   "MANUAL EXCLUSIONS:",
#   "  SAV_Cover       - insufficient within-survey variation across sites",
#   "  WetlandDistance - insufficient within-survey variation across sites",
#   "",
#   "FINAL PREDICTOR SET:",
#   "  Slope, SubstrateDiversity, Fetch, Light, Year,",
#   "  Sand, Gravel, Cobble, Rubble"
# )
# 
# cat(paste(temp_rationale, collapse = "\n"), "\n\n")
# # writeLines(temp_rationale, "04 - Outputs/habitat_parameter_selection_rationale.txt")
# 
# rm(list = ls(pattern = "^temp_"))
# cat("Cleanup complete.\n\n")


#####  Comment 40: Annual Fish Size and Sex Ratio  ###############----
#----------------------------#
# Reports mean fork length (+/-SD), total length (+/-SD), and
# male:female ratio by electrofishing survey year.
# Note: sex is heavily male-skewed (321 M, 4 F, ~50 NA).
#-------------------------------------------------------------#

cat("\n=== COMMENT 40: ANNUAL FISH SIZE AND SEX RATIO ===\n\n")

temp_summary_fish_annual <- data_efish_fish %>%
  group_by(Year) %>%
  summarise(
    n_fish       = n(),
    FL_mean      = round(mean(Fork.Length,  na.rm = TRUE), 2),
    FL_sd        = round(sd(Fork.Length,    na.rm = TRUE), 2),
    TL_mean      = round(mean(Total.Length, na.rm = TRUE), 2),
    TL_sd        = round(sd(Total.Length,   na.rm = TRUE), 2),
    n_male       = sum(Sex == "M", na.rm = TRUE),
    n_female     = sum(Sex == "F", na.rm = TRUE),
    n_unknown    = sum(is.na(Sex)),
    sex_ratio_MF = round(n_male / if_else(n_female > 0,
                           as.numeric(n_female), NA_real_), 1),
    .groups = "drop"
  )

cat("--- Annual Fish Size and Sex Summary ---\n")
print(temp_summary_fish_annual)

rm(list = ls(pattern = "^temp_"))
cat("Cleanup complete.\n\n")


#####  Comment 17: Tag Signal Detection Efficiency  ##############----
#----------------------------#
# Compares detected signals to theoretically possible signals in
# each spawning event window given the tag transmission interval.
#-------------------------------------------------------------#

cat("\n=== COMMENT 17: TAG SIGNAL DETECTION EFFICIENCY ===\n\n")

param_tag_interval_sec <- 200  # Walleye V13 ping rate: 130 - 270

temp_detect_eff <- data_spawn %>%
  group_by(animal_id, moveID) %>%
  summarise(
    n_detected   = n(),
    duration_min = as.numeric(difftime(max(detection_timestamp_EST),
                                        min(detection_timestamp_EST),
                                        units = "mins")),
    .groups = "drop"
  ) %>%
  mutate(
    n_expected  = round((duration_min * 60) / param_tag_interval_sec),
    detect_eff  = round(n_detected / if_else(n_expected > 0,
                          as.numeric(n_expected), NA_real_), 3)
  )

cat("--- Tag Signal Detection Efficiency ---\n")
cat("  Tag transmission interval assumed:", param_tag_interval_sec, "seconds\n")
cat("  n spawning events evaluated:", nrow(temp_detect_eff), "\n")
cat("  Mean detection efficiency:",
    round(mean(temp_detect_eff$detect_eff, na.rm = TRUE), 2),
    "SD:", round(sd(temp_detect_eff$detect_eff, na.rm = TRUE), 2), "\n")
cat("  Min:", round(min(temp_detect_eff$detect_eff, na.rm = TRUE), 2),
    "  Max:", round(max(temp_detect_eff$detect_eff, na.rm = TRUE), 2), "\n")

rm(list = ls(pattern = "^temp_"))
rm(param_tag_interval_sec)
cat("Cleanup complete.\n\n")


# #####  Comment 43: Daytime Telemetry Diel Comparison  ###########----
# #----------------------------#
# # Compares spawning detection rates across diel periods (day, dawn,
# # dusk, night) using data from Script2-7 which retains all-day April
# # detections before the dusk/night filter is applied.
# #-------------------------------------------------------------#


temp_summary_diel <- df_summary_plot_spawning_hourly %>%
  left_join(
    df_SunCategory %>% 
     mutate(hour = hour(Timestamp)) %>% 
     select(hour, SunCategory),
    by = "hour"
  ) %>%
  group_by(SunCategory) %>%
  summarise(
    total_detections = sum(total_detections),
    spawn_detections = sum(spawn_detections),
    prop_spawn       = round(spawn_detections / total_detections, 3),
    .groups = "drop"
  ) %>%
  arrange(desc(prop_spawn))

cat("--- Spawning Proportion by Diel Period ---\n")
print(temp_summary_diel)




#####  Comment 18: Tag Signal Detection Efficiency  ##############----
#----------------------------#
# Identify how many individuals skipped spawning for a year
#-------------------------------------------------------------#

# All fish-years with April presence (mirrors Script2-6 base)
temp_all_fish_years <- data_det %>%
  mutate(year = lubridate::year(detection_timestamp_utc)) %>%
  group_by(animal_id, year) %>%
  summarise(n_det = n(), .groups = "drop") %>%
  filter(n_det >= 10)  # match Script2-6 threshold

# Fish-years with confirmed spawning events
temp_spawn_years <- data_spawn %>%
  mutate(year = lubridate::year(detection_timestamp_utc)) %>%
  distinct(animal_id, year) %>%
  mutate(spawned = TRUE)

# Classify each fish
temp_fish_interannual <- temp_all_fish_years %>%
  left_join(temp_spawn_years, by = c("animal_id", "year")) %>%
  mutate(spawned = if_else(is.na(spawned), FALSE, spawned)) %>%
  group_by(animal_id) %>%
  summarise(
    n_years_present = n(),
    n_years_spawned = sum(spawned),
    .groups = "drop"
  ) %>%
  mutate(
    category = case_when(
      n_years_spawned == 0               ~ "never_spawned",
      n_years_spawned == n_years_present ~ "every_year",
      TRUE                               ~ "skipped_at_least_one"
    )
  )

cat("--- Interannual Spawning (n =", nrow(temp_fish_interannual), "fish) ---\n")
print(table(temp_fish_interannual$category))
cat("\nPercentages:\n")
print(round(prop.table(table(temp_fish_interannual$category)) * 100, 1))

# Spawning events per fish per year (among fish-years with spawning)
temp_events_per_fish_year <- data_spawn %>%
  mutate(year = lubridate::year(detection_timestamp_utc)) %>%
  group_by(animal_id, year) %>%
  summarise(n_events = n_distinct(moveID), .groups = "drop")

cat("\nSpawning events per fish per spawning year:\n")
cat("  n fish-years:", nrow(temp_events_per_fish_year), "\n")
cat("  Mean:", round(mean(temp_events_per_fish_year$n_events), 0),
    " SD:", round(sd(temp_events_per_fish_year$n_events), 0), "\n")
cat("  Range:", min(temp_events_per_fish_year$n_events), "-",
    max(temp_events_per_fish_year$n_events), "\n")




rm(list = ls(pattern = "^temp_"))
cat("Cleanup complete.\n\n")

#####  Comment 19: Receiver Spacing (nearest-neighbour distances)  ####----
#----------------------------#
# For each receiver station, compute the distance to its nearest neighbour.
# Reports min and max of those nearest-neighbour distances across the array.
# Uses unique station positions (one point per station_no, ignoring
# deployment date ranges — spatial footprint of the array only).
#-------------------------------------------------------------#

cat("\n=== COMMENT 19: RECEIVER ARRAY SPACING ===\n\n")

temp_stations_sf <- data_rec_locs_raw %>%
  select(station_no, deploy_lat, deploy_long) %>%
  distinct(station_no, .keep_all = TRUE) %>%
  sf::st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) %>%
  sf::st_transform(crs = 32618)  # UTM Zone 18N (Hamilton Harbour)

temp_dist_matrix <- sf::st_distance(temp_stations_sf)  # pairwise distances (metres)
diag(temp_dist_matrix) <- NA  # exclude self-distances

temp_nn_dist <- apply(temp_dist_matrix, 1, min, na.rm = TRUE)  # nearest neighbour per station

cat("  n stations:", nrow(temp_stations_sf), "\n")
cat("  Nearest-neighbour distance (m):\n")
cat("    Min:", round(min(temp_nn_dist), 0), "m\n")
cat("    Max:", round(max(temp_nn_dist), 0), "m\n")
cat("    Mean:", round(mean(temp_nn_dist), 0),
    " SD:", round(sd(temp_nn_dist), 0), "m\n")

rm(list = ls(pattern = "^temp_"))
cat("Cleanup complete.\n\n")


cat("=== Script_Revisions_ManuscriptR1.R complete ===\n")

