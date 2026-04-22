## --------------------------------------------------------------#
## Script name: Script2-8_revisions_R1.R
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
# temp_cor_vars <- dplyr::select(data_habfish_raw,
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
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    n_fish       = dplyr::n(),
    FL_mean      = round(mean(Fork.Length,  na.rm = TRUE), 2),
    FL_sd        = round(sd(Fork.Length,    na.rm = TRUE), 2),
    TL_mean      = round(mean(Total.Length, na.rm = TRUE), 2),
    TL_sd        = round(sd(Total.Length,   na.rm = TRUE), 2),
    n_male       = sum(Sex == "M", na.rm = TRUE),
    n_female     = sum(Sex == "F", na.rm = TRUE),
    n_unknown    = sum(is.na(Sex)),
    sex_ratio_MF = round(n_male / dplyr::if_else(n_female > 0,
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
# IMPORTANT: confirm param_tag_interval_sec from
#   HH_Fish_Workbook_Jun2024.xlsx (column: tag_model).
#   Typical VEMCO V16: 45-90s random delay; mean = 67.5s.
#-------------------------------------------------------------#

cat("\n=== COMMENT 17: TAG SIGNAL DETECTION EFFICIENCY ===\n\n")

param_tag_interval_sec <- 270  # Walleye V13 ping rate: 130 - 270

temp_detect_eff <- data_spawn %>%
  dplyr::group_by(animal_id, moveID) %>%
  dplyr::summarise(
    n_detected   = dplyr::n(),
    duration_min = as.numeric(difftime(max(detection_timestamp_EST),
                                        min(detection_timestamp_EST),
                                        units = "mins")),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    n_expected  = round((duration_min * 60) / param_tag_interval_sec),
    detect_eff  = round(n_detected / dplyr::if_else(n_expected > 0,
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


#####  Comment 43: Daytime Telemetry Diel Comparison  ###########----
#----------------------------#
# Compares spawning detection rates across diel periods (day, dawn,
# dusk, night) using data from Script2-7 which retains all-day April
# detections before the dusk/night filter is applied.
# Note: 'Light' in RF models = nearshore light POLLUTION index
#   (habitat covariate); it is independent of diel period.
#-------------------------------------------------------------#

cat("\n=== COMMENT 43: DAYTIME TELEMETRY DIEL COMPARISON ===\n\n")

temp_summary_diel <- df_summary_plot_spawning_hourly %>%
  dplyr::left_join(
    df_SunCategory %>% dplyr::select(hour, SunCategory),
    by = "hour"
  ) %>%
  dplyr::group_by(SunCategory) %>%
  dplyr::summarise(
    total_detections = sum(total_detections),
    spawn_detections = sum(spawn_detections),
    prop_spawn       = round(spawn_detections / total_detections, 3),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(prop_spawn))

cat("--- Spawning Proportion by Diel Period ---\n")
print(temp_summary_diel)
cat("\nNote: 'Light' in RF models is nearshore light pollution (1-4 scale),\n")
cat("not diel light availability. These are independent variables.\n")
cat("Low prop_spawn during 'day' supports the dusk/night filter justification.\n")

rm(list = ls(pattern = "^temp_"))
cat("Cleanup complete.\n\n")

cat("=== Script_Revisions_ManuscriptR1.R complete ===\n")
