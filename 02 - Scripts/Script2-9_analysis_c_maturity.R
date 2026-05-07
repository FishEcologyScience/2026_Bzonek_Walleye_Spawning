## --------------------------------------------------------------#
## Script name: Script2-9_analysis_c_maturity
##
## Purpose of script:
##    Estimate age at capture from von Bertalanffy growth equation,
##    project fork length and age for each study year since tagging,
##    join a user-supplied age-maturity key, and correlate maturity
##    and predicted length with spawning behaviour metrics.
##
## Dependencies:
##    - Script1-2_format_data_t.R       (data_fish)
##    - Script2-4_analysis_c_repeatability.R (df_behaviour)
##    - 01 - Data/HH_Fish_Workbook_Jun2024.xlsx (glatos_caught_date)
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2026-05-07
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#


#####Parameters ##################################################----
#-------------------------------------------------------------#

### Von Bertalanffy growth parameters (fork length, mm)
#----------------------------#
# FL = L_inf * (1 - exp(-K * (age - t0)))
# Equivalent to user-supplied: FL = 601.41 - e^(-0.31*(age+1.006))
# where t0 = -1.006 so (age - t0) = (age + 1.006)
# Values from Brooks et al. 2025
param_vb_linf <- 601.41   # asymptotic fork length (mm)
param_vb_k    <- 0.31     # growth coefficient
param_vb_t0   <- -1.006   # theoretical age at length zero (years)


### Age-maturity key (USER-SUPPLIED)
#----------------------------#
temp_key_maturity <- data.frame(
  age      = 1:20,
  maturity = c(rep("immature", 2), rep("mature",18))
)



#####Tagging Summary Table #######################################----
#-------------------------------------------------------------#

cat("\n=== TAGGING SUMMARY ===\n")

### Back-calculate age at capture using VB inverse
#----------------------------#
# Inverse of FL = L_inf * (1 - exp(-K * (age - t0))):
# age = -log(1 - FL / L_inf) / K + t0
# Note: FL must be < L_inf (601.41 mm); values at or above produce NaN.

df_tag_summary <- data_fish %>%
  mutate(
    age_at_tag = -log(1 - pmin(length_fork, param_vb_linf - 0.01) / param_vb_linf) / param_vb_k + param_vb_t0,
    tag_year = year(release_date)  
    ) %>%
  select(animal_id, release_date, tag_year, length_fork, age_at_tag)

cat("--- Fish Tagged Per Year ---\n")
print(table(df_tag_summary$tag_year))

cat("\n--- Tagging Summary Table ---\n")
print(df_tag_summary)

cat("\nAge at tag:       mean =", round(mean(df_tag_summary$age_at_tag,  na.rm = TRUE), 1),
    "  SD =", round(sd(df_tag_summary$age_at_tag,  na.rm = TRUE), 1), "\n")
cat("FL at tag (mm):   mean =", round(mean(df_tag_summary$length_fork, na.rm = TRUE), 0),
    "  SD =", round(sd(df_tag_summary$length_fork, na.rm = TRUE), 0), "\n")


#####Per Fish-Year Growth Projection #############################----
#-------------------------------------------------------------#

cat("\n=== PROJECTING GROWTH AND MATURITY PER FISH-YEAR ===\n")

df_behaviour_maturity <- df_behaviour %>%
  left_join(
    select(df_tag_summary, animal_id, tag_year, age_at_tag),
    by = "animal_id"
  ) %>%
  mutate(
    year_numeric    = as.integer(as.character(year)),
    years_since_tag = year_numeric - tag_year,
    age_at_year     = round(age_at_tag + years_since_tag),
    predicted_FL    = param_vb_linf * (1 - exp(-param_vb_k * (age_at_year - param_vb_t0))),
    predicted_FL    = round(predicted_FL, 1)
        ) %>%
  left_join(temp_key_maturity, by = c("age_at_year" = "age"))

cat("Rows in df_behaviour_maturity:", nrow(df_behaviour_maturity), "\n")
cat("Fish-years with projected age:", sum(!is.na(df_behaviour_maturity$age_at_year)), "\n")
cat("Predicted FL range:",
    round(min(df_behaviour_maturity$predicted_FL, na.rm = TRUE), 0), "to",
    round(max(df_behaviour_maturity$predicted_FL, na.rm = TRUE), 0), "mm\n")

#####Behaviour Correlations ######################################----
#-------------------------------------------------------------#

cat("\n=== BEHAVIOUR CORRELATIONS ===\n")

temp_behav_metrics <- c("station_count", "station_count_ratio",
                        "residence_mean", "depth_mean")


### Predicted fork length vs. behaviour metrics
#----------------------------#
temp_cor_predicted_FL <- purrr::map_dfr(temp_behav_metrics, function(metric) {
  temp_test <- cor.test(df_behaviour_maturity$predicted_FL,
                        df_behaviour_maturity[[metric]],
                        method = "spearman", exact = FALSE)
  data.frame(
    metric  = metric,
    rho     = round(temp_test$estimate, 2),
    p_value = round(temp_test$p.value, 4),
    n       = sum(!is.na(df_behaviour_maturity$predicted_FL) &
                  !is.na(df_behaviour_maturity[[metric]]))
  )
})

cat("--- Spearman: Predicted FL vs. Behaviour Metrics ---\n")
print(temp_cor_predicted_FL)


#####Annual Summary ##############################################----
#-------------------------------------------------------------#

cat("\n=== ANNUAL SUMMARY ===\n")

temp_annual_summary <- df_behaviour_maturity %>%
  group_by(year) %>%
  summarise(
    n_fish            = n_distinct(animal_id),
    age_mean          = round(mean(age_at_year,   na.rm = TRUE), 1),
    age_sd            = round(sd(age_at_year,     na.rm = TRUE), 1),
    predicted_FL_mean = round(mean(predicted_FL,  na.rm = TRUE), 0),
    predicted_FL_sd   = round(sd(predicted_FL,    na.rm = TRUE), 0),
    n_mature          = sum(maturity == "mature",  na.rm = TRUE),
    prop_mature       = round(n_mature / n_fish, 2),
    .groups = "drop"
  )

cat("--- Predicted Age and FL by Study Year ---\n")
print(temp_annual_summary)


#####Cleanup #####################################################----
#-------------------------------------------------------------#

rm(list = ls(pattern = "^temp_"))
rm(df_tag_summary)
cat("Cleanup complete.\n")
