## --------------------------------------------------------------#
## Script name: Script4-2_analysis_c_maturity
##
## Purpose of script:
##    Estimate age at capture from von Bertalanffy growth equation,
##    project fork length and age for each study year since tagging,
##    and correlate predicted length with spawning behaviour metrics.
##    
##
## Dependencies:
##    - Script1-2_format_data_t.R       (data_fish)
##    - Script2-4_analysis_c_repeatability.R (df_behaviour)
##    - 01 - Data/HH_Fish_Workbook_Jun2024.xlsx (glatos_caught_date)
##
## Author: Paul Bzonek 
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
# Values from Brooks et al. 2025

# Lake Ontario model: "FL = 601.41 - e^(-0.31*(age+1.006))" 
# FL = L_inf * (1 - exp(-K * (age - t0)))
# where t0 = -1.006 so (age - t0) = (age + 1.006)
# param_vb_linf <- 601.41   # asymptotic fork length (mm)
# param_vb_k    <- 0.31     # growth coefficient
# param_vb_t0   <- -1.006   # theoretical age at length zero (years)

# Hamilton Harbour Model: "FL = 642.5 - e^(-0.375*(age+0.5))" 
# FL = L_inf * (1 - exp(-K * (age - t0)))
param_vb_linf <- 642.5   # asymptotic fork length (mm)
param_vb_k    <- 0.375   # growth coefficient
param_vb_t0   <- -0.5    # theoretical age at length zero (years)

### Age-maturity threshold
#----------------------------#
# Males mature at age 2; population assumed predominantly male.
param_maturity_age <- 2L



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
    # pmin caps FL just below L∞ to prevent log(0) NaN; fish at or above L∞ have their FL corrected downstream via pmax
    age_at_tag = -log(1 - pmin(length_fork, param_vb_linf - 0.01) / param_vb_linf) / param_vb_k + param_vb_t0,
    tag_year = year(release_date)  
    ) %>%
  select(animal_id, release_date, tag_year, length_fork, age_at_tag) %>%
  filter(animal_id %in% unique(data_det$animal_id))

cat("Fish in tagging summary:", nrow(df_tag_summary),
    "(filtered to fish detected in April telemetry window)\n")
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
    select(df_tag_summary, animal_id, tag_year, age_at_tag, length_fork),
    by = "animal_id"
  ) %>%
  mutate(
    year_numeric    = as.integer(as.character(year)),  
    years_since_tag = year_numeric - tag_year,
    age_pred        = age_at_tag + years_since_tag,
    predicted_FL    = param_vb_linf * (1 - exp(-param_vb_k * (age_pred - param_vb_t0))),
    predicted_FL    = round(pmax(predicted_FL, length_fork), 1)  # prevents fish above L∞ from appearing to shrink toward the asymptote
  ) %>%
  mutate(maturity = if_else(age_pred >= param_maturity_age, "mature", "immature")) %>%
  left_join(
    df_rec_specialization %>%
      select(animal_id, year, specialization_index_duration) %>%
      mutate(year = as.numeric(as.character(year))),  # df_rec_specialization$year is a factor; coerce via character to match df_behaviour's numeric year
    by = c("animal_id", "year")
  )

cat("Rows in df_behaviour_maturity:", nrow(df_behaviour_maturity), "\n")
cat("Fish-years with projected age:", sum(!is.na(df_behaviour_maturity$age_pred)), "\n")
cat("Predicted FL range:",
    round(min(df_behaviour_maturity$predicted_FL, na.rm = TRUE), 0), "to",
    round(max(df_behaviour_maturity$predicted_FL, na.rm = TRUE), 0), "mm\n")

#####Behaviour Correlations ######################################----
#-------------------------------------------------------------#

cat("\n=== BEHAVIOUR CORRELATIONS ===\n")

temp_behav_metrics <- c("station_count", "station_count_ratio",
                        "residence_mean", "depth_mean",
                        "specialization_index_duration")


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
    age_mean          = round(mean(age_pred,   na.rm = TRUE), 1),
    age_sd            = round(sd(age_pred,     na.rm = TRUE), 1),
    predicted_FL_mean = round(mean(predicted_FL,  na.rm = TRUE), 0),
    predicted_FL_sd   = round(sd(predicted_FL,    na.rm = TRUE), 0),
    n_mature          = sum(maturity == "mature",  na.rm = TRUE),
    prop_mature       = round(n_mature / n_fish, 2),
    .groups = "drop"
  )

cat("--- Predicted Age and FL by Study Year ---\n")
print(temp_annual_summary)


#####Growth Trajectory Plot ######################################----
#-------------------------------------------------------------#

cat("\n=== GROWTH TRAJECTORY PLOT ===\n")

### Spawning indicator per fish-year
#----------------------------#
temp_spawn_flag <- data_spawn %>%
  mutate(year = as.integer(lubridate::year(detection_timestamp_utc))) %>%
  distinct(animal_id, year) %>%
  mutate(spawned = TRUE)

### Build trajectory data (three separate layers)
#----------------------------#
# Lines: tag year (observed FL) + detected years (predicted FL), one y per fish-year
# Tag year rows placed first in bind_rows so distinct() retains observed FL if tag year overlaps a detected spawning year
temp_traj_lines <- bind_rows(
  df_tag_summary %>%
    transmute(animal_id, year = tag_year, FL = length_fork, tag_year),
  df_behaviour_maturity %>%
    select(animal_id, year = year_numeric, FL = predicted_FL, tag_year)
) %>%
  distinct(animal_id, year, .keep_all = TRUE) %>%
  mutate(tag_year_fct = as.factor(tag_year)) %>%
  arrange(animal_id, year)

# Circles: all detected years with spawning indicator (may overlap tag year)
temp_traj_circles <- df_behaviour_maturity %>%
  select(animal_id, year = year_numeric, FL = predicted_FL, tag_year) %>%
  left_join(temp_spawn_flag, by = c("animal_id", "year")) %>%
  mutate(
    point_type   = if_else(!is.na(spawned), "spawning", "present"),  # left_join gives NA (not FALSE) for non-spawning years
    tag_year_fct = as.factor(tag_year)
  )

# Diamonds: observed FL at tagging
temp_traj_diamonds <- df_tag_summary %>%
  transmute(animal_id, year = tag_year, FL = length_fork,
            tag_year_fct = as.factor(tag_year))


### Plot
#----------------------------#
plots$behaviour$growth_trajectory <-
  ggplot() +
  geom_line(
    data = temp_traj_lines,
    aes(x = year, y = FL, group = animal_id, colour = tag_year_fct, 
        # text aes is ignored by ggplot2; read by ggplotly() as tooltip
        text = paste0("ID: ", animal_id, "<br>Tag year: ", tag_year)),  
    linewidth = 0.9, alpha = 0.5
  ) +
  geom_point(
    data = temp_traj_circles,
    aes(x = year, y = FL, colour = tag_year_fct, shape = point_type,
        # text aes is ignored by ggplot2; read by ggplotly() as tooltip
        text = paste0("ID: ", animal_id,
                      "<br>Year: ", year,
                      "<br>FL (pred): ", round(FL, 0), " mm",
                      "<br>Tag year: ", tag_year,
                      "<br>", point_type)),
    size = 3
  ) +
  geom_point(
    data = temp_traj_diamonds,
    aes(x = year, y = FL, colour = tag_year_fct,
        # text aes is ignored by ggplot2; read by ggplotly() as tooltip
        text = paste0("ID: ", animal_id,
                      "<br>Tag year: ", year,
                      "<br>FL (measured): ", round(FL, 0), " mm")),
    shape = 18, size = 2
  ) +
  scale_shape_manual(values = c("spawning" = 16, "present" = 1)) +
  scale_colour_viridis_d() +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  labs(x = "Year", y = "Fork Length (mm)",
       colour = "Tag Year", shape = NULL) +
  theme_classic()

print(plots$behaviour$growth_trajectory)
plotly::ggplotly(plots$behaviour$growth_trajectory, tooltip = "text")


### Second plot: age trajectory coloured by tag year
#----------------------------#
temp_age_lines <- bind_rows(
  df_tag_summary %>%
    transmute(animal_id, year = tag_year, age = age_at_tag, tag_year),
  df_behaviour_maturity %>%
    select(animal_id, year = year_numeric, age = age_pred, tag_year)
) %>%
  distinct(animal_id, year, .keep_all = TRUE) %>%
  mutate(tag_year_fct = as.factor(tag_year)) %>%
  arrange(animal_id, year)

temp_age_circles <- df_behaviour_maturity %>%
  select(animal_id, year = year_numeric, age = age_pred, tag_year) %>%
  left_join(temp_spawn_flag, by = c("animal_id", "year")) %>%
  mutate(
    point_type   = if_else(!is.na(spawned), "spawning", "present"),
    tag_year_fct = as.factor(tag_year)
  ) %>%
  select(-spawned)

temp_age_diamonds <- df_tag_summary %>%
  transmute(animal_id, year = tag_year, age = age_at_tag,
            tag_year_fct = as.factor(tag_year))

plots$behaviour$growth_trajectory_age <-
  ggplot() +
  geom_line(
    data = temp_age_lines,
    aes(x = year, y = age, group = animal_id, colour = tag_year_fct),
    linewidth = 0.9, alpha = 0.5
  ) +
  geom_point(
    data = temp_age_circles,
    aes(x = year, y = age, colour = tag_year_fct, shape = point_type),
    size = 3
  ) +
  geom_point(
    data = temp_age_diamonds,
    aes(x = year, y = age, colour = tag_year_fct),
    shape = 18, size = 2
  ) +
  scale_shape_manual(values = c("spawning" = 16, "present" = 1)) +
  scale_colour_viridis_d() +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  scale_y_continuous(breaks = scales::breaks_width(1)) +
  labs(x = "Year", y = "Age (years)",
       colour = "Tag Year", shape = NULL) +
  theme_classic()

### Patchwork combination
#----------------------------#
plots$behaviour$growth_trajectory_combined <-
  plots$behaviour$growth_trajectory /
  plots$behaviour$growth_trajectory_age +
  patchwork::plot_layout(guides = "collect")  # merges the shared Tag Year legend from both panels into one

print(plots$behaviour$growth_trajectory_combined)


#####Cleanup #####################################################----
#-------------------------------------------------------------#

rm(list = ls(pattern = "^temp_"))
rm(df_tag_summary, param_vb_linf, param_vb_k, param_vb_t0, param_maturity_age)  # df_behaviour_maturity is retained for downstream use
cat("Cleanup complete.\n")
