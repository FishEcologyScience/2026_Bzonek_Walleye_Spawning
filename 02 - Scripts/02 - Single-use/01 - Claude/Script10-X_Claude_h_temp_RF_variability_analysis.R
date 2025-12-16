## --------------------------------------------------------------#
## Script name: Script10-4_Claude_h_loop_RF_variability_analysis
##
## Purpose of script: 
##    Investigate Random Forest model variability by running
##    Script1-1 and Script2-1 multiple times with different seeds
##    and collecting R² values from model_RF_plot$RF_RF2_R2
##
## Dependencies: 
##    - Script0-1_load_packages.R (packages and initial param_seed)
##    - Script1-1_format_data_h.R (habitat data formatting)
##    - Script2-1_analysis_h_habfish_RF.R (RF model creation)
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2025-09-12
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#

### Load packages once
#----------------------------#
source("02 - Scripts/Script0-1_load_packages.R")

### Set up analysis parameters
#----------------------------#
param_n_iterations <- 11
param_seeds <- c(1987, 1992, 1758, 123, 1903, 2024, 3141, 9876, 555, 1234, 8888)


### Initialize storage for results
#----------------------------#
loop_rf_results <- list()

cat("=== RF VARIABILITY ANALYSIS ===\n")
cat("Running", param_n_iterations, "iterations with different seeds...\n\n")

### Run analysis loop
#----------------------------#
for(i in 1:param_n_iterations) {
  cat("Iteration", i, "of", param_n_iterations, "- Seed:", param_seeds[i], "\n")

  # Update seed parameter (overwrite the one from Script0-1)
  param_seed <- param_seeds[i]

  # Clear previous analysis objects but keep our loop variables
  loop_objects_to_keep <- c("param_n_iterations", "param_seeds", "loop_rf_results",
                           "param_seed", "i")
  loop_all_objects <- ls()  # Get list of all objects in environment
  loop_objects_to_remove <- setdiff(loop_all_objects, loop_objects_to_keep)  # Find objects not in keep list
  loop_objects_to_remove <- loop_objects_to_remove[!grepl("^param_", loop_objects_to_remove)]  # Keep all param_ objects
  rm(list = loop_objects_to_remove)  # Remove all other objects

  # Run analysis scripts
  source("02 - Scripts/Script1-1_format_data_h.R")
  source("02 - Scripts/Script2-1_analysis_h_habfish_RF.R")

  # Extract and store R² values from both models
  if(exists("model_RF_VarImp") && exists("model_RF2_VarImp")) {  # Check if variable importance objects exist
    # Store the variable importance data frames for this iteration
    loop_rf_results[[i]] <- list(
      seed = param_seed,
      abundance_varimp = model_RF_VarImp,
      presence_varimp = model_RF2_VarImp
    )

    cat("  → Stored variable importance data\n")

    # Export plot as PNG in date-stamped subfolder
    loop_date_folder <- format(Sys.Date(), "%Y-%m-%d")  # Create YYYY-mm-dd folder name
    loop_output_dir <- paste0("04 - Outputs/r2/", loop_date_folder)  # Build full directory path
    if(!dir.exists(loop_output_dir)) dir.create(loop_output_dir, recursive = TRUE)  # Create folder if it doesn't exist
    loop_filename <- paste0(loop_output_dir, "/RF_plot_seed_", param_seed, ".png")  # Build full file path
    ggsave(loop_filename, model_RF_plot[["RF_RF2_R2"]], width = 10, height = 8, dpi = 300)
    cat("  → Plot saved:", loop_filename, "\n")

  } else {
    cat("  → ERROR: model variable importance data not found\n")
    loop_rf_results[[i]] <- list(
      seed = param_seed,
      abundance_varimp = NA,
      presence_varimp = NA
    )
  }

  cat("\n")
}


### Summarize results across seeds
#----------------------------#
cat("\n=== VARIABILITY SUMMARY ===\n")

# Extract R² values for each predictor across all seeds
temp_summary_abundance <- bind_rows(lapply(loop_rf_results, function(x) {  # Loop through each iteration's results
  if(!is.data.frame(x$abundance_varimp)) return(NULL)  # Skip if data not available
  x$abundance_varimp %>% mutate(seed = x$seed, model = "Abundance")  # Add seed and model columns
}))

temp_summary_presence <- bind_rows(lapply(loop_rf_results, function(x) {  # Loop through each iteration's results
  if(!is.data.frame(x$presence_varimp)) return(NULL)  # Skip if data not available
  x$presence_varimp %>% mutate(seed = x$seed, model = "Occurrence")  # Add seed and model columns
}))

# Combine both models
temp_summary_all <- bind_rows(temp_summary_abundance, temp_summary_presence)

# Calculate variability statistics per predictor
temp_summary_stats <- temp_summary_all %>%
  group_by(predictor, model) %>%
  summarise(
    mean_pR2 = mean(pR2, na.rm = TRUE),
    sd_pR2 = sd(pR2, na.rm = TRUE),
    cv_pR2 = sd_pR2 / mean_pR2 * 100,  # Coefficient of variation
    min_pR2 = min(pR2, na.rm = TRUE),
    max_pR2 = max(pR2, na.rm = TRUE),
    range_pR2 = max_pR2 - min_pR2,
    .groups = "drop"
  ) %>%
  arrange(model, desc(mean_pR2))

cat("\nVariability by predictor and model:\n")
print(temp_summary_stats, n = Inf)

# Overall variability by model
temp_summary_overall <- temp_summary_all %>%
  group_by(seed, model) %>%
  summarise(total_pR2 = sum(pR2, na.rm = TRUE), .groups = "drop") %>%
  group_by(model) %>%
  summarise(
    mean_total = mean(total_pR2),
    sd_total = sd(total_pR2),
    cv_total = sd_total / mean_total * 100,
    .groups = "drop"
  )

cat("\nOverall model variability:\n")
print(temp_summary_overall)


### Create variability visualization
#----------------------------#
# Set target seed to highlight
param_target_seed <- 1987

# Calculate mean values per predictor and model for lollipop stems
temp_summary_means <- temp_summary_all %>%
  group_by(predictor, model) %>%
  summarise(mean_pR2 = mean(pR2, na.rm = TRUE), .groups = "drop")

# Lollipop plot showing predictor variability across seeds
temp_plot_variability <- temp_summary_all %>%
  #mutate(is_target = seed == param_target_seed) %>%  # Flag target seed
  ggplot(aes(x = fct_reorder(.f = predictor, .x = pR2, .fun = mean), y = pR2)) +
  # Add lollipop stems (from 0 to mean)
  geom_segment(data = temp_summary_means,
               aes(x = predictor, xend = predictor, y = 0, yend = mean_pR2),
               linewidth = 1, color = "black") +
  # Add mean points at end of stems
  geom_point(data = temp_summary_means,
             aes(x = predictor, y = mean_pR2, fill = model),
             color = "black", size = 3.5, shape = 21) +
  # # Add individual seed points (non-target seeds)
  geom_point(aes(color = model), alpha = 0.4, size = 2,
             position = position_jitter(width = 0.2, height = 0)) +
  # Add highlighted target seed points
  geom_point(data = filter(temp_summary_all, seed %in% param_target_seed),
             aes(fill = model), color = "black", size = 2.5, shape = 21, alpha=0.9) +
  coord_flip() +
  scale_color_manual(values = c("Abundance" = "#b7001480", "Occurrence" = "#01c04c80")) +
  scale_fill_manual(values = c("Abundance" = "#b70014", "Occurrence" = "#01c04c")) +
  theme_bw() +
  ylab(bquote("pseudo-R"^2)) +
  xlab("Predictor") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "Variable Importance Across Seeds",
       subtitle = paste0("Black lollipops = mean across ", param_n_iterations,
                        " seeds; colored points = individual seeds; outlined points = seed ", param_target_seed)) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "gray90"))

# Display plot
print(temp_plot_variability)

# Save plot to date folder
loop_date_folder <- format(Sys.Date(), "%Y-%m-%d")
loop_output_dir <- paste0("04 - Outputs/r2/", loop_date_folder)
if(!dir.exists(loop_output_dir)) dir.create(loop_output_dir, recursive = TRUE)
loop_variability_filename <- paste0(loop_output_dir, "/RF_variability_across_seeds.png")
ggsave(loop_variability_filename, temp_plot_variability, width = 10, height = 8, dpi = 300)
cat("\n→ Variability plot saved:", loop_variability_filename, "\n")


### Reset to original seed
#----------------------------#
param_seed <- 1987

# ### Cleanup
# #----------------------------#
# rm(list = ls(pattern = "^temp_summary"))
# rm(list = ls(pattern = "^loop_"))
# 
