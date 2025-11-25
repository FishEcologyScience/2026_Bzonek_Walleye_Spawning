## --------------------------------------------------------------#
## Script name: Script5-7_Claude_simplified_variance_analysis
##
## Purpose of script: 
##    Simplified variance analysis focusing on key research question:
##    Water level (environmental) vs Individual identity effects
##    Drops problematic predictors (length, year) to avoid singularity
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2025-01-03
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   - Simplified model structure to avoid singularity issues
##   - Focus on core question: environment vs individual effects
##   - Uses only water_level and detcount as predictors
## --------------------------------------------------------------#

### Load required packages
#----------------------------#
library(tidyverse)

### Examine df_behaviour dataset and prepare for analysis
#----------------------------#
cat("=== SIMPLIFIED VARIANCE ANALYSIS ===\n")
cat("Dataset dimensions:", dim(df_behaviour), "\n")
cat("Sample size by year:\n")
print(table(df_behaviour$year, useNA = "ifany"))

# Create clean analysis dataset
temp_analysis_data <- df_behaviour %>%
  # Keep essential variables only
  select(animal_id, year, station_count, station_count_ratio, 
         depth_mean, water_level, detcount_sum) %>%
  # Clean missing data
  filter(
    !is.na(animal_id),
    !is.na(water_level),
    !is.na(detcount_sum)
  ) %>%
  # Replace remaining NAs with zeros for spawning metrics
  mutate(
    detcount_sum = ifelse(is.na(detcount_sum), 0, detcount_sum),
    station_count_ratio = ifelse(is.na(station_count_ratio), 0, station_count_ratio),
    depth_mean = ifelse(is.na(depth_mean), 0, depth_mean)
  ) %>%
  # Filter for fish with multiple observations
  group_by(animal_id) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  # Create scaled predictors
  mutate(
    water_level_scaled = as.numeric(scale(water_level)),
    detcount_sum_scaled = as.numeric(scale(detcount_sum)),
    # Convert to factors
    animal_id = as.factor(animal_id),
    year = as.factor(year)
  )

# Report sample sizes
cat("\nFinal dataset summary:\n")
cat("- Total observations:", nrow(temp_analysis_data), "\n")
cat("- Unique fish:", n_distinct(temp_analysis_data$animal_id), "\n")
cat("- Years:", paste(sort(unique(temp_analysis_data$year)), collapse = ", "), "\n")

# Check sample sizes per group
temp_fish_counts <- table(temp_analysis_data$animal_id)
cat("- Fish obs per individual: min =", min(temp_fish_counts), 
    "max =", max(temp_fish_counts), 
    "mean =", round(mean(temp_fish_counts), 1), "\n")

### Define response variables for simplified analysis
#----------------------------#
temp_response_vars <- list(
  "station_count" = "Total stations used per year",
  "station_count_ratio" = "Proportion of stations used for spawning", 
  "depth_mean" = "Mean spawning depth preference"
)

### Simplified variance partitioning function
#----------------------------#
run_simple_variance_analysis <- function(data, response_var, description) {
  
  cat("\n=== ANALYZING:", description, "===\n")
  
  # Filter for complete cases
  temp_model_data <- data %>%
    filter(!is.na(.data[[response_var]])) %>%
    filter(.data[[response_var]] != 0 | response_var != "depth_mean")  # Remove zero depth (non-spawning)
  
  cat("Sample size:", nrow(temp_model_data), "\n")
  cat("Fish:", n_distinct(temp_model_data$animal_id), "\n")
  
  if(nrow(temp_model_data) < 10) {
    cat("Insufficient data for analysis\n")
    return(NULL)
  }
  
  # Check if we have enough observations per fish for random effects
  temp_fish_counts <- table(temp_model_data$animal_id)
  param_adequate_fish <- sum(temp_fish_counts >= 2)
  
  cat("Fish with ≥2 observations:", param_adequate_fish, "\n")
  
  # Model selection based on data structure
  if(param_adequate_fish >= 5) {
    # Try mixed effects model
    cat("Attempting mixed effects model...\n")
    
    tryCatch({
      # Simple mixed model: water_level + detcount + (1|animal_id)
      temp_mixed_model <- lme4::lmer(
        as.formula(paste0(response_var, " ~ water_level_scaled + detcount_sum_scaled + (1|animal_id)")),
        data = temp_model_data
      )
      
      # Check for singularity
      if(lme4::isSingular(temp_mixed_model)) {
        cat("Singular fit - using fixed effects model\n")
        temp_model <- lm(
          as.formula(paste0(response_var, " ~ water_level_scaled + detcount_sum_scaled")),
          data = temp_model_data
        )
        temp_model_type <- "fixed_effects"
      } else {
        cat("Mixed model successful\n")
        temp_model <- temp_mixed_model
        temp_model_type <- "mixed_effects"
      }
      
    }, error = function(e) {
      cat("Mixed model failed:", e$message, "\n")
      cat("Using fixed effects model\n")
      temp_model <- lm(
        as.formula(paste0(response_var, " ~ water_level_scaled + detcount_sum_scaled")),
        data = temp_model_data
      )
      temp_model_type <- "fixed_effects"
    })
    
  } else {
    # Use fixed effects only
    cat("Insufficient fish replication - using fixed effects model\n")
    temp_model <- lm(
      as.formula(paste0(response_var, " ~ water_level_scaled + detcount_sum_scaled")),
      data = temp_model_data
    )
    temp_model_type <- "fixed_effects"
  }
  
  # Extract results based on model type
  if(temp_model_type == "mixed_effects") {
    
    # Mixed effects results
    temp_fixed_effects <- broom.mixed::tidy(temp_model, effects = "fixed")
    temp_icc <- performance::icc(temp_model)
    temp_r2 <- performance::r2(temp_model)
    
    # Calculate variance components manually
    temp_var_components <- lme4::VarCorr(temp_model)
    temp_individual_var <- as.numeric(temp_var_components$animal_id[1])
    temp_residual_var <- attr(temp_var_components, "sc")^2
    temp_total_var <- temp_individual_var + temp_residual_var
    
    # Results summary
    temp_results <- list(
      variable = response_var,
      description = description,
      model_type = temp_model_type,
      n_obs = nrow(temp_model_data),
      n_fish = n_distinct(temp_model_data$animal_id),
      
      # Variance components
      individual_var_pct = round(temp_individual_var / temp_total_var * 100, 1),
      residual_var_pct = round(temp_residual_var / temp_total_var * 100, 1),
      
      # Model fit
      marginal_r2 = round(temp_r2$R2_marginal, 3),
      conditional_r2 = round(temp_r2$R2_conditional, 3),
      icc = round(temp_icc$ICC_adjusted, 3),
      
      # Fixed effects
      fixed_effects = temp_fixed_effects,
      model = temp_model
    )
    
    cat("Individual variance:", temp_results$individual_var_pct, "%\n")
    cat("Marginal R² (fixed effects):", temp_results$marginal_r2, "\n")
    cat("Conditional R² (total):", temp_results$conditional_r2, "\n")
    cat("ICC (individual consistency):", temp_results$icc, "\n")
    
  } else {
    
    # Fixed effects results
    temp_fixed_effects <- broom::tidy(temp_model)
    temp_model_summary <- summary(temp_model)
    temp_anova <- anova(temp_model)
    
    # Calculate variance explained by each predictor
    temp_ss_total <- sum(temp_anova$`Sum Sq`)
    temp_water_var_pct <- round(temp_anova$`Sum Sq`[temp_anova$Df == 1][1] / temp_ss_total * 100, 1)
    temp_detcount_var_pct <- round(temp_anova$`Sum Sq`[temp_anova$Df == 1][2] / temp_ss_total * 100, 1)
    
    temp_results <- list(
      variable = response_var,
      description = description,
      model_type = temp_model_type,
      n_obs = nrow(temp_model_data),
      n_fish = n_distinct(temp_model_data$animal_id),
      
      # Variance components (no individual effects)
      individual_var_pct = 0,
      water_level_var_pct = temp_water_var_pct,
      detcount_var_pct = temp_detcount_var_pct,
      
      # Model fit
      r_squared = round(temp_model_summary$r.squared, 3),
      adj_r_squared = round(temp_model_summary$adj.r.squared, 3),
      
      # Fixed effects
      fixed_effects = temp_fixed_effects,
      anova_results = temp_anova,
      model = temp_model
    )
    
    cat("Water level variance:", temp_results$water_level_var_pct, "%\n")
    cat("Detection count variance:", temp_results$detcount_var_pct, "%\n")
    cat("Total R²:", temp_results$r_squared, "\n")
  }
  
  # Print significance of fixed effects
  temp_water_p <- temp_fixed_effects$p.value[temp_fixed_effects$term == "water_level_scaled"]
  temp_detcount_p <- temp_fixed_effects$p.value[temp_fixed_effects$term == "detcount_sum_scaled"]
  
  cat("Water level significance: p =", 
      ifelse(length(temp_water_p) > 0, round(temp_water_p, 4), "NA"), "\n")
  cat("Detection count significance: p =", 
      ifelse(length(temp_detcount_p) > 0, round(temp_detcount_p, 4), "NA"), "\n")
  
  return(temp_results)
}

### Run simplified analysis for all response variables
#----------------------------#
cat("\n=== RUNNING SIMPLIFIED VARIANCE ANALYSIS ===\n")

temp_simple_results <- map2(
  names(temp_response_vars),
  temp_response_vars,
  ~ run_simple_variance_analysis(temp_analysis_data, .x, .y)
)

names(temp_simple_results) <- names(temp_response_vars)
temp_simple_results <- temp_simple_results[!sapply(temp_simple_results, is.null)]

### Create summary table
#----------------------------#
cat("\n=== SUMMARY TABLE ===\n")

temp_summary <- map_dfr(temp_simple_results, function(result) {
  
  if(is.null(result)) return(NULL)
  
  # Extract significance indicators
  temp_fixed <- result$fixed_effects
  temp_water_sig <- any(grepl("water_level_scaled", temp_fixed$term) & temp_fixed$p.value < 0.05)
  temp_detcount_sig <- any(grepl("detcount_sum_scaled", temp_fixed$term) & temp_fixed$p.value < 0.05)
  
  data.frame(
    Variable = result$variable,
    Description = result$description,
    Model_Type = result$model_type,
    N_obs = result$n_obs,
    N_fish = result$n_fish,
    
    Individual_Variance_Pct = ifelse("individual_var_pct" %in% names(result), 
                                   result$individual_var_pct, 0),
    Water_Level_Effect_Sig = temp_water_sig,
    Detcount_Effect_Sig = temp_detcount_sig,
    
    Model_R2 = ifelse("conditional_r2" %in% names(result), 
                     result$conditional_r2, result$r_squared),
    ICC = ifelse("icc" %in% names(result), result$icc, 0)
  )
})

print(temp_summary)

### Answer the research question
#----------------------------#
cat("\n=== RESEARCH QUESTION: WATER LEVEL vs INDIVIDUAL EFFECTS ===\n")

for(i in 1:nrow(temp_summary)) {
  row <- temp_summary[i, ]
  
  cat("\n", row$Description, ":\n")
  
  if(row$Model_Type == "mixed_effects") {
    if(row$Individual_Variance_Pct > 20) {
      cat("  → STRONG INDIVIDUAL EFFECTS: ", row$Individual_Variance_Pct, "% variance\n")
    } else if(row$Individual_Variance_Pct > 10) {
      cat("  → MODERATE INDIVIDUAL EFFECTS: ", row$Individual_Variance_Pct, "% variance\n") 
    } else {
      cat("  → WEAK INDIVIDUAL EFFECTS: ", row$Individual_Variance_Pct, "% variance\n")
    }
    cat("  → ICC (repeatability): ", row$ICC, "\n")
  }
  
  cat("  → Water level significant: ", row$Water_Level_Effect_Sig, "\n")
  cat("  → Detection count significant: ", row$Detcount_Effect_Sig, "\n")
  cat("  → Total model fit (R²): ", row$Model_R2, "\n")
  
  # Overall interpretation
  if(row$Model_Type == "mixed_effects" && row$Individual_Variance_Pct > 15) {
    cat("  → CONCLUSION: Individual consistency detected\n")
  } else if(row$Water_Level_Effect_Sig) {
    cat("  → CONCLUSION: Primarily environmental (water level) driven\n")
  } else {
    cat("  → CONCLUSION: Low predictability - other factors important\n")
  }
}

### Create simple visualization
#----------------------------#
cat("\n=== CREATING VISUALIZATION ===\n")

if(nrow(temp_summary) > 0) {
  
  # Prepare data for plotting
  temp_plot_data <- temp_summary %>%
    select(Description, Individual_Variance_Pct, Water_Level_Effect_Sig, Detcount_Effect_Sig) %>%
    mutate(
      Environmental_Effect = ifelse(Water_Level_Effect_Sig, "Significant", "Not Significant"),
      Individual_Effect_Strength = case_when(
        Individual_Variance_Pct > 20 ~ "Strong (>20%)",
        Individual_Variance_Pct > 10 ~ "Moderate (10-20%)",
        TRUE ~ "Weak (<10%)"
      )
    )
  
  # Create simple bar plot
  temp_plot <- ggplot(temp_plot_data, aes(x = reorder(Description, Individual_Variance_Pct), 
                                         y = Individual_Variance_Pct)) +
    geom_col(aes(fill = Environmental_Effect), alpha = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("Significant" = "blue", "Not Significant" = "gray")) +
    labs(
      title = "Individual Consistency in Spawning Behavior",
      subtitle = "Color indicates significant water level effects",
      x = "Behavioral Metric",
      y = "Individual Variance (%)",
      fill = "Water Level Effect",
      caption = paste("n =", nrow(temp_analysis_data), "observations,", 
                     n_distinct(temp_analysis_data$animal_id), "fish")
    ) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  print(temp_plot)
  
  # Save plot
  ggsave("04 - Outputs/simple_variance_analysis_plot.png", temp_plot, 
         width = 10, height = 6, dpi = 300)
}

### Export results
#----------------------------#
cat("\n=== EXPORTING RESULTS ===\n")

# Save summary table
write.csv(temp_summary, "04 - Outputs/simple_variance_summary.csv", row.names = FALSE)

# Save detailed results
save(temp_simple_results, temp_summary, temp_analysis_data,
     file = "04 - Outputs/simple_variance_analysis.RData")

cat("Results saved to 04 - Outputs/\n")
cat("- simple_variance_summary.csv\n")
cat("- simple_variance_analysis.RData\n") 
cat("- simple_variance_analysis_plot.png\n")

### Cleanup
#----------------------------#
rm(list = ls(pattern = "^temp_"))
cat("\nSimplified variance analysis complete!\n")