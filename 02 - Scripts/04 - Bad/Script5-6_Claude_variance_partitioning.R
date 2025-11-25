## --------------------------------------------------------------#
## Script name: Script5-6_Claude_variance_partitioning
##
## Purpose of script: 
##    Partition variance in spawning behaviors between environmental 
##    and individual factors using df_behaviour dataset
##    Compare relative influence of water_level, fish length, 
##    detcount vs individual identity and year effects
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2025-01-03
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   - Uses df_behaviour for station count ratio analysis
##   - Comprehensive variance partitioning approach
##   - Clear separation of environmental vs individual effects
## --------------------------------------------------------------#

### Load required packages
#----------------------------#
library(tidyverse)
# Using explicit namespace calls for specialized packages
# lme4::lmer, performance::icc, partR2::partR2

### Set analysis parameters
#----------------------------#
param_min_years_per_fish <- 2      # Minimum years of data per fish
param_min_detections_fish <- 10    # Reduced minimum detections per fish
param_min_obs_per_group <- 3       # Minimum observations per random effect level
param_significance_level <- 0.05   # Statistical significance threshold

### Prepare dataset for variance partitioning
#----------------------------#

# Examine structure of df_behaviour
cat("Dataset structure:\n")
str(df_behaviour)
cat("\nSample size by year:\n")
table(df_behaviour$year, useNA = "ifany")

# Create analysis dataset with proper filtering
temp_variance_data <- df_behaviour %>%
  # Calculate additional derived variables
  mutate(
    # Log transformations for right-skewed variables
    mcp95_log = ifelse(mcp95 > 0, log(mcp95), NA),
    mcp95_spawn_log = ifelse(mcp95_spawn > 0, log(mcp95_spawn), NA),
    
    # Station count ratio (spawning sites / total sites)
    station_count_ratio = ifelse(station_count > 0, 
                                station_count_spawn / station_count, 
                                NA),
    
    # Standardize continuous predictors for effect size comparison
    water_level_scaled = as.numeric(scale(water_level)[,1]),
    length_total_scaled = as.numeric(scale(length_total)[,1]),
    detcount_sum_scaled = as.numeric(scale(detcount_sum)[,1]),
    
    # Ensure numeric response variables
    station_count = as.numeric(station_count),
    station_count_spawn = as.numeric(station_count_spawn),
    station_count_ratio = as.numeric(station_count_ratio),
    detcount_all_sum = as.numeric(detcount_all_sum),
    depth_mean = as.numeric(depth_mean),
    water_level = as.numeric(water_level)
  ) %>%
  # Fix station_count_ratio calculation (already exists in df_behaviour)
  mutate(
    # Handle missing detcount_sum properly
    detcount_sum = ifelse(is.na(detcount_sum), 0, detcount_sum),
    detcount_all_sum = ifelse(is.na(detcount_all_sum), 0, detcount_all_sum)
  ) %>%
  # Filter for fish with adequate data
  group_by(animal_id) %>%
  mutate(
    fish_years = n(),
    fish_total_detections = sum(detcount_sum, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(
    fish_years >= param_min_years_per_fish,
    fish_total_detections >= param_min_detections_fish,
    !is.na(water_level),
    !is.na(length_total)
  ) %>%
  # Convert grouping variables to factors
  mutate(
    animal_id = as.factor(animal_id),
    year = as.factor(year)
  )

# Report final sample sizes
cat("\n=== FINAL DATASET SUMMARY ===\n")
cat("Total observations:", nrow(temp_variance_data), "\n")
cat("Unique fish:", n_distinct(temp_variance_data$animal_id), "\n")
cat("Years represented:", n_distinct(temp_variance_data$year), "\n")
cat("Fish per year (mean ± SD):", 
    round(mean(table(temp_variance_data$year)), 1), "±", 
    round(sd(table(temp_variance_data$year)), 1), "\n")

### Define response variables for analysis
#----------------------------#
temp_response_variables <- list(
  # Spatial behavior metrics
  "station_count" = "Total stations used per year",
  #"station_count_spawn" = "Spawning stations used per year", 
  "station_count_ratio" = "Ratio spawning/total stations",
  
  # Movement/space use metrics  
  #"mcp95_log" = "Total home range size (log)",
  #"mcp95_spawn_log" = "Spawning range size (log)",
  
  # Activity metrics
  #"detcount_all_sum" = "Total detections per year",
  # Note: detcount_sum removed to avoid circularity as predictor
  
  # Temporal/environmental metrics
  "depth_mean" = "Mean depth preference",
  #"water_level" = "Mean water level experienced"
)

### Function to run variance partitioning analysis
#----------------------------#
run_variance_partitioning <- function(data, response_var, response_description) {
  
  cat("\n--- Analyzing:", response_description, "---\n")
  
  # Check if response variable exists and has sufficient data
  if(!response_var %in% names(data)) {
    cat("ERROR: Variable", response_var, "not found in dataset\n")
    return(NULL)
  }
  
  # Remove rows with missing response data
  temp_analysis_data <- data %>%
    filter(!is.na(.data[[response_var]]))
  
  if(nrow(temp_analysis_data) < 10) {
    cat("WARNING: Insufficient data for", response_var, "(n =", nrow(temp_analysis_data), ")\n")
    return(NULL)
  }
  
  # Build model formula
  formula_str <- paste0(response_var, " ~ water_level_scaled + length_total_scaled + detcount_sum_scaled + (1|animal_id) + (1|year)")
  
  cat("Model formula:", formula_str, "\n")
  cat("Sample size:", nrow(temp_analysis_data), "\n")
  
  # Diagnostic checks before fitting model
  cat("Data type checks:\n")
  cat("- Response variable class:", class(temp_analysis_data[[response_var]]), "\n")
  cat("- Response variable range:", range(temp_analysis_data[[response_var]], na.rm=TRUE), "\n")
  cat("- Predictor classes:\n")
  cat("  water_level_scaled:", class(temp_analysis_data$water_level_scaled), "\n")
  cat("  length_total_scaled:", class(temp_analysis_data$length_total_scaled), "\n") 
  cat("  detcount_sum_scaled:", class(temp_analysis_data$detcount_sum_scaled), "\n")
  cat("  animal_id:", class(temp_analysis_data$animal_id), "\n")
  cat("  year:", class(temp_analysis_data$year), "\n")
  
  # Check for problematic values
  if(any(is.infinite(temp_analysis_data[[response_var]]))) {
    cat("WARNING: Infinite values in response variable\n")
  }
  if(any(is.infinite(temp_analysis_data$water_level_scaled))) {
    cat("WARNING: Infinite values in water_level_scaled\n")
  }
  
  # Check sample sizes for random effects (key for avoiding singularity)
  temp_animal_counts <- table(temp_analysis_data$animal_id)
  temp_year_counts <- table(temp_analysis_data$year)
  
  cat("Random effect sample sizes:\n")
  cat("- Animals: n =", length(temp_animal_counts), 
      "min obs/animal =", min(temp_animal_counts),
      "mean obs/animal =", round(mean(temp_animal_counts), 1), "\n")
  cat("- Years: n =", length(temp_year_counts), 
      "min obs/year =", min(temp_year_counts),
      "mean obs/year =", round(mean(temp_year_counts), 1), "\n")
  
  # Determine appropriate model structure based on sample sizes
  use_year_effect <- length(temp_year_counts) >= 3 && min(temp_year_counts) >= param_min_obs_per_group
  use_animal_effect <- length(temp_animal_counts) >= 3 && min(temp_animal_counts) >= param_min_obs_per_group
  
  cat("Model structure decisions:\n")
  cat("- Include year random effect:", use_year_effect, "\n")
  cat("- Include animal random effect:", use_animal_effect, "\n")
  
  # Build appropriate model formula based on sample size decisions
  if(use_animal_effect && use_year_effect) {
    final_formula <- paste0(response_var, " ~ water_level_scaled + length_total_scaled + detcount_sum_scaled + (1|animal_id) + (1|year)")
    model_type <- "full_mixed"
  } else if(use_animal_effect && !use_year_effect) {
    final_formula <- paste0(response_var, " ~ water_level_scaled + length_total_scaled + detcount_sum_scaled + (1|animal_id)")
    model_type <- "animal_only_mixed"
  } else if(!use_animal_effect && use_year_effect) {
    final_formula <- paste0(response_var, " ~ water_level_scaled + length_total_scaled + detcount_sum_scaled + (1|year)")
    model_type <- "year_only_mixed"
  } else {
    final_formula <- paste0(response_var, " ~ water_level_scaled + length_total_scaled + detcount_sum_scaled")
    model_type <- "fixed_effects_only"
  }
  
  cat("Final model formula:", final_formula, "\n")
  cat("Model type:", model_type, "\n")
  
  # Fit mixed effects model
  tryCatch({
    
    # Fit appropriate model
    if(model_type == "fixed_effects_only") {
      temp_model <- lm(as.formula(final_formula), data = temp_analysis_data)
      cat("Fitted fixed effects only model\n")
    } else {
      temp_model <- lme4::lmer(as.formula(final_formula), data = temp_analysis_data)
      
      # Check for singularity
      if(lme4::isSingular(temp_model)) {
        cat("WARNING: Singular fit detected. Falling back to fixed effects model.\n")
        temp_model <- lm(as.formula(paste0(response_var, " ~ water_level_scaled + length_total_scaled + detcount_sum_scaled")), 
                        data = temp_analysis_data)
        model_type <- "fixed_effects_fallback"
      } else {
        cat("Mixed model fitted successfully\n")
      }
    }
    
    # Extract variance components based on model type
    if(model_type %in% c("fixed_effects_only", "fixed_effects_fallback")) {
      # For fixed effects models, use standard R² 
      temp_model_summary <- summary(temp_model)
      temp_r2 <- temp_model_summary$r.squared
      
      # Extract fixed effects
      temp_fixed_effects <- broom::tidy(temp_model)
      
      # Create simplified results structure
      temp_results <- list(
        response_variable = response_var,
        description = response_description,
        sample_size = nrow(temp_analysis_data),
        n_fish = n_distinct(temp_analysis_data$animal_id),
        n_years = n_distinct(temp_analysis_data$year),
        model_type = model_type,
        
        # Fixed effects only results
        r_squared = temp_r2,
        fixed_effects = temp_fixed_effects,
        model = temp_model,
        
        # No random effects
        icc_results = NULL,
        variance_partition = NULL
      )
      
      cat("Fixed effects R²:", round(temp_r2, 3), "\n")
      cat("No random effects (fixed effects model)\n")
      
    } else {
      # For mixed effects models, use partR2 and ICC
      temp_variance_partition <- partR2::partR2(
        temp_model, 
        partvars = c("water_level_scaled", "length_total_scaled", "detcount_sum_scaled"),
        data = temp_analysis_data,  # CRITICAL: explicit data argument
        R2_type = "marginal",
        nboot = 100  # Reduced for speed, increase for final analysis
      )
      
      # Extract random effects variance with performance
      temp_icc_results <- performance::icc(temp_model)
      
      # Extract fixed effects
      temp_fixed_effects <- broom.mixed::tidy(temp_model, effects = "fixed")
      
      # Compile results
      temp_results <- list(
        response_variable = response_var,
        description = response_description,
        sample_size = nrow(temp_analysis_data),
        n_fish = n_distinct(temp_analysis_data$animal_id),
        n_years = n_distinct(temp_analysis_data$year),
        model_type = model_type,
        
        # Variance partitioning results
        variance_partition = temp_variance_partition,
        
        # Random effects (individual and year effects)
        icc_results = temp_icc_results,
        
        # Fixed effects coefficients
        fixed_effects = temp_fixed_effects,
        
        # Model object for further analysis
        model = temp_model
      )
      
      # Print key results
      cat("Fixed effects R²:", round(temp_variance_partition$R2_full, 3), "\n")
      if(!is.null(temp_icc_results$ICC_conditional)) {
        cat("Individual ICC:", round(temp_icc_results$ICC_conditional, 3), "\n")
      }
    }
    
    return(temp_results)
    
  }, error = function(e) {
    cat("ERROR in model fitting:", e$message, "\n")
    return(NULL)
  })
}

### Run variance partitioning for all response variables
#----------------------------#
cat("\n=== VARIANCE PARTITIONING ANALYSIS ===\n")

# Run analysis for each response variable
temp_variance_results <- map2(
  names(temp_response_variables),
  temp_response_variables,
  ~ run_variance_partitioning(temp_variance_data, .x, .y)
)

# Name the results list
names(temp_variance_results) <- names(temp_response_variables)

# Remove any failed analyses
temp_variance_results <- temp_variance_results[!sapply(temp_variance_results, is.null)]

### Compile summary results table
#----------------------------#
cat("\n=== COMPILING RESULTS SUMMARY ===\n")

temp_summary_table <- map_dfr(temp_variance_results, function(result) {
  
  if(is.null(result)) return(NULL)
  
  # Extract variance partition percentages
  temp_partition <- result$variance_partition$R2
  
  # Fixed effects variance
  water_level_var <- ifelse("water_level_scaled" %in% rownames(temp_partition), 
                           temp_partition["water_level_scaled", "R2"], NA)
  length_var <- ifelse("length_total_scaled" %in% rownames(temp_partition), 
                      temp_partition["length_total_scaled", "R2"], NA)
  detcount_var <- ifelse("detcount_sum_scaled" %in% rownames(temp_partition), 
                        temp_partition["detcount_sum_scaled", "R2"], NA)
  
  # Random effects variance  
  individual_var <- result$icc_results$ICC_adjusted  # Individual consistency
  year_var <- result$icc_results$ICC_conditional - result$icc_results$ICC_adjusted  # Year effects
  
  # Fixed effects significance
  temp_fixed <- result$fixed_effects
  water_level_p <- temp_fixed$p.value[temp_fixed$term == "water_level_scaled"]
  length_p <- temp_fixed$p.value[temp_fixed$term == "length_total_scaled"] 
  detcount_p <- temp_fixed$p.value[temp_fixed$term == "detcount_sum_scaled"]
  
  # Compile row
  data.frame(
    Variable = result$response_variable,
    Description = result$description,
    N_obs = result$sample_size,
    N_fish = result$n_fish,
    N_years = result$n_years,
    
    # Variance components (as percentages)
    Water_Level_R2 = round(water_level_var * 100, 1),
    Length_R2 = round(length_var * 100, 1), 
    Detcount_R2 = round(detcount_var * 100, 1),
    Individual_ICC = round(individual_var * 100, 1),
    Year_ICC = round(year_var * 100, 1),
    
    # Significance indicators
    Water_Level_Sig = ifelse(length(water_level_p) > 0 && !is.na(water_level_p), 
                            water_level_p < param_significance_level, FALSE),
    Length_Sig = ifelse(length(length_p) > 0 && !is.na(length_p), 
                       length_p < param_significance_level, FALSE),
    Detcount_Sig = ifelse(length(detcount_p) > 0 && !is.na(detcount_p), 
                         detcount_p < param_significance_level, FALSE)
  )
})

# Sort by individual consistency (ICC)
temp_summary_table <- temp_summary_table %>%
  arrange(desc(Individual_ICC))

# Display results
cat("\nVARIANCE PARTITIONING SUMMARY TABLE:\n")
print(temp_summary_table)

### Create visualization of results
#----------------------------#
cat("\n=== CREATING VISUALIZATION ===\n")

# Prepare data for plotting
temp_plot_data <- temp_summary_table %>%
  select(Variable, Description, Water_Level_R2, Length_R2, Detcount_R2, Individual_ICC, Year_ICC) %>%
  pivot_longer(cols = c(Water_Level_R2, Length_R2, Detcount_R2, Individual_ICC, Year_ICC),
               names_to = "Component", values_to = "Variance_Percent") %>%
  mutate(
    Component_Type = case_when(
      Component %in% c("Water_Level_R2", "Length_R2", "Detcount_R2") ~ "Fixed Effects",
      TRUE ~ "Random Effects"
    ),
    Component_Clean = case_when(
      Component == "Water_Level_R2" ~ "Water Level",
      Component == "Length_R2" ~ "Fish Length", 
      Component == "Detcount_R2" ~ "Detection Count",
      Component == "Individual_ICC" ~ "Individual ID",
      Component == "Year_ICC" ~ "Year Effects",
      TRUE ~ Component
    )
  )

# Create stacked bar plot
temp_variance_plot <- ggplot(temp_plot_data, aes(x = reorder(Description, Variance_Percent), 
                                                y = Variance_Percent, 
                                                fill = Component_Clean)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_viridis_d(name = "Variance\nComponent") +
  labs(
    title = "Variance Partitioning: Spawning Behavior Components",
    subtitle = "Relative influence of environmental vs individual factors",
    x = "Spawning Behavior Metric",
    y = "Percent Variance Explained",
    caption = paste("Analysis based on", nrow(temp_variance_data), "fish-year observations")
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

print(temp_variance_plot)

### Export results
#----------------------------#
cat("\n=== EXPORTING RESULTS ===\n")

# Save summary table
write.csv(temp_summary_table, "04 - Outputs/variance_partitioning_summary.csv", row.names = FALSE)

# Save detailed results  
save(temp_variance_results, temp_summary_table, temp_variance_data, 
     file = "04 - Outputs/variance_partitioning_detailed.RData")

# Save plot
ggsave("04 - Outputs/variance_partitioning_plot.png", temp_variance_plot, 
       width = 12, height = 8, dpi = 300)

### Key findings summary
#----------------------------#
cat("\n=== KEY FINDINGS SUMMARY ===\n")

# Identify variables with strongest individual effects
temp_top_individual <- temp_summary_table %>%
  arrange(desc(Individual_ICC)) %>%
  slice_head(n = 3)

cat("Variables with strongest INDIVIDUAL consistency:\n")
for(i in 1:nrow(temp_top_individual)) {
  cat(paste0(i, ". ", temp_top_individual$Description[i], 
            " (", temp_top_individual$Individual_ICC[i], "% ICC)\n"))
}

# Identify variables with strongest environmental effects
temp_top_environmental <- temp_summary_table %>%
  mutate(Max_Environmental = pmax(Water_Level_R2, Length_R2, Detcount_R2, na.rm = TRUE)) %>%
  arrange(desc(Max_Environmental)) %>%
  slice_head(n = 3)

cat("\nVariables with strongest ENVIRONMENTAL effects:\n")
for(i in 1:nrow(temp_top_environmental)) {
  cat(paste0(i, ". ", temp_top_environmental$Description[i], 
            " (", temp_top_environmental$Max_Environmental[i], "% max effect)\n"))
}

# Flag potential detcount circularity issues
temp_detcount_issues <- temp_summary_table %>%
  filter(Detcount_R2 > 50 | (Detcount_Sig == TRUE & Detcount_R2 > 20))

if(nrow(temp_detcount_issues) > 0) {
  cat("\nPOTENTIAL DETCOUNT CIRCULARITY ISSUES:\n")
  for(i in 1:nrow(temp_detcount_issues)) {
    cat(paste0("- ", temp_detcount_issues$Description[i], 
              " (", temp_detcount_issues$Detcount_R2[i], "% variance)\n"))
  }
}

### Cleanup temporary objects
#----------------------------#
rm(list = ls(pattern = "^temp_"))
cat("\nVariance partitioning analysis complete.\n")