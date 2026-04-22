## --------------------------------------------------------------#
## Script name: Script2-3_analysis_h_habfish_RF_sensitivity
##
## Purpose of script:
##    Leave-One-Variable-Out (LOVO) sensitivity analysis for habitat
##    random forest models. Re-fits each model with one predictor
##    removed at a time and reports change in R2 (count model) and
##    accuracy (occurrence model) relative to the full model.
##
## Author: Paul Bzonek 
##
## Date Created: 2026-04-22
##
## Requires: model_RF, model_RF2, data_habfish_pseudo.train,
##           data_habfish_pseudo.test from Script2-1
## --------------------------------------------------------------#
## Modification Notes:
##    Added to address Reviewer Comment 36 (sensitivity analysis)
## --------------------------------------------------------------#


#####  LOVO Sensitivity Analysis  ###############################----
#-------------------------------------------------------------#

param_predictors <- c("Slope", "SubstrateDiversity", "Fetch", "Light",
                      "Year", "Sand", "Gravel", "Cobble", "Rubble")

cat("=== LOVO RF SENSITIVITY ANALYSIS ===\n")
cat("Re-fitting models with each predictor removed in turn.\n")
cat("ntree = 1000 per model;", length(param_predictors), "models per response variable.\n\n")


### Count model LOVO
#----------------------------#
temp_lovo_count <- purrr::map_dfr(param_predictors, function(drop_var) {

  temp_formula <- as.formula(paste(
    "Total.Count ~",
    paste(setdiff(param_predictors, drop_var), collapse = " + ")
  ))

  temp_mod <- randomForest::randomForest(
    formula    = temp_formula,
    data       = na.omit(data_habfish_pseudo.train),
    ntree      = 1000,
    importance = FALSE
  )

  temp_pred   <- predict(temp_mod, data_habfish_pseudo.test)
  temp_ss_res <- sum((data_habfish_pseudo.test$Total.Count - temp_pred)^2)
  temp_ss_tot <- sum((data_habfish_pseudo.test$Total.Count -
                        mean(data_habfish_pseudo.test$Total.Count))^2)

  data.frame(
    dropped  = drop_var,
    R2_LOVO  = round(1 - temp_ss_res / temp_ss_tot, 3)
  )
})

cat("--- Count Model LOVO (test-set R2) ---\n")
print(temp_lovo_count)
cat("\n")


### Occurrence model LOVO
#----------------------------#
temp_lovo_occ <- purrr::map_dfr(param_predictors, function(drop_var) {

  temp_formula <- as.formula(paste(
    "Occurrence ~",
    paste(setdiff(param_predictors, drop_var), collapse = " + ")
  ))

  temp_mod <- randomForest::randomForest(
    formula    = temp_formula,
    data       = na.omit(data_habfish_pseudo.train),
    ntree      = 1000,
    importance = FALSE
  )

  temp_pred <- predict(temp_mod, data_habfish_pseudo.test)

  data.frame(
    dropped       = drop_var,
    Accuracy_LOVO = round(mean(temp_pred == data_habfish_pseudo.test$Occurrence), 3)
  )
})

cat("--- Occurrence Model LOVO (test-set accuracy) ---\n")
print(temp_lovo_occ)
cat("\n")


#####  Cleanup  ##################################################----
#-------------------------------------------------------------#
rm(list = ls(pattern = "^temp_"))
rm(param_predictors)
cat("Cleanup complete.\n")
