## --------------------------------------------------------------#
## Script name: ScriptX_utility_index_telemetry_data
##
## Purpose of script:
##    Create an optimized, indexed version of data_det_raw for faster loading
##    This script only needs to be run once to create the optimized file
##
## Dependencies:
##    - data.table package
##    - Raw data file: HH_detections_clipped_filtered_2015-2023_good.rds
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2025-01-13
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#

### Load required packages
#----------------------------#
library(data.table)

### Load raw data
#----------------------------#
cat("Loading raw detections data for indexing optimization...\n")
cat("File size:",
    round(file.size("01 - Data/HH_detections_clipped_filtered_2015-2023_good.rds")/1024/1024, 1),
    "MB\n")

data_det_raw <- readRDS("01 - Data/HH_detections_clipped_filtered_2015-2023_good.rds")
cat("✓ Loaded", format(nrow(data_det_raw), big.mark = ","), "detection records\n")

### Convert to data.table and optimize
#----------------------------#
cat("Converting to data.table and optimizing...\n")

# Convert to data.table for indexing
setDT(data_det_raw)

# Set keys/indices on frequently used columns for faster filtering
# These are the columns most commonly used in filtering operations
setkey(data_det_raw, common_name_e, detection_timestamp_EST, station_no, animal_id)

# Convert character columns to factors where appropriate
# This saves memory and improves performance for categorical data
data_det_raw[, common_name_e := as.factor(common_name_e)]
data_det_raw[, station_no := as.factor(station_no)]
data_det_raw[, animal_id := as.factor(animal_id)]

# Additional indexing for month filtering (commonly used)
data_det_raw[, month := format(detection_timestamp_EST, "%m")]
setindex(data_det_raw, month)  # Secondary index for fast month filtering

cat("✓ Indexing and optimization complete\n")

### Save optimized version
#----------------------------#
param_output_file <- "01 - Data/HH_detections_optimized_indexed.rds"

cat("Saving optimized version with compression...\n")
saveRDS(data_det_raw, param_output_file, compress = "xz")

# Check file sizes
param_original_size <- file.size("01 - Data/HH_detections_clipped_filtered_2015-2023_good.rds")
param_optimized_size <- file.size(param_output_file)

cat("✓ Optimized version saved to:", basename(param_output_file), "\n")
cat("Original file size:", round(param_original_size/1024/1024, 1), "MB\n")
cat("Optimized file size:", round(param_optimized_size/1024/1024, 1), "MB\n")
cat("Size change:", round((param_optimized_size - param_original_size)/param_original_size * 100, 1), "%\n")

### Performance test (optional)
#----------------------------#
cat("\n=== PERFORMANCE COMPARISON ===\n")

# Test loading times
cat("Testing load times...\n")
param_time_original <- system.time({
  temp_original <- readRDS("01 - Data/HH_detections_clipped_filtered_2015-2023_good.rds")
})
rm(temp_original)

param_time_optimized <- system.time({
  temp_optimized <- readRDS(param_output_file)
})
rm(temp_optimized)

cat("Original file load time:", round(param_time_original[3], 2), "seconds\n")
cat("Optimized file load time:", round(param_time_optimized[3], 2), "seconds\n")
cat("Speed improvement:", round(param_time_original[3]/param_time_optimized[3], 1), "x faster\n")

### Usage instructions
#----------------------------#
cat("\n=== USAGE INSTRUCTIONS ===\n")
cat("To use the optimized file in Script1-2, change line 31 from:\n")
cat("  data_det_raw <- readRDS('01 - Data/HH_detections_clipped_filtered_2015-2023_good.rds')\n")
cat("to:\n")
cat("  data_det_raw <- readRDS('01 - Data/HH_detections_optimized_indexed.rds')\n")
cat("\nThe optimized file includes:\n")
cat("- Pre-set keys for faster filtering on common columns\n")
cat("- Factorized categorical variables for better performance\n")
cat("- Secondary index on month for faster temporal filtering\n")
cat("- Compressed storage for smaller file size\n")

### Clean up
#----------------------------#
rm(list = ls(pattern = "^param_"))
cat("\nOptimization complete.\n")




### Save Walleye only dataset
#----------------------------#
data_det_raw <- readRDS("01 - Data/HH_detections_optimized_indexed.rds")
data_det_raw_Walleye <- data_det_raw[common_name_e == "Walleye"]
saveRDS(data_det_raw_Walleye, "01 - Data/HH_detections_Walleye_optimized_indexed.rds", compress = "xz")


