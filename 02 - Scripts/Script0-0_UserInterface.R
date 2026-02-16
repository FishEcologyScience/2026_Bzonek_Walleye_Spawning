## --------------------------------------------------------------#
## Script name: Script0-0_UserInterface
##
## Purpose of script: 
##    A central location to order and source project scripts
##    Script naming:
##      - higher numbers depend on lower numbers
##      - letters indicate that there is no dependency between 
##           scripts of the same number
##     
## Author: Paul Bzonek 
##
## Date Created: 2024-12-03
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#


### Set verbosity level
#----------------------------#
param_verbose <- "minimal"  # Options: "debug", "full", "minimal", "silent"


### Core Data Processing
#----------------------------#
# Load packages and helper functions first (must use standard source)
source("02 - Scripts/Script0-1_load_packages.R")

# Now use func_source_clean for remaining scripts
func_source_clean("02 - Scripts/Script1-1_format_data_h.R", level = param_verbose)
func_source_clean("02 - Scripts/Script1-1.2_format_data_efish_raw.R", level = param_verbose)
func_source_clean("02 - Scripts/Script1-2_format_data_t.R", level = param_verbose)
func_source_clean("02 - Scripts/Script1-3_process_data_t.R", level = param_verbose)

### Primary Analysis Pipeline
#----------------------------#
# Habitat workflow
func_source_clean("02 - Scripts/Script2-1_analysis_h_habfish_RF.R", level = param_verbose)

# Telemetry workflow
func_source_clean("02 - Scripts/Script2-2_process_t_dataset.R", level = param_verbose)
#func_source_clean("02 - Scripts/Script2-3_analysis_t_homerange.R", level = param_verbose)

# Combined workflows
func_source_clean("02 - Scripts/Script2-4_analysis_c_repeatability.R", level = param_verbose)

### Additional Analysis Scripts (Optional, not currently in report)
#----------------------------#
#source("02 - Scripts/Script2-5_analysis_c_efish_correlations.R") # Combined: correlation analysis
#source("02 - Scripts/Script2-6_analysis_t_RFspatial.R")          # Telemetry: spatial RF
#source("02 - Scripts/Script2-7_analysis_t_network.R")            # Telemetry: network analysis  
#source("02 - Scripts/Script2-8_analysis_t_repeatability.R")      # Telemetry: repeatability (old) of t data, using complex structure

### Claude Analysis Suite
#----------------------------#
func_source_clean("02 - Scripts/Script10-1_Claude_t_station_consistency.R", level = param_verbose)
func_source_clean("02 - Scripts/Script10-2a_Claude_c_variation_behavioral.R", level = param_verbose)
func_source_clean("02 - Scripts/Script10-2b_Claude_c_variation_spawning.R", level = param_verbose)
func_source_clean("02 - Scripts/Script10-3_Claude_t_spawning_summary.R", level = param_verbose)
func_source_clean("02 - Scripts/Script10-4_Claude_t_spawning_proportion_by_hour.R", level = param_verbose)
func_source_clean("02 - Scripts/Script10-5_Claude_c_ranked_repeatability.R", level = param_verbose)

### Visualization
#----------------------------#
func_source_clean("02 - Scripts/Script3-1_plot_methods.R", level = param_verbose)
func_source_clean("02 - Scripts/Script3-2_plot_publication.R", level = param_verbose, beep_on_complete=T)


### One-off scripts not tied to report
#----------------------------#
#source("02 - Scripts/Script2-8_analysis_t_repeatability.R")      # Telemetry: repeatability (old) of t data, using complex structure





















