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

 

##### Run project scripts ########################################----
#-------------------------------------------------------------#
# Load packages and helper functions first (must use standard source)
source("02 - Scripts/Script0-1_load_packages.R")

# Now use func_source_clean for remaining scripts

### Core Data Processing
#----------------------------#
func_source_clean("02 - Scripts/Script1-1_format_data_h.R", level = param_verbose)       # Clean raw habitat and electrofishing data
func_source_clean("02 - Scripts/Script1-1.2_format_data_efish_raw.R", level = param_verbose) # Combine walleye spawning survey data 2022-2024
func_source_clean("02 - Scripts/Script1-2_format_data_t.R", level = param_verbose)       # Import and format raw telemetry data
func_source_clean("02 - Scripts/Script1-3_process_data_t.R", level = param_verbose)      # Filter, calculate movements, residency, and spawning events

### Primary Analysis Pipeline
#----------------------------#
# Habitat random forests workflow
func_source_clean("02 - Scripts/Script2-1_analysis_h_habfish_RF.R", level = param_verbose)
# Telemetry spawning workflow
func_source_clean("02 - Scripts/Script2-2_process_t_dataset.R", level = param_verbose)
# Combined workflows for repeatability
func_source_clean("02 - Scripts/Script2-4_analysis_c_repeatability.R", level = param_verbose)
# Spatial metrics for spawning behaviour
func_source_clean("02 - Scripts/Script2-5_analysis_t_station_consistency.R", level = param_verbose)


### Extended Analysis
#----------------------------#
func_source_clean("02 - Scripts/Script2-6_analysis_t_spawning_summary.R", level = param_verbose)             # Spawning proportion per fish per year
func_source_clean("02 - Scripts/Script2-7_analysis_t_spawning_proportion_by_hour.R", level = param_verbose) # Spawning proportion by hour (diel patterns)

### Visualization
#----------------------------#
func_source_clean("02 - Scripts/Script3-1_plot_methods.R", level = param_verbose)                    # Spatial methods figures for manuscript
func_source_clean("02 - Scripts/Script3-2_plot_publication.R", level = param_verbose, beep_on_complete=T) # Publication-ready results figures

