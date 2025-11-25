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


### prep scripts
#----------------------------#
source("02 - Scripts/Script0-1_load_packages.R")
source("02 - Scripts/Script1-1_format_data.R")

### efishing analysis
#----------------------------#
source("02 - Scripts/Script2-Y_analysis_habfish_RF.R")
#source("02 - Scripts/Script2-Y-2_analysis_habfish_RF_interactions.R")
#source("02 - Scripts/Script2-Y_analysis_habfish_glm.R")

### telemetry analysis
#----------------------------#
# Identify spawning and non-spawning behaviour
source("02 - Scripts/Script1-2_format_data_t_v2.R")
source("02 - Scripts/Script2-X-1_process_t_m_dataset.R")

# Use limited dataset to model spawning by habitat type
##source("02 - Scripts/Script2-X-2_analysis_t_glmer.R")
##source("02 - Scripts/Script2-X-2_analysis_t_RFspatial.R")
##source("02 - Scripts/Script2-_analysis_t_RFspatial.R")

# Look at spawning behaviour without habitat
source("02 - Scripts/Script2-Y_analysis_t_homerange.R")
#source("02 - Scripts/Script2-Y_analysis_t_network_plot.R")
##source("02 - Scripts/Script2-Y_analysis_t_repeatability.R")

# Integrate datasources
#source("02 - Scripts/Script3-1_combine_results.R")
source("02 - Scripts/Script2-Y_repeatability_telemetry.R")






















