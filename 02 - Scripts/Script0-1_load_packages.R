## --------------------------------------------------------------#
## Script name: Script0-1_load_packages
##
## Purpose of script: 
##    Load general-purpose scripts used throughout project
##    Rarely used/problematic packages may be loaded in specific scripts 
##    custom telemetrytoolsFESL needs to be installed with devtools
##
## Author: Paul Bzonek 
##
## Date Created: 2024-12-03
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#

###Install missing packages
#----------------------------#
param_required_packages <- c(
  # Loaded via library()
  "tidyverse",     #Basic organizing and plotting
  "sf",            #handle shapefiles, projections, and distance calculations
  "patchwork",     #Add plots together
  "randomForest",  #Random forest models
  # Called via :: notation
  "adehabitatHR",  #investigate homeranges
  "ggmap",         #google basemaps
  "corrplot",      #Plot variable correlations
  "vegan",         #Calculate diversity (for substrate)
  "ggridges",      #Make ridges boxplot
  "rptR",          #Look at Intraclass Correlation Coefficients
  "beepr",         #Add warning chimes
  "keyring"        #Manage secrets such as google maps API
)
temp_missing <- param_required_packages[!param_required_packages %in% installed.packages()[, "Package"]]
if (length(temp_missing) > 0) {
  cat("Installing missing packages:", paste(temp_missing, collapse = ", "), "\n")
  install.packages(temp_missing)
}
rm(temp_missing)

###Load core packages
#----------------------------#
library('randomForest') #Random forest models
library('sf')           #Handle shapefiles, projections, and distance calculations
library('tidyverse')    #Basic organizing and plotting
library('patchwork')    #Combine plots with + and / operators


###Source functions
#----------------------------#
source("02 - Scripts/01 - Functions/Function1-1_JakesFunctions.R")
source("02 - Scripts/01 - Functions/Function1-3_HelperFunctions.R")


###Adjust Settings
#----------------------------#
theme_set(theme_classic()) #ggplot background
options(scipen=999) #Drop scientific notation
param_seed <- 1987 #Set seed for randomized analysis


###Build objects
#----------------------------#
###plots list
# Only create `plots` list if it does not already exist
if (!exists("plots", inherits = FALSE)) {
  plots <- list()  # Home for plots
} else{cat("list object 'plots' already exists")}


##google basemap
#Set map data
# Load cached basemap if available; otherwise fetch from Google API and cache
if (file.exists("01 - Data/HHmap_basemap.rds")) {
  HHmap <- readRDS("01 - Data/HHmap_basemap.rds")
} else {
  # Requires Google API key stored via: keyring::key_set("google_api", username = "Jake")
  param_google_api_key <- keyring::key_get("google_api", username = "Jake")
  ggmap::register_google(key = param_google_api_key)
  HHmap <- ggmap::get_googlemap(center = c(lon=-79.82892, lat=43.29598),
                         zoom = 13, scale = 4, size = c(720, 720),
                         maptype = c("satellite"))
  # saveRDS(HHmap, "01 - Data/HHmap_basemap.rds")
}

