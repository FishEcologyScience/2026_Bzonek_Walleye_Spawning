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

###Basic packages on CRAN
#----------------------------#
library('adehabitatHR') #investigate homeranges NOTE: select function in dependency MASS conflicts with dplyr select()
library('randomForest') #Original methodology
library('ggmap') #google basemaps
library('sf') #handle shapefiles, projections, and distance calculations
library('lme4') #Mixed effects models; lmer, glmer
library('tidyverse') #Basic organizing and plotting
library('corrplot') #Plot variable correlations
library('vegan') #Calculate diversity (for substrate)
library('patchwork') #Add plots together
library('ggridges') #Make ridges boxplot
library('rptR') #Look at Intraclass Correlation Coefficients
library('beepr') #Add warning chimes
library('keyring') #Manage secrets such as google maps API


###Packages off CRAN
#----------------------------#
###Lab package - not needed in main workflow, just for network analysis
#Only need to install during updates
# devtools::install_github("pbzonek/telemetrytoolsFESL")
#library('telemetrytoolsFESL')


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

