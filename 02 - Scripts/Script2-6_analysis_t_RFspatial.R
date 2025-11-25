## --------------------------------------------------------------#
## Script name: Script2-6_analysis_t_RFspatial
##
## Purpose of script: 
##    Build a spatial random forest model with telemetry data (telemetry workflow)
##    Investigate habitat feature importance and spatial autocorrelation
##    Uses the m_data datasets
##
## Dependencies: 
##    - Script2-2_process_t_dataset.R (m_data objects)
##
## Author: Paul Bzonek 
##
## Date Created: 2025-01-07
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   2025-01-11: Renamed from Script2-X-2_analysis_t_RFspatial.R
## --------------------------------------------------------------#
## --------------------------------------------------------------#



#####Spatial RF example######################----
#-------------------------------------------------------------#
### Make the RF models
#----------------------------#
#random seed for reproducibility
random.seed <- 1987
#coordinates of the cases
temp_xy <-m_data.train[, c("deploy_lat", "deploy_long")]
#distance matrix
temp_dist.matrix <- dist(temp_xy)
#distance thresholds (same units as distance_matrix)
temp_dist.thresholds <- c(0.005, 0.01, 0.025, 0.05, 0.075)

### Non-spatial model
RF_non.spatial <- spatialRF::rf(
  data =m_data.train,
  dependent.variable.name = "spawn",
  predictor.variable.names = c("depth_median", "light_max", "Substrate_Siltation",
                               "Fine", "Silt", "Sand", "Gravel", "Cobble", "Rubble", 
                               "Boulder", "Armourstone", "Concrete", "Terrestrial"),
  distance.matrix = as.matrix(temp_dist.matrix),
  distance.thresholds = temp_dist.thresholds,
  xy = temp_xy, #not needed by rf, but other functions read it from the model
  seed = random.seed,
  verbose = TRUE
)

### Spatial model
RF_spatial <- spatialRF::rf_spatial(
  model = RF_non.spatial,
  method = "mem.moran.sequential", #default method
  verbose = FALSE,
  seed = random.seed
  )

#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name

### Inspect the models
#----------------------------#
#Plot Morans I
spatialRF::plot_moran(
  RF_spatial, 
  verbose = FALSE)

#Compare model importance
RF_p1 <- spatialRF::plot_importance(
  RF_non.spatial, 
  verbose = FALSE) + 
  ggplot2::ggtitle("Non-spatial model") 

RF_p2 <- spatialRF::plot_importance(
  RF_spatial,
  verbose = FALSE) + 
  ggplot2::ggtitle("Spatial model")

RF_p1 | RF_p2 





#####Check spatial correlation in habitat RF ##################################################----
#-------------------------------------------------------------#
library(spatialRF)
### Make the RF models
#----------------------------#
#random seed for reproducibility
set.seed(param_seed)
#coordinates of the cases
temp_xy <-data_habfish_pseudo.train[, c("lat", "lon")]
#distance matrix
temp_dist.matrix <- dist(temp_xy)
#distance thresholds (same units as distance_matrix)
temp_dist.thresholds <- c(0.005, 0.01, 0.025, 0.05, 0.075)

### Non-spatial model
RF_non.spatial <- spatialRF::rf(
  data = mutate(data_habfish_pseudo.train, Presence = as.numeric(Presence)),
  dependent.variable.name = "Presence",
  predictor.variable.names = c("Slope", "SubstrateDiversity", "Fetch", "Light", "Year",
                               "Sand", "Gravel", "Cobble", "Rubble"),
  distance.matrix = as.matrix(temp_dist.matrix),
  distance.thresholds = temp_dist.thresholds,
  xy = temp_xy, #not needed by rf, but other functions read it from the model
  seed = param_seed,
  verbose = TRUE
)

### Spatial model
RF_spatial <- spatialRF::rf_spatial(
  model = RF_non.spatial,
  method = "mem.moran.sequential", #default method
  verbose = FALSE,
  seed = random.seed
  )

#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name

### Inspect the models
#----------------------------#
#Plot Morans I
spatialRF::plot_moran(
  RF_spatial, 
  verbose = FALSE)

#Compare model importance
RF_p1 <- spatialRF::plot_importance(
  RF_non.spatial, 
  verbose = FALSE) + 
  ggplot2::ggtitle("Non-spatial model") 

RF_p2 <- spatialRF::plot_importance(
  RF_spatial,
  verbose = FALSE) + 
  ggplot2::ggtitle("Spatial model")

RF_p1 | RF_p2 








