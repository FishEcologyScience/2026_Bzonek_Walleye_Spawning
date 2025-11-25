## --------------------------------------------------------------#
## Script name: Script2-Y_analysis_habfish_RFspatial
##
## Purpose of script: 
##    Build a spatial random forest model with habitat and electrofishing data
##    Investigate habitat feature importance and spatial autocorrelation
##   
##
## Author: Paul Bzonek 
##
## Date Created: 2024-12-13
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#



#####Spatial RF example######################----
#-------------------------------------------------------------#
### Make the RF models
#----------------------------#
#random seed for reproducibility
random.seed <- 1987
#coordinates of the cases
temp_xy <-data_habfish_pseudo.train[, c("lat", "lon")]
#distance matrix
temp_dist.matrix <- dist(temp_xy)
#distance thresholds (same units as distance_matrix)
temp_dist.thresholds <- c(0.005, 0.01, 0.025, 0.05, 0.075)

### Non-spatial model
RF_non.spatial <- spatialRF::rf(
  data =data_habfish_pseudo.train,
  dependent.variable.name = "Total.Count",
  #predictor.variable.names = c("Slope", "Light", "Year",
  #                             "Sand", "Gravel", "Cobble", "SubstrateDiversity"),
  predictor.variable.names = c("Slope", "Light", "Year",
                               "Sand", "Gravel", "Cobble", "SubstrateDiversity",
                               "Rubble", "WetlandDistance",  "Fetch"),

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






