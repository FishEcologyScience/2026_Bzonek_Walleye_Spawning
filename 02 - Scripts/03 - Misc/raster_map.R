library(gstat) #Spatial interpolation

### Format the data
#----------------------------#
data_hab2 <- data_hab %>% 
 #Look at underwater habitat
 filter(Depth__m_ < 0) %>%
 #Make light polution numeric
 mutate(LightPoll_numeric = case_when(LightPoll_Cat == "None" ~ 1,
                                      LightPoll_Cat == "Low" ~ 2,
                                      LightPoll_Cat == "Moderate" ~ 3,
                                      LightPoll_Cat == "High" ~ 4)) %>% 
 #Summarize habitat to PointID level do remove overlapping
 group_by(PointID) %>% 
 summarize(lat = mean(Start_Latitude),
           lon = mean(Start_Longitude),
           depth_min = min(Depth__m_),
           depth_median = median(Depth__m_),
           light_max = max(LightPoll_numeric),
           #get transect totals for all habitat traits
           across(c(Substrate_Siltation,  Fine:Terrestrial, CheckSum), sum)
           ) %>%
 #Get habitat proportions by dividing habitat sums by transect sums 
 mutate(across(c(Fine:Terrestrial), ~ . / CheckSum))

 # mutate(lat = Start_Latitude,
 #           lon = Start_Longitude,
 #           depth = Depth__m_,
 #           depth_median = Depth__m_)

# data_hab2 <- data_hab2 %>% 
#  select(PointID, lat, lon, depth, Dist_From_Start__m_, Dist_Pred, 
#         Dominant_Substrate_Type, Substrate_Siltation, Fine:Terrestrial, LightPoll_Cat)


### Interpolate habitat
#----------------------------#
temp_spatial_points_hab <- SpatialPointsDataFrame(coords = data_hab2 [, c("lon", "lat")],
                                                   data = data_hab2 )    
proj4string(temp_spatial_points_hab) <- CRS("+proj=longlat +datum=WGS84") #Identify starting projection 
#Check for duplicate points
sp::zerodist(temp_spatial_points_hab)


            ###Make new 2d grid
            temp_bbox <- bbox(temp_spatial_points_hab) # Step 1: Create a bounding box for the SpatialPointsDataFrame
            # Generate the grid using expand.grid
            temp_grid <- expand.grid(lon = seq(temp_bbox[1, 1], temp_bbox[1, 2], length.out = 100), #Build a sequence of grid points along each axis
                                     lat = seq(temp_bbox[2, 1], temp_bbox[2, 2], length.out = 100)) #Manually set range to match plots
            # Convert the grid to a SpatialPoints object
            coordinates(temp_grid) <- ~lat + lon
            proj4string(temp_grid) <- CRS("+proj=longlat +datum=WGS84") #Identify starting projection

### Interpolate depth  
# Calculate the 3D variogram
temp_KT_variogram_model <- gstat::variogram(depth_median ~ lat + lon, data = temp_spatial_points_hab)
# Fit the 3D variogram model
temp_KT_fitted_variogram <- gstat::fit.variogram(temp_KT_variogram_model, model = vgm(model = "Exp"))
plot(temp_KT_variogram_model, temp_KT_fitted_variogram)

# Perform 3D kriging interpolation
temp_kriged_data <- gstat::krige(depth_median ~ lat + lon, 
                     temp_spatial_points_hab,
                     newdata = temp_grid,
                     model = temp_KT_fitted_variogram)
temp_data_2d_long <- temp_kriged_data %>%  
 as.data.table()
       
            
            
            
            