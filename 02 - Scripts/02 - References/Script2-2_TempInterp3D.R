## --------------------------------------------------------------#
## Script name: Script2-2_TempInterp3D.R 
##
## Purpose of script: 
##    Produce a 3D spatial interpolation of temperature
##     
##
## Author: Paul Bzonek
##
## Date Created: 2023-08-15
##
## --------------------------------------------------------------#  
## 
##   
## --------------------------------------------------------------#

# Load required packages
library(gstat)
library(sp)
library(plotly)

#Specify snapshot time
temp_snapshottime <- as.POSIXct("2022-08-11 14:00:00")

#####Format dataset ##############################################----
#-------------------------------------------------------------#
SL_data_receiver_T <- readRDS("01 - Data/RDS objects/SL_data_receiver_T_2023-08-02.RDS")

#Grab float lengths
temp_floatlength <- read_xlsx(path="01 - Data/Stoney Lake receiver list_2022-05-19.xlsx")
temp_floatlength <- temp_floatlength %>% 
 select(Station=station, Float= `anchor type`, Notes) %>% 
 mutate(FloatLength = case_when(Float == "6 foot float" ~ 1.2,
                                Float == "long float" ~ 1.8,
                                Float == "normal float" ~ 1.8,
                                Station %in% c("T3", "T21") ~ 3,
                                TRUE ~ 1)
        )

#Add receiver locations
SL_data_receiver_T <- SL_data_receiver_T %>% 
 left_join(SL_data_rec_locs) %>% 
 filter(Timestamp >= deploy_date_time,
        Timestamp <= recover_date_time2) %>% distinct()

SL_data_receiver_T <- SL_data_receiver_T %>% 
 left_join(temp_floatlength, by=c("station_no"="Station")) %>% #Add float lengths
 mutate(receiver_depth = bottom_depth-FloatLength)
rm(temp_floatlength)

#Make example 3d data
#PB NOTE: MORE WORK REQUIRED
KT_data_3d_raw <- SL_data_receiver_T %>% 
 filter(Timestamp == temp_snapshottime) %>% #2023-05-25
 select(x=deploy_long, y=deploy_lat, z=receiver_depth, Temp)

#Add hobo logger data
KT_data_3d_raw <- SL_data_Hobo %>% 
filter(Timestamp == temp_snapshottime) %>% 
select(x=deploy_long, y=deploy_lat, z=instrument_depth, Temp=Temperature) %>% 
rbind(KT_data_3d_raw) 
 

#####Krige a map with simplified data ############################----
#-------------------------------------------------------------#
# Create a SpatialPointsDataFrame
KT_spatial_points_df <- SpatialPointsDataFrame(coords = KT_data_3d_raw[, c("x", "y", "z")],
                                            data = KT_data_3d_raw)
     proj4string(KT_spatial_points_df) <- CRS("+proj=longlat +datum=WGS84") #Identify starting projection
     KT_spatial_points_df = spTransform(KT_spatial_points_df, CRS("EPSG:3857")) #Switch to a projection with meters, not degrees. Also try 'EPSG:32724'
       
     #Grid to interpolate onto
     temp_bbox <- bbox(KT_spatial_points_df) # Step 1: Create a bounding box for the SpatialPointsDataFrame
     # Generate the grid using expand.grid
     temp_grid_3d <- expand.grid(x = seq(temp_bbox[1, 1], temp_bbox[1, 2], length.out = 100), #Build a sequence of grid points along each axis
                                 y = seq(5545000, 5558000, length.out = 50), #Manually set range to match plots
                                 z = seq(temp_bbox[3, 1], temp_bbox[3, 2], length.out = 15))
     # Convert the grid to a SpatialPoints object
     coordinates(temp_grid_3d) <- ~x + y + z
     proj4string(temp_grid_3d) <- CRS("EPSG:3857") #Identify starting projection
     
     
# Calculate the 3D variogram
KT_variogram_model <- variogram(Temp ~ 1, data = KT_spatial_points_df)

# Fit the 3D variogram model
KT_fitted_variogram <- fit.variogram(KT_variogram_model, model = vgm(psill = 1, model = "Exp"))

# Perform 3D kriging interpolation
KT_data_3d_long <- krige(Temp ~ 1, 
                     KT_spatial_points_df,
                     newdata = temp_grid_3d,
                     model = KT_fitted_variogram)

# The 'kriged_data' object contains the interpolated values in 3D space
KT_data_3d_long <- KT_data_3d_long %>%  
 as.data.table() %>% 
 mutate(z=-z)

 

#####Prep environment for plotting ###############################----
#-------------------------------------------------------------#
###Prep plot objects
#Shapefile
SL_shapefile_EPSG3857 = spTransform(SL_shapefile, CRS("EPSG:3857")) #Also try 'EPSG:32724'

#Receiver locations
temp_rec_locs <- SL_data_rec_locs %>% ungroup() %>% 
 select(deploy_long, deploy_lat, instrument_depth=bottom_depth) %>% 
 mutate(group="Receiver")
  #Logger locations
  temp_rec_locs <- SL_data_Hobo %>% 
   select(deploy_long, deploy_lat, instrument_depth) %>% 
   unique() %>% 
   full_join(temp_rec_locs)
   
  coordinates(temp_rec_locs) <- c("deploy_long", "deploy_lat")
   proj4string(temp_rec_locs) <- CRS("+proj=longlat +datum=WGS84")
   temp_rec_locs = spTransform(temp_rec_locs, CRS("EPSG:3857")) #Also try 'EPSG:32724'
   temp_rec_locs <- as.data.table(temp_rec_locs)


 
###Produce a quick depth interpolation over a 2d grid
#----------------------------#

 
       ###Make a depth spatial dataset
       temp_KT_data_3d_raw <- SL_data_receiver_T %>% 
        filter(Timestamp == temp_snapshottime) %>% 
        select(x=deploy_long, y=deploy_lat, z=bottom_depth) %>% #Use bottom depth, not receiver depth
        group_by(x,y) %>% 
        summarize(z=max(z)) %>% 
        ungroup()

       temp_spatial_points_2d <- SpatialPointsDataFrame(coords = temp_KT_data_3d_raw[, c("x", "y")],
                                                   data = temp_KT_data_3d_raw)    
            proj4string(temp_spatial_points_2d) <- CRS("+proj=longlat +datum=WGS84") #Identify starting projection
            temp_spatial_points_2d = spTransform(temp_spatial_points_2d, CRS("EPSG:3857")) #Switch to a projection with meters, not degrees. Also try 'EPSG:32724'
       
            ###Make new 2d grid
            temp_bbox2 <- bbox(temp_spatial_points_2d) # Step 1: Create a bounding box for the SpatialPointsDataFrame
            # Generate the grid using expand.grid
            temp_grid_2d <- expand.grid(x = seq(temp_bbox[1, 1], temp_bbox[1, 2], length.out = 100), #Build a sequence of grid points along each axis
                                   y = seq(5545000, 5558000, length.out = 50)) #Manually set range to match plots
            # Convert the grid to a SpatialPoints object
            coordinates(temp_grid_2d) <- ~x + y
            proj4string(temp_grid_2d) <- CRS("EPSG:3857") #Identify starting projection
            
       ### Interpolate depth  
       # Calculate the 3D variogram
       temp_KT_variogram_model2 <- variogram(z ~ 1, data = temp_spatial_points_2d)
       # Fit the 3D variogram model
       temp_KT_fitted_variogram2 <- fit.variogram(temp_KT_variogram_model2, model = vgm(psill = 1, model = "Exp"))
       # Perform 3D kriging interpolation
       temp_kriged_data <- krige(z ~ 1, 
                            temp_spatial_points_2d,
                            newdata = temp_grid_2d,
                            model = temp_KT_fitted_variogram2)
       temp_data_2d_long <- temp_kriged_data %>%  
        as.data.table()
       
       #Shapefile mask
       temp_mask_2d <- 
        sp::over( #Identify spatial overlaps
                 x = SpatialPoints(temp_data_2d_long, proj4string=CRS("EPSG:3857")), #format the input datatable
                 y = methods::as(SL_shapefile_EPSG3857,"SpatialPolygons") #format the 'mask' shapefile 
                )
       
       #Re plot with mask around shapefile
        ggplot()+
         geom_tile(data=temp_data_2d_long[!is.na(temp_mask_2d),], #Filter data outside of mask
                   aes(x=x, y=y, fill=var1.pred))+
         geom_polygon(data=SL_shapefile_EPSG3857, aes(x=long, y=lat, group=group), colour = "black", fill=NA)+
         geom_point(data=temp_rec_locs, aes(x=deploy_long, y=deploy_lat), colour="firebrick")+
         ylim(5545000, 5558000)+ coord_fixed()+ 
         scale_fill_viridis_c()
        
#Mask 3D cells by maximum depth
KT_data_3d_long_masked <- full_join(KT_data_3d_long, 
                       select(temp_data_2d_long[!is.na(temp_mask_2d),],
                              x, y, depth=var1.pred)) %>% 
                       filter(!is.na(z),
                              z>= -depth)
rm(KT_data_3d_long) 


#####Produce 3D Temp-Depth plot ##################################----
#-------------------------------------------------------------#
plot_ly() %>% #Start with an empty plot

  # Add the scatter3d trace for temperature
  add_trace(
    type = "scatter3d",
    data = KT_data_3d_long_masked,
    x = ~x,
    y = ~y,
    z = ~z,
    color = ~var1.pred,
    text = ~var1.pred,
    mode = "markers",
    marker = list(
     #size = ~var1.pred,  # Use the 'temperature' variable for marker size
     #sizemode = "diameter",  # Use 'diameter' mode for marker size
     size = 5 #opacity = 0.9
    )
  ) %>% 
 
  # Add the receiver locations
  add_trace(
    type = "scatter3d",
    data = temp_rec_locs,
    x = ~deploy_long,
    y = ~deploy_lat,
    z = ~-instrument_depth,
    #color = ~group,
    #colorscale = c("red", "purple"),
    mode = "markers",
    marker = list(colour="burlywood", size=3)
  ) %>% 
 
 # Add the shapefile
 add_trace(
    type = "scatter3d",
    data = filter(fortify(SL_shapefile_EPSG3857), 
                  hole==F,
                  lat < 5558000,
                  lat > 5545000),
    x = ~long,
    y = ~lat,
    z = ~0,
    group = ~piece,
    mode = "lines",
    line = list(color = "black"),
    fillcolor = "transparent"
  ) %>% 


  # Set the y-axis limits
  layout(title = "3D snapshot of StoneyLake termal profile", #Set title
         yaxis = list(range = c(5545000, 5558000)), #Set Axis limits
         scene = list(camera = list(eye = list(x = -0.2, y = -1.2, z = 1.8),  # Set the initial camera position
                                    up = list(x = 0, y = 0, z = 1)           # Set the initial camera up vector
                      ))
         ) 



#Clean up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name
rm(list = paste(ls(pattern="KT_"))) #Remove environment objects with 'KT_' in name

