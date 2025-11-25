## --------------------------------------------------------------#
## Script name: Script2-3_analysis_t_homerange
##
## Purpose of script: 
##    Calculate home range metrics from telemetry data (telemetry workflow)
##    
## Dependencies: 
##    - Script2-2_process_t_dataset.R (data_not_spawn, df_spawn)
##
## Author: Paul Bzonek 
##
## Date Created: 2025-01-09
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   2025-01-11: Renamed from Script2-Y_analysis_t_homerange.R
## --------------------------------------------------------------#


#####Minimum convex polygon for all detections ###################----
#-------------------------------------------------------------#
df_homerange <- data_not_spawn %>% dplyr::select(deploy_long, deploy_lat, animal_id) #Make minimalist dataframe
coordinates(df_homerange) <- c("deploy_long", "deploy_lat")
proj4string(df_homerange) <- CRS("+proj=longlat +datum=WGS84")
mcp50 <-mcp(df_homerange, percent=50, unout = c("m2"))
mcp95 <-mcp(df_homerange, percent=95, unout = c("m2"))


###map of rec
#ggmap(HHmap, extent='normal')+
ggplot()+
   geom_polygon(data = fortify(mcp95),  aes(long, lat, group=id), fill = "#13cec1", colour = "#13cec1", alpha = 0.15)+
   geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
   scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
   scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
   ylab("Latitude")+
   xlab("Longitude")


#Subset homerange to specific fish
ID_values_present <- "A69-1105-79"
temp_mcptest <- mcp50[mcp50@data$id %in% ID_values_present, ] #Subset mcp50 by list (doesnt work)
plot(temp_mcptest)


# Look at fish that dont show up on interactive homerange
#----------------------------#
data_det %>% 
 filter(animal_id %in% c("A69-9002-4494", "A69-9002-4495", "A69-9002-4497")) %>% 
 count(animal_id, station_no)

