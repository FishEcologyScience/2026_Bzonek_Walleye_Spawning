## --------------------------------------------------------------#
## Script name: Script3-1_combine_results
##
## Purpose of script: 
##    Combine analysis results from various data sources
##    
##
## Author: Paul Bzonek 
##
## Date Created: 2024-12-03
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   Requires: format_data, format_data_t
## --------------------------------------------------------------#




###Plot spawning events
#----------------------------#
 ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #receivers
  geom_point(data = df_spawn_plot,
             aes(x = deploy_long, y = deploy_lat, size = countfish, fill=as.numeric(residence_median)),
             shape=21)+
  #efishing
  geom_point(data = data_efish_obs,
             aes(x=Long, y=Lat, size=Total.Count), colour="firebrick", alpha=0.5)+
  #hab data
  geom_point(data = data_hab,
             aes(x=Start_Longitude, y=Start_Latitude), colour="burlywood")+
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_viridis_c()+
  theme_classic()+
  theme(legend.position = "NA")


