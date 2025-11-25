## --------------------------------------------------------------#
## Script name: temp_summaries_Mel
##
## Purpose of script: 
##        Generate summaries of fish use by receiver for Mel
##
##     
## Author: Paul Bzonek 
##
## Date Created: 2025-06-06
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#

#####Grab telemetry data##########################################----
#-------------------------------------------------------------#
temp <- df_spawn %>% ungroup() %>% 
    group_by(station_no) %>% 
    #filter(station_no ==7) %>% 
    summarize(deploy_lat = mean(deploy_lat),
              deploy_long = mean(deploy_long),
              depth = mean(meanDepth),
              residence_mean = mean(residence),
              residence_sum = sum(residence),
              residence_median = median(residence),
              detcount_mean = mean(detcount),
              detcount_sum = sum(detcount),
              detcount_median = median(detcount),
              countspawn = n(),
              countfish = data.table::uniqueN(animal_id))
#clipr::write_clip(temp)



#####Grab efishing data###########################################----
#-------------------------------------------------------------#
temp1 <- df_spawn %>% 
 filter(station_no %in% c("1", "7", "15", "14", "37", "18", "2", "3", "45", "25")) 

temp1 <- temp1 %>% 
 group_by(year, station_no, deploy_lat, deploy_long) %>% 
 summarize(residence_sum = sum(residence),
           residence_mean = mean(residence),
           fishcount = length(unique(animal_id))) %>% 
 ungroup() %>% 
 mutate(key_rec = row_number())

temp1

  ###Match efishing and receiver layers by distance
  #----------------------------#
  #Look at distances between all efishing obervations and receiver sites
  temp_sitecode <- cross_join(select(data_efish_obs, key_efish_obs = X, Total.Count, Date, Sample, Observation, lon_efish = Long, lat_efish=Lat),
                              select(temp1, key_rec, station_no, lon_rec=deploy_long, lat_rec=deploy_lat, year_rec = year)) %>% 
   mutate(year_efish = year(Date)) %>% 
   filter(year_efish==year_rec)
  
  #Filter cross table down to the min eucdist pair per fish (Eucdist is not in meters)
  temp_sitecode <- temp_sitecode %>% 
   mutate(eucdist = sqrt((lat_efish - lat_rec)^2 + (lon_efish - lon_rec)^2)) %>% #Calculate eucdist
   group_by(key_efish_obs) %>% slice_min(eucdist) %>% na.omit()# Find smallest distance
  
  #Reproject data to calculate eucledian distance in meters
  temp_sitecode <-temp_sitecode %>% rowwise() %>% 
   mutate(locs.efish = st_sfc(st_point(c(lon_efish, lat_efish)), crs = 4326),
          locs.hab = st_sfc(st_point(c(lon_rec, lat_rec)), crs = 4326),
          #Transform projections
          locs.efish_utm = st_transform(locs.efish, crs = 32618),
          locs.hab_utm = st_transform(locs.hab, crs = 32618),
          #Calculate eucdist in meters
          eucdist_m = as.numeric(st_distance(locs.efish_utm, locs.hab_utm))
          ) %>% 
   filter(eucdist_m < 1000) %>% #Remove outlier distance
   ungroup() #%>% as.data.frame() #Clean-up
  
  #Look at distribution of distances between fish and hab sites
   hist(temp_sitecode$eucdist_m, breaks=25)
  
      
  ###Apply cutoff distance to matching hab and efishing data
  temp_sitecode <- temp_sitecode %>% 
   filter(eucdist_m <= 350) 
  
  temp <- temp_sitecode %>% 
   group_by(station_no) %>% 
   reframe(sum(Total.Count))

  mean(temp$`sum(Total.Count)`)  

  
  
##### Plot combined data##########################################----
#-------------------------------------------------------------#
df_spawn %>% 
 ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #receivers
  geom_point(data = df_spawn_plot, 
             aes(x = deploy_long, y = deploy_lat, fill = countfish, size=as.numeric(residence_median)),
             shape=21)+
  geom_text(data = df_spawn_plot, aes(x = deploy_long, y = deploy_lat, label = station_no))+
  #efishing
  geom_point(data = filter(data_efish_obs, X %in% temp_sitecode$key_efish_obs,
                                           year(Date) >= 2022), 
             aes(x=Long, y=Lat, size=Total.Count, colour = Total.Count), alpha=0.5, shape=18)+
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_viridis_c(option="A", begin=0.2)+
  scale_size_continuous(range = c(2, 7.5))+ 
  theme(legend.position = "top")+
  theme_classic()  
  
  
  
  #Habitat site map
df_spawn %>% 
 ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #efishing
  geom_label(data = df_hab, 
             aes(x=lon, y=lat, label=PointID), alpha=0.5)+
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_viridis_c(option="A", begin=0.2)+
  scale_size_continuous(range = c(2, 7.5))+ 
  theme(legend.position = "top")+
  theme_classic()
