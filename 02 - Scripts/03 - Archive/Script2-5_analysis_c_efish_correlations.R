## --------------------------------------------------------------#
## Script name: Script2-5_analysis_c_efish_correlations
##
## Purpose of script: 
##    Analyze correlations between telemetry and electrofishing data (combined workflow)
##    Compare spawning events across data sources
##    
## Dependencies: 
##    - Script2-2_process_t_dataset.R (df_spawn)
##
## Author: Paul Bzonek 
##
## Date Created: 2024-04-10
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   2025-01-11: Renamed from efish_rec_correlations.R
## --------------------------------------------------------------#

# Look at telem and efishing data

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
  temp_sitecode <- cross_join(select(data_efish_obs, key_efish_obs = X, Date, Sample, Observation, lon_efish = Long, lat_efish=Lat),
                              select(temp1, key_rec, lon_rec=deploy_long, lat_rec=deploy_lat, year_rec = year)) %>% 
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
   filter(eucdist_m <= 350) %>% #350
   select(key_efish_obs, key_rec)

  #Update data_fetch with key_efish_obs
  df_correlation <- temp1 %>% 
   left_join(temp_sitecode, by = join_by(key_rec)) %>% 
   #filter(!is.na(key_efish_obs)) %>% 
   left_join(data_efish_obs, by = c("key_efish_obs"="X"))

   
  
###Plot data overlap
#----------------------------#
 ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #receivers
  geom_point(data = df_spawn_plot, 
             aes(x = deploy_long, y = deploy_lat, fill = countfish, size=as.numeric(residence_median)),
             shape=21)+
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
 

  
#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name

#Clean-up dataset
#----------------------------#
#Overwrite NAs and remove unmatched years
df_correlation <- df_correlation %>% 
 filter(year >= 2022) %>% 
 mutate(Total.Count = case_when(is.na(Total.Count) ~ 0,
                                TRUE ~ Total.Count)
        ) %>% 
 select(year:key_efish_obs, Total.Count)

#Summarize by receiver
df_correlation <- df_correlation %>% 
 group_by(year, station_no, deploy_lat, deploy_long, residence_sum, residence_mean, fishcount) %>% 
 reframe(fishcount_efish = sum(Total.Count))


#Plot the data
#----------------------------#
#fishcount
df_correlation %>% 
 ggplot(aes(x=fishcount, y=fishcount_efish, colour=as.factor(year)))+
  geom_point(size=3, alpha=0.5)

with(df_correlation, cor(fishcount, fishcount_efish))

#fishcount
df_correlation %>% 
 ggplot(aes(x=log(residence_sum), y=fishcount_efish, colour=as.factor(year)))+
  geom_jitter(size=3, alpha=0.5)

#fishcount
df_correlation %>% 
 ggplot(aes(x=residence_mean, y=fishcount_efish, colour=as.factor(year)))+
  geom_jitter(size=3, alpha=0.5)







