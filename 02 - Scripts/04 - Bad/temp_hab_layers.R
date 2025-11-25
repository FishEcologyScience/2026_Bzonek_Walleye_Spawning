###temp_hab_layers
#----------------------------#
#Generate weekly water level estimates
data_waterlevel <- readxl::read_excel("01 - Data/RBG_Barrier Data_1996-2022.xlsx", sheet = "Water Quality")

data_waterlevel <- data_waterlevel %>% 
 select(date=Date, year=Year, month=Month, water_level = `W_depth(CP)`, wind_speed = `Wind km/hr`, wind_dir = `Wind Dir`) %>% 
 filter(month=="April", year %in% c(2016:2023)) %>% 
 mutate(week = lubridate::week(date)) %>% 
 group_by(year, week) %>% 
 reframe(water_level = mean(water_level, na.rm=TRUE))



###Add Habitat Layers
#----------------------------#
data_fetch <- readxl::read_excel(path="01 - Data/HH_50mGrid_Depth_SAV_Fetch_WetlandDist_06June2023.xlsx")

#Plot habitat layers
 ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #Habitat layer
  geom_point(data = data_fetch,
             aes(x=Longitude, y=Latitude, 
                 colour=Mean.Fetch.km), 
                 #colour=SAV_Cover),
                 #colour=WL_Dist.m),
             alpha=0.5)+
  #efishing
  geom_point(data = data_habfish_raw,
             aes(x=lon, y=lat, size=Total.Count), colour="firebrick", alpha=0.25)+
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  #ggtitle("Habitat Layer with SAV and fetch")+
  ggtitle("Fetch")+
  scale_colour_viridis_c()+
  theme_classic()
 
 
###Match efishing and habitat layers by distance
#----------------------------#
#Look at distances between all efishing obervations and habitat sites
temp_sitecode <- cross_join(select(data_efish_obs, key_efish_obs = X, Date, Sample, Observation, lon_efish = Long, lat_efish=Lat),
                            unique(select(data_fetch, key_fetch = key, lon_hab=Longitude, lat_hab=Latitude))) 

#Filter cross table down to the min eucdist pair per fish (Eucdist is not in meters)
temp_sitecode <- temp_sitecode %>% 
 mutate(eucdist = sqrt((lat_efish - lat_hab)^2 + (lon_efish - lon_hab)^2)) %>% #Calculate eucdist
 group_by(key_efish_obs) %>% slice_min(eucdist) %>% na.omit()# Find smallest distance

#Reproject data to calculate eucledian distance in meters
temp_sitecode <-temp_sitecode %>% rowwise() %>% 
 mutate(locs.efish = st_sfc(st_point(c(lon_efish, lat_efish)), crs = 4326),
        locs.hab = st_sfc(st_point(c(lon_hab, lat_hab)), crs = 4326),
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
df_habfish_key2 <- temp_sitecode %>% 
 select(key_efish_obs, Sample, Observation, Date, key_fetch)

#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name

