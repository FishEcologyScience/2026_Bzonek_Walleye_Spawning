## --------------------------------------------------------------#
## Script name: Script1-1_format_data_h
##
## Purpose of script: 
##    Clean raw habitat and electrofishing data (habitat workflow)
##    
## Dependencies: 
##    - Script0-1_load_packages.R (packages)
##    - Raw data files in 01 - Data/
##
## Author: Paul Bzonek 
##
## Date Created: 2024-07-21
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   2025-01-11: Renamed from Script1-1_format_data.R
## --------------------------------------------------------------#



###Import data
#----------------------------#
data_hab <- read.csv("01 - Data/HH_Walleye_NearshoreHabitatSurveys_2021.csv")
shapefile <- read_sf("01 - Data/HH_shapefile/HH_Water_Poly.shp") #st_read()
data_efish_obs <- read.csv("01 - Data/HH_WalleyeSpawningSurvey_2022-2024_SummarizedObservations.csv")
data_efish_fish <- read.csv("01 - Data/HH_WalleyeSpawningSurvey_2022-2024_SummarizedFishInfo2.csv")
data_waterlevel <- readxl::read_excel("01 - Data/RBG_Barrier Data_1996-2022.xlsx", sheet = "Water Quality") #Generate weekly water level estimates
data_fetch <- readxl::read_excel(path="01 - Data/HH_50mGrid_Depth_SAV_Fetch_WetlandDist_06June2023.xlsx")

###Set parameters
#----------------------------#
param_min_dist <- 25

   
#####Format data##################################################----
#-------------------------------------------------------------#
### Process misc data
#----------------------------#
###Project shapefile
shapefile <- st_transform(shapefile, "WGS84") #Also try "EPSG:4326"; 'EPSG:32724'

#Format water level data
data_waterlevel <- data_waterlevel %>% 
 dplyr::select(date=Date, year=Year, month=Month, lift_time = `Lift Time`, water_level = `W_depth(CP)`, air_temp = `Air Temp`,
        wind_speed = `Wind km/hr`, wind_dir = `Wind Dir`) %>% 
 filter(month=="April", 
        year %in% c(2016:2023)) %>% 
 mutate(week = lubridate::week(date)) %>% 
 group_by(year, week) %>% 
 group_by(year) %>% 
 reframe(water_level = mean(water_level, na.rm=TRUE),
         air_temp = mean(air_temp, na.rm=TRUE))



### Format Shoreline Habitat data
#----------------------------#
### Habitat data
#Extract transect ID
data_hab <- mutate(data_hab, transect = gsub(pattern="_", replacement="", x=substr(PointID, 1,3),))

#Summarize habitat data
df_hab <- data_hab %>% 
 #NOTE: transects start in the water, deeper depths are more negative. Depths > 0 are above surface
 #Raw slope is run/rise and needs to be flipped to rise/run
 #Look at underwater habitat
 filter(Depth__m_ < 0) %>% #glm model performance improves after removing
 #Make light polution numeric
 mutate(LightPoll_numeric = case_when(LightPoll_Cat == "None" ~ 1,
                                      LightPoll_Cat == "Low" ~ 2,
                                      LightPoll_Cat == "Moderate" ~ 3,
                                      LightPoll_Cat == "High" ~ 4)) %>% 
 #Summarize habitat to transect level
 #group_by(transect) %>% 
 group_by(PointID) %>%
 summarize(lat = mean(Start_Latitude),
           lon = mean(Start_Longitude),
           Light = max(LightPoll_numeric),
           Slope = mean(Interval_Slope, na.rm=T),
           Slope = 1/Slope, #Take inverse to correct raw dataset
           Slope = atan(Slope)* 180/pi, #ArcTan of Rise/Run is slope in radians, convert to degrees with * 180/pi
           DepthIndex = max(DistanceSum),
           DepthCategory = case_when(DepthIndex < 5 ~ "Steep",
                                DepthIndex > 5 & DepthIndex < 15~ "Gentle",
                                DepthIndex > 15 ~ "Shallow"),
           #get transect totals for all habitat traits
           across(c(Substrate_Siltation,  Fine:Terrestrial, CheckSum), sum)
           ) %>% 
 #Get habitat proportions by dividing habitat sums by transect sums 
 mutate(across(c(Fine:Terrestrial), ~ . / CheckSum)) %>% 
 select(-CheckSum)

#Add substrate diversity
df_hab$SubstrateDiversity <- df_hab %>% 
 select(Fine:Armourstone) %>% diversity(index="simp")


#Plot habitat data
data_hab %>% 
 full_join(select(df_hab, PointID, DepthCategory)) %>% 
 ggplot(aes(x=DistanceSum, y=-Depth__m_, colour=DepthCategory))+
  geom_point()+
  geom_line()+
  facet_wrap(~DepthCategory+PointID)+
  theme_bw()
 
 
 
#####Format efishing data#########################################----
#-------------------------------------------------------------#
#Set dates
data_efish_obs <- data_efish_obs %>% mutate(Date = as.Date(YMD, format = "%m/%d/%Y"))
data_efish_fish <- data_efish_fish %>% mutate(Date = as.Date(YMD, format = "%m/%d/%Y"))
str(data_efish_fish)


###Plot spawning events
#----------------------------#
 ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #efishing
  geom_point(data = data_efish_obs,
             aes(x=Long, y=Lat, size=Total.Count), colour="firebrick", alpha=0.5)+
  #hab data
  geom_point(data = data_hab,
             aes(x=Start_Longitude, y=Start_Latitude), colour="burlywood", size=3)+
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  ggtitle("All walleye (red) and habitat data (gold)")+
  scale_fill_viridis_c()+
  theme_classic()

 
 
   ###Match efishing and shoreline habitat data by distance
   #----------------------------#
   #Look at distances between all efishing obervations and habitat sites
   temp_sitecode <- cross_join(select(data_efish_obs, key_efish_obs = X, Date, Sample, Observation, lon_efish = Long, lat_efish=Lat),
                               unique(select(data_hab, PointID, lon_hab=Start_Longitude, lat_hab=Start_Latitude))) 
   
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
   
       ###Identify fish-hab site cut-off distance
       #----------------------------#
       #Look at distances between habitat sites
       temp_min_site_dist <- temp_sitecode %>% 
         #Pull useful info
         select(PointID, locs.hab_utm) %>% 
         unique() %>% 
         #Make a distance matrix
         mutate(dist_matrix = s2::s2_distance_matrix(locs.hab_utm, locs.hab_utm)) %>% 
         #Find minimum distance per PointID
         rowwise() %>% 
         mutate(min_dist = min(dist_matrix[dist_matrix > 0], na.rm = TRUE)) %>%  # Exclude 0s and compute minimum
         ungroup() %>% 
         select(PointID, min_dist)
       
       #Plot distrubtion of distances
        hist(temp_min_site_dist$min_dist)
       
       
   ###Apply cutoff distance to matching hab and efishing data
   df_habfish_key <- temp_sitecode %>% 
    filter(eucdist_m <= param_min_dist) %>% 
    select(key_efish_obs, Sample, Observation, Date, PointID)

#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name

 

  ###Match efishing and fetch habitat layers by distance
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
  temp_sitecode <- temp_sitecode %>% 
   #filter(eucdist_m <= param_min_dist) %>% 
   select(key_efish_obs, key_fetch)

  #Update data_fetch with key_efish_obs
  df_fetch <- data_fetch %>% 
   left_join(temp_sitecode, by = c("key" = "key_fetch")) %>% 
   filter(!is.na(key_efish_obs)) %>% 
   select(key_efish_obs, WetlandDistance = WL_Dist.m, SAV_Cover, Fetch=Mean.Fetch.km)
  
#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name

 

   ###Match shoreline and fetch habitat layers by distance
   #----------------------------#
   #Look at distances between all efishing obervations and habitat sites
   temp_sitecode <- cross_join(select(df_hab, PointID, lat, lon),
                               unique(select(data_fetch, key_fetch = key, lon_hab=Longitude, lat_hab=Latitude))) 
   
   #Filter cross table down to the min eucdist pair per fish (Eucdist is not in meters)
   temp_sitecode <- temp_sitecode %>% 
    mutate(eucdist = sqrt((lat - lat_hab)^2 + (lon - lon_hab)^2)) %>% #Calculate eucdist
    group_by(PointID) %>% slice_min(eucdist) %>% na.omit()# Find smallest distance
   
   #Reproject data to calculate eucledian distance in meters
   temp_sitecode <-temp_sitecode %>% rowwise() %>% 
    mutate(locs.efish = st_sfc(st_point(c(lon, lat)), crs = 4326),
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
   temp_sitecode <- temp_sitecode %>% 
    filter(eucdist_m <= param_min_dist) %>% 
    select(PointID, key_fetch)
 

#Clean-up
#rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name

#####Combine e-fishing and habitat data###########################----
#-------------------------------------------------------------#

###Dataset of fish biology
#----------------------------#
#Build dataset
data_habfish_raw <- df_habfish_key %>% 
 left_join(df_hab, by = c("PointID")) %>% #Add shoreline habitat
 left_join(data_efish_fish, by = c("Sample", "Observation", "Date")) %>% #Add efishing data
 left_join(df_fetch, by = c("key_efish_obs")) %>% #Add fetch and habitat data
 select(-c(Lat, Long, YMD)) #Drop unwanted columns


###Split up data
set.seed(param_seed)
data_habfish_raw$nrow <- 1:length(data_habfish_raw$Total.Count)
head(data_habfish_raw)
set.seed(param_seed)
data_habfish_raw.train <- data_habfish_raw %>% sample_frac(0.7) # 70/30 split. 
data_habfish_raw.test <- data_habfish_raw[!(data_habfish_raw$nrow %in% data_habfish_raw.train$nrow),]
# data_habfish_raw.tune <- data_habfish_raw.test %>% sample_frac(0.5) 
# data_habfish_raw.test <- data_habfish_raw.test[!(data_habfish_raw.test$nrow %in% data_habfish_raw.tune$nrow),]

### Dataset of raw and pseudo detections
#----------------------------#
data_habfish_pseudo <- df_habfish_key %>% 
 left_join(df_hab, by = c("PointID")) %>% #Need to remove date
 left_join(data_efish_obs, by = c("Sample", "Observation", "Date")) %>% 
 left_join(df_fetch, by = c("key_efish_obs")) %>% #Add fetch and habitat data
 select(-c(Lat, Long, YMD)) #Drop unwanted columns

   ###add pseudo absences
   #Make a dataset of all sites and years
   temp_absences <- expand.grid(PointID = unique(df_hab$PointID),
                                Year = unique(data_efish_obs$Year),
                                Total.Count = 0,
                                Number.Captured = 0,
                                Number.Missed = 0,
                                Pseudo0 = TRUE) %>% 
                    left_join(temp_sitecode, by="PointID") %>% 
                    left_join(select(data_fetch, key_fetch = key, WetlandDistance=WL_Dist.m, SAV_Cover, Fetch=Mean.Fetch.km))

   #Summarize the sites that did recieve fish
   temp_pres_key <- data_habfish_raw %>% 
    select(PointID, Year) %>% 
    unique()
   #Keep true site absences
   temp_absences <- anti_join(temp_absences, temp_pres_key)
   #Add habitat data
   temp_absences <- temp_absences %>% 
    left_join(df_hab, by = c("PointID"))  #Need to remove date

#Combine data and add a binary column
data_habfish_pseudo <- bind_rows(data_habfish_pseudo, temp_absences) %>% 
 mutate(Occurrence = case_when(Total.Count > 1 ~ 1, #Turn counts into 1s
                             TRUE ~ Total.Count),
        Occurrence = as.factor(Occurrence),
        Year = as.factor(Year)
        )
with(data_habfish_pseudo, table(Occurrence, Year)) %>% colSums()

###Split up data
set.seed(param_seed)
data_habfish_pseudo$nrow <- 1:length(data_habfish_pseudo$Total.Count)
head(data_habfish_pseudo)
set.seed(param_seed)
data_habfish_pseudo.train <- data_habfish_pseudo %>% sample_frac(0.7) # 70/30 split. 
data_habfish_pseudo.test <- data_habfish_pseudo[!(data_habfish_pseudo$nrow %in% data_habfish_pseudo.train$nrow),]
# data_habfish_pseudo.tune <- data_habfish_pseudo.test %>% sample_frac(0.5) 
# data_habfish_pseudo.test <- data_habfish_pseudo.test[!(data_habfish_pseudo.test$nrow %in% data_habfish_pseudo.tune$nrow),]

#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name
rm(df_habfish_key)









