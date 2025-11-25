## --------------------------------------------------------------#
## Script name: Script1-2_format_data_telemetry
##
## Purpose of script: 
##    Clean raw telemetry data 
##    
##
## Author: Paul Bzonek 
##
## Date Created: 2024-12-03
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#



###Import data
#----------------------------#
data_det_raw <- readRDS("01 - Data/HH_detections_clipped_filtered_2015-2023_good.rds")
data_rec_locs_raw <- read.csv("01 - Data/GLATOS_receiverLocations_20240122_234632.csv")
data_fish <- readxl::read_excel("01 - Data/HH_Fish_Workbook_Jun2024.xlsx")
df_key <- readxl::read_excel("01 - Data/receiver_transect_key2.xlsx")
df_SunCategory <- readRDS("01 - Data/data_det_raw_SunPosition_2024-07-12.rds")
#data_waterlevel <- read.csv("01 - Data/WaterLevel_02HB017.csv") #From https://wateroffice.ec.gc.ca/report/historical_e.html?stn=02HB017&dataType=Monthly&parameterType=Level&first_year=2016&last_year=2022&mode=Table&page=historical&year=2022&start_year=1850&end_year=2025

   
#####Format data##################################################----
#-------------------------------------------------------------#


### Process misc data
#----------------------------#
###Receiver-Habitat Key
#Format Key
df_key <- df_key %>% 
 mutate(station_no = as.factor(station), 
        transect = as.factor(transect))

###Fish data
data_fish <- data_fish %>% 
 filter(COMMON_NAME_E == "Walleye", !is.na(`Pinger Full ID`)) %>% 
 dplyr::select(animal_id = `Pinger Full ID`, 
        # release_location=`Release Location`, 
        # release_latitude = `Release Latitude`, 
        # release_longitude = `Release Longitude`,
        length_standard =`Standard (mm)`,
        length_fork=`Fork (mm)`,
        length_total=`Total (mm)`,
        mass=`Mass (g)`) %>% 
 mutate(length_standard = as.numeric(length_standard),
        length_fork=as.numeric(length_fork),
        length_total=as.numeric(length_total))


### format receiver data
#----------------------------#
#Hamilton Array
data_rec_locs_raw <- data_rec_locs_raw %>% 
 filter(glatos_array =="HAM")

#Format datatable
data_rec_locs_raw <- data_rec_locs_raw %>% rowwise() %>% 
  mutate(ins_serial_no = as.numeric(ins_serial_no),
         ins_model_no = as.factor(ins_model_no),
         station_no = as.factor(station_no),
         receiver_sn = paste(ins_model_no, ins_serial_no, sep="-"),
         #Fix date formats
         deploy_date_time = as.POSIXct(deploy_date_time, format = "%Y-%m-%d %H:%M:%OS"),
         recover_date_time = as.POSIXct(recover_date_time, format = "%Y-%m-%d %H:%M:%OS"),
         #Overwrite Inf values
         recover_date_time2 = if_else(is.infinite(recover_date_time),
                                     true = as.POSIXct(Sys.time(), format = "%Y-%m-%d %H:%M:%OS"),
                                     false = as.POSIXct(recover_date_time))
         ) %>% 
  #Keep columns of interest
  select(receiver_sn, ins_serial_no, station_no, deploy_lat, deploy_long, ins_model_no, bottom_depth,
         deploy_date_time, recover_date_time, recover_date_time2, comments) %>% 
  filter(!is.na(ins_serial_no)) %>% 
  ungroup()

#Clean-up reciever dataframe
data_rec_locs_raw <- data_rec_locs_raw %>%
 mutate(deployDate = strftime(deploy_date_time, tz="EST", format="%Y-%m-%d"),
        deployDate = as.POSIXct(deployDate, tz="EST", format="%Y-%m-%d"),
        recoverDate = strftime(recover_date_time, tz="EST", format="%Y-%m-%d"),
        recoverDate = as.POSIXct(recoverDate, tz="EST", format="%Y-%m-%d")) %>% 
 group_by(ins_serial_no) %>% mutate(receiver_deployment = row_number()) %>% 
 group_by(station_no) %>% mutate(station_rec_count = n_distinct(ins_serial_no)) %>% 
 select(station_no, receiver_sn, deploy_lat, deploy_long, 
        bottom_depth, ins_model_no, ins_serial_no, 
        deploy_date_time, recover_date_time, recover_date_time2, 
        deployDate, recoverDate, receiver_deployment, station_rec_count, comments) %>% 
 ungroup()

#Keep recievers that last for the full duration
temp_df_longterm <- data_rec_locs_raw %>% 
  group_by(station_no) %>% 
  summarize(firstdeploy = min(deployDate), lastrecover = max(recoverDate, na.rm=T)) %>% 
  filter(firstdeploy < as.POSIXct("2017-01-01"),
         lastrecover > as.POSIXct("2023-01-01")) %>% 
 ungroup()

#Keep data from longeterm recievers
temp_rec_locs_longterm <- filter(data_rec_locs_raw, station_no %in% temp_df_longterm$station_no) %>% 
 ungroup()
data_rec_locs <- filter(data_rec_locs_raw, 
                        station_no %in% temp_rec_locs_longterm$station_no)

# #Keep data from manually selected recievers
# data_rec_locs <- filter(data_rec_locs_raw, 
#                          station_no %in% c(#First round picks
#                                            #"1", "7", "15", "23", "37",
#                                            #"28", "18", "2", "3", "12", "25",
#                                            #Second round picks - Used in telem modelling
#                                            "1", "7", "15", "14", "37",
#                                            "18", "2", "3", "45", "25"))
data_rec_locs <- left_join(data_rec_locs, df_key)
data_rec_locs <- left_join(data_rec_locs, df_hab, by=c("transect"="PointID"))                           
  
#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name
     
 
                  
### format raw detection data
#----------------------------#
#Filter data and add sun position
data_det_raw <- data_det_raw %>% 
 filter(common_name_e =="Walleye") %>% #Only Walleye
 mutate(sensor_value.Cal = (as.numeric(Intercept) + (as.numeric(sensor_value)*as.numeric(Slope)))) #Calculate depth


### format spawning detection data
#----------------------------#
data_det <- data_det_raw %>% 
 filter(format(detection_timestamp_EST, "%m") == "04", #Only April
        station_no %in% df_key$station_no)   #Only flagged stations
data_det <- cbind(data_det, df_SunCategory)

#Keep sensor data and calculate depth
data_det <- data_det %>% 
 filter(SunCategory %in% c("dusk", "night"), #Only keep night and dusk
        !is.na(sensor_value))
 
#Add fish data
data_det <- left_join(data_det, data_fish, by=join_by(animal_id))


gc() #Clear memory

#Plot a subset of depths
ggExtra::ggMarginal({
                      data_det %>% sample_n(10000) %>% 
                      ggplot(aes(y=-sensor_value.Cal, x=format(detection_timestamp_utc, "%d")))+
                        geom_jitter(shape=21, alpha=0.2)+
                        geom_hline(yintercept=-2)+
                        labs(x="day of month (April)", y="depth (m)", title="Walleye detection depth") 
                     },  margins = "y", size=10, colour="red"
                    )




### Summarize detection data
#----------------------------#
###Raw dataset
#Date ranges
paste(paste("min", min(data_det$detection_timestamp_EST)),
      paste("median", median(data_det$detection_timestamp_EST)),
      paste("max", max(data_det$detection_timestamp_EST)),
      collapse ="\n")

#Detections per fish 
data_det %>% count(format(detection_timestamp_EST, "%Y"), #Year
                      animal_id) #Fish ID 

#Fish per year 
data_det %>% 
 group_by(format(detection_timestamp_EST, "%Y")) %>%
 summarize(count = length(unique(animal_id)))


###Plot habitat and receiver data
#----------------------------#
###map of rec
ggmap(HHmap, extent='normal')+
 #ggplot()+
   geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
   scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
   scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
   ylab("Latitude")+
   xlab("Longitude")+
   #Receivers
   geom_point(data=data_rec_locs, aes(x=deploy_long, y=deploy_lat, colour=station_no),  shape=21, size=8)+
   geom_text(data=slice_head(data_rec_locs, n=1, by=station_no),
              aes(x=deploy_long, y=deploy_lat, label = station_no), colour= "firebrick", size=3)+
   #Transects
   geom_point(data=data_rec_locs, aes(x=lon, y=lat, colour=station_no), alpha=0.5)+
   geom_text(data=slice_head(data_rec_locs, n=1, by=transect), 
             aes(x=lon, y=lat,label = transect), colour = "black", size=3)+
   theme_bw()



#####Summarize fish movements##################################----
#-------------------------------------------------------------#
#Look at fish movements
df_movement <- data_det %>% 
 select(detection_timestamp_utc, To_animal_id=animal_id, 
        To_station=station_no, To_lat=deploy_lat, To_lon=deploy_long,
        mass, length_total) %>% 
 mutate(param_year = year(detection_timestamp_utc)) %>% # Add year column to group by year
 arrange(To_animal_id, detection_timestamp_utc) %>% 
 group_by(To_animal_id, param_year) %>% # Group by animal AND year to prevent cross-year lag calculations
 mutate(From_station = lag(To_station), From_animal_id=lag(To_animal_id), 
        From_lat=lag(To_lat), From_lon=lag(To_lon), 
        From_timestamp = lag(detection_timestamp_utc), # Now lag() only operates within same year
        lagtime = difftime(detection_timestamp_utc, From_timestamp, units=c("hours")), # Lag times limited to within-year
        moveID = paste0(From_animal_id, From_station, To_station),
        moveID = cumsum(moveID != lag(moveID, default = first(moveID)))) %>% 
 ungroup() %>% # Remove grouping
 select(-param_year) # Remove temporary year column

#Filter and format dataset
df_residency <- df_movement %>% 
 filter(To_animal_id==From_animal_id,lagtime < 12) %>% #Detections of same fish with 12 hours of previous
 group_by(To_animal_id, From_station, moveID) %>% 
 summarize(detcount=n(),
           time_start = min(From_timestamp),
           time_end = max(detection_timestamp_utc),
           residence = difftime(time_end, time_start, units = c("hours"))) %>% 
filter(detcount >= 3,
        residence >= 0.32) %>% 
 ungroup()

#Look at depth for spawn-condition detections
data_spawn <- df_movement %>% 
 filter(moveID %in% df_residency$moveID) %>% 
 select(detection_timestamp_utc, animal_id=To_animal_id, station_no=To_station, 
        moveID) %>% 
 left_join(select(df_residency, moveID, residence, time_start, time_end, detcount), by=join_by(moveID)) %>% 
 left_join(data_det, by=join_by(detection_timestamp_utc, animal_id, station_no)) %>% 
 group_by(moveID) %>% 
 mutate(meanDepth = mean(-sensor_value.Cal)) %>% 
 filter(meanDepth > -2) %>% 
 select(-c(glatos_array, transmitter_codespace, tag_model, tag_serial_number, 
           sex, release_group, glatos_caught_date, DST, tag_type)) %>% 
 ungroup()

#Plot detection depths
data_spawn %>% 
 ggplot(aes(y=-sensor_value.Cal, x=format(detection_timestamp_utc, "%d")))+
  geom_jitter(shape=21, alpha=0.2)+
  geom_hline(yintercept=-2)+
  labs(x="day of month (April)", y="depth (m)")

#Summarize data to spawning events
df_spawn <- data_spawn %>% 
 group_by(animal_id, moveID) %>% 
 summarize(station_no = first(station_no),
           deploy_lat = first(deploy_lat),
           deploy_long = first(deploy_long),
           meanDepth = first(meanDepth),
           length_total = first(length_total),
           mass = first(mass),
           residence = first(residence),
           time_start = first(time_start),
           time_end = first(time_end), 
           detcount = first(detcount),
           year = year(time_start),
           week = week(time_start)
           )

# df_spawn <- left_join(df_spawn, #Add water level
#                       select(data_waterlevel, Year, water_level = Apr),
#                       by = c("year" = "Year"))
# rm(data_waterlevel)

#Summarize spawning events per receiver
df_spawn_plot <- df_spawn %>% 
 group_by(station_no) %>% 
 summarize(deploy_lat = mean(deploy_lat, na.rm=T),
           deploy_long = mean(deploy_long, na.rm=T),
           depth = mean(meanDepth, na.rm=T),
           residence_mean = mean(residence, na.rm=T),
           residence_sum = sum(residence, na.rm=T),
           residence_median = median(residence, na.rm=T),
           detcount_mean = mean(detcount, na.rm=T),
           detcount_sum = sum(detcount, na.rm=T),
           detcount_median = median(detcount, na.rm=T),
           countspawn = n(),
           countfish = data.table::uniqueN(animal_id))

#Plot spawning events
ggmap(HHmap, extent='normal')+
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  geom_point(data = df_spawn_plot,
             aes(x = deploy_long, y = deploy_lat, size = countfish, fill=as.numeric(residence_median)),
             shape=21)+
 scale_fill_viridis_c()



### Summarize fish
#----------------------------#
temp_fish_summary <- data.frame(animal_id = unique(df_spawn$animal_id)) %>% 
  left_join(data_fish) %>% 
  summarize(FL_mean = mean(length_fork, na.rm=TRUE),
            FL_SD = sd(length_fork, na.rm=TRUE),
            Mass_mean = mean(mass, na.rm=TRUE),
            Mass_SD = sd(mass, na.rm=TRUE),)

temp_fish_summary


