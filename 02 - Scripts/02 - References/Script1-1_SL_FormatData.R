## --------------------------------------------------------------#
## Script name: Script1-1_SL_FormatData.R 
##
## Purpose of script: 
##    Clean raw telemetry data 
##    
##
## Author: Paul Bzonek 
##
## Date Created: 2023-06-26
##
## --------------------------------------------------------------#  
## Modification Notes: Code modified from Brownscombe's "initial processing.R"  
##   
## --------------------------------------------------------------#



#####Pull raw data################################################----
#-------------------------------------------------------------#
###Import data
#----------------------------#
SL_data_rec_locs <- read_xlsx(path="01 - Data/otn-instrument-deployment-short-form_Stoney_UTC_12July2023.xlsx",
                          sheet=2, skip=3)

SL_data_tags <- read_xlsx(path="01 - Data/otn_metadata_tagging_Stoney_12June2023_Fathom_format.xlsx",
                          sheet=2, skip=3)

temp_ref_tags <- read_xlsx(path="01 - Data/range testing metadata.xlsx",
                          sheet=4)

SL_data_det_raw <- read.csv("01 - Data/SL_dets_timecorrected_2023-07-20.csv")

df_timestamp_SunPosition <- readRDS("01 - Data/RDS objects/SL_timestamp_SunPosition_2023-08-04.RDS")


###Import raw tag specs 
   #Make the clean list of raw files
   files <- append(x = c(), #Make a new vector to build from
                   list.files(path = "01 - Data/Tag Specs/", #Look in location...
                               recursive=T)) #Pull out `.csv` files from folders
   files <- paste0("01 - Data/Tag Specs/", files) #return files names to file path
   #Import the data 
   SL_data_tag_specs <- rio::import_list(files, sheet=2, rbind = TRUE, fill=TRUE, rbind_fill=TRUE) #Make a single LB table
      
   rm(files) # clean-up

   
#####Format data##################################################----
#-------------------------------------------------------------#
### Raw detections
#----------------------------#
SL_data_det_raw <- SL_data_det_raw %>% 
 select(Timestamp = Date.and.Time..UTC., Receiver, Transmitter, Sensor.Value, Sensor.Unit) %>% 
 mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS"))


### Receiver locations
#----------------------------#
SL_data_rec_locs <- SL_data_rec_locs %>% rowwise() %>% 
 select(-c(RISER_LENGTH, INSTRUMENT_DEPTH, CODE_SET, AR_MODEL_NO, AR_SERIAL_NO,
           `DEPLOYED_BY (Lead Technicians)`, FILENAME)) %>% 
  mutate(INS_SERIAL_NO = as.numeric(INS_SERIAL_NO),
         INS_MODEL_NO = as.factor(INS_MODEL_NO),
         STATION_NO = as.factor(STATION_NO),
         receiver_sn = paste(INS_MODEL_NO, INS_SERIAL_NO, sep="-"),
         #Fix date formats
            #Deploy column a mix of numbers and character strings
              DEPLOY_DATE_TIME_num = as.POSIXct(as.numeric(`DEPLOY_DATE_TIME   (yyyy-mm-ddThh:mm:ss)`)*(60*60*24),
                                                origin="1899-12-30"),
              DEPLOY_DATE_TIME_char = as.POSIXct(`DEPLOY_DATE_TIME   (yyyy-mm-ddThh:mm:ss)`,
                                                  format = "%Y/%m/%dT%H:%M:%OS"),
            DEPLOY_DATE_TIME = max(DEPLOY_DATE_TIME_num, DEPLOY_DATE_TIME_char, na.rm=T),
            #Recover column a mix of numbers and character strings
              RECOVER_DATE_TIME_num = as.POSIXct(as.numeric(`RECOVER_DATE_TIME (yyyy-mm-ddThh:mm:ss)`)*(60*60*24),
                                                origin="1899-12-30"),
              RECOVER_DATE_TIME_char = as.POSIXct(`RECOVER_DATE_TIME (yyyy-mm-ddThh:mm:ss)`,
                                                  format = "%Y/%m/%dT%H:%M:%OS"),
            RECOVER_DATE_TIME = max(RECOVER_DATE_TIME_num, RECOVER_DATE_TIME_char, na.rm=T),
         #Fill in missing recover dates
         #Overwrite Inf values
         RECOVER_DATE_TIME2 = if_else(is.infinite(RECOVER_DATE_TIME),
                                     true = as.POSIXct(Sys.time(), format = "%Y-%m-%d %H:%M:%OS"),
                                     false = as.POSIXct(RECOVER_DATE_TIME))
         ) %>% 
  #Keep columns of interest
  select(receiver_sn, INS_SERIAL_NO, STATION_NO, DEPLOY_LAT, DEPLOY_LONG, INS_MODEL_NO, BOTTOM_DEPTH,
         DEPLOY_DATE_TIME, RECOVER_DATE_TIME, RECOVER_DATE_TIME2, COMMENTS) %>% 
  filter(!is.na(INS_SERIAL_NO))

#Clean-up reciever dataframe
SL_data_rec_locs <- SL_data_rec_locs %>%
 mutate(deployEST = strftime(DEPLOY_DATE_TIME, tz="EST", format="%Y-%m-%d"),
        deployEST = as.POSIXct(deployEST, tz="EST", format="%Y-%m-%d"),
        recoverEST = strftime(RECOVER_DATE_TIME, tz="EST", format="%Y-%m-%d"),
        recoverEST = as.POSIXct(recoverEST, tz="EST", format="%Y-%m-%d")) %>% 
 group_by(INS_SERIAL_NO) %>% mutate(receiver_deployment = row_number()) %>% 
 group_by(STATION_NO) %>% mutate(station_rec_count = n_distinct(INS_SERIAL_NO)) %>% 
 rename(station_no=STATION_NO, deploy_lat=DEPLOY_LAT, deploy_long=DEPLOY_LONG, 
        bottom_depth=BOTTOM_DEPTH, ins_model_no=INS_MODEL_NO, ins_serial_no=INS_SERIAL_NO, 
        deploy_date_time=DEPLOY_DATE_TIME, recover_date_time=RECOVER_DATE_TIME, recover_date_time2=RECOVER_DATE_TIME2, 
        comments=COMMENTS) %>% 
 select(station_no, receiver_sn, deploy_lat, deploy_long, 
        bottom_depth, ins_model_no, ins_serial_no, 
        deploy_date_time, recover_date_time, recover_date_time2, 
        deployEST, recoverEST, receiver_deployment, station_rec_count, comments)

 

### Tag data
#----------------------------#
SL_data_tags <- SL_data_tags %>% 
  select(TAG_MODEL, TAG_SERIAL_NUMBER, TAG_ID_CODE, TAG_CODE_SPACE,
         COMMON_NAME_E, SCIENTIFIC_NAME, 
         `LENGTH (m)`, `LENGTH2 (m)`, `WEIGHT (kg)`, SEX, DNA_SAMPLE_TAKEN,
         TAG_ACTIVATION_DATE, EST_TAG_LIFE, TAGGER,
         CAPTURE_LOCATION, CAPTURE_LATITUDE, CAPTURE_LONGITUDE, 
         RELEASE_GROUP, RELEASE_LOCATION, RELEASE_LATITUDE, RELEASE_LONGITUDE, UTC_RELEASE_DATE_TIME, COMMENTS) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(Transmitter = paste(TAG_CODE_SPACE, TAG_ID_CODE, sep="-"),
         TAG_SERIAL_NUMBER = as.character(TAG_SERIAL_NUMBER))

#Identify tags associated to multiple fish
temp_duplicate_tags <- SL_data_tags %>% 
 arrange(Transmitter) %>% unique() %>% 
 group_by(Transmitter) %>% 
 filter(n()>1)

#Remove duplicate tags
SL_data_tags <- anti_join(SL_data_tags, temp_duplicate_tags)

 
### Pull reference tags
#----------------------------#
SL_data_ref_det <- filter(SL_data_det_raw, 
                            grepl(do.call(paste, c(as.list(temp_ref_tags$ID), sep = "|")), #Turn vector into grepl list
                                  Transmitter))

### Format tag specs
#----------------------------#
SL_data_tag_specs <- SL_data_tag_specs %>%  #Remove NA columns
select(TAG_SERIAL_NUMBER = `Serial No.`, Transmitter = `VUE Tag ID`, Transmitter.Count = `# of ID's`, 
       Researcher, Tag.Type =`Tag Family`, Tag.Life =`Est tag life (days)`, 
       Min.Delay.sec =`Step 1 Min Delay (sec)`, Max.Delay.sec = `Step 1 Max Delay (sec)`,
       Sensor.Type =`Sensor type`, Range:Intercept)

#Find sensor tags
df_sensor_tags <- filter(SL_data_tag_specs, !is.na(Sensor.Type)) 
   
#Compile list of deployed Transmitters. 
#Sensor tags have 2 transmitter IDs per serial ID, with only 1 ID logged in SL_data_tags
temp_tag_serial_key <- rbind(SL_data_tags[c("TAG_SERIAL_NUMBER", "Transmitter")],
                             SL_data_tag_specs[c("TAG_SERIAL_NUMBER", "Transmitter")]) %>% 
                       distinct()

 
### Raw detection data
#----------------------------#
SL_data_det <- SL_data_det_raw %>% 
 anti_join(SL_data_ref_det, by=c("Transmitter")) %>% #Remove reference tags
 left_join(temp_tag_serial_key, by=c("Transmitter")) %>% #Add transmitter serial number
 #Add names to match GLATOS package format requirements
 mutate(detection_timestamp_utc = Timestamp,
        transmitter_codespace = substr(Transmitter, 1,8),
        transmitter_id = str_sub(Transmitter, 6),
        receiver_sn = Receiver,
        animal_id = TAG_SERIAL_NUMBER
        ) %>% 
 min_lag() %>% #Look at lag periods
 filter(min_lag>=30 & min_lag<=3600, #Rough filter removing code collisions, and first detection after 24h lag. WHY IS MIN LAG 0
        receiver_sn %in% SL_data_rec_locs$receiver_sn) #Make sure receive ids match

  
SL_data_full <- left_join(SL_data_det, SL_data_rec_locs, by="receiver_sn", relationship="many-to-many") %>% 
 filter(Timestamp >= deploy_date_time,
        Timestamp <= recover_date_time2,
        !is.na(deploy_lat),
        #REMOVE UNKNOWN TAGS
        !is.na(animal_id)
        ) %>% distinct()

#add sun position
SL_data_full <- left_join(SL_data_full, df_timestamp_SunPosition, multiple="first")

#add Sensor values
temp_sensor_dets <- filter(SL_data_full, !is.na(Sensor.Value)) #Grab formatted dets with sensor data
temp_sensor_dets <- left_join(temp_sensor_dets, df_sensor_tags) #Merge tag specs with formatted dets

df_sensor_dets <- temp_sensor_dets %>% 
 mutate(Sensor.Value.Cal = (as.numeric(Intercept) + (as.numeric(Sensor.Value)*as.numeric(Slope)))) %>% 
 select(Timestamp, station_no, Receiver, Transmitter, TAG_SERIAL_NUMBER, Sensor.Value.Cal, Units, Sensor.Type,  deploy_lat, deploy_long, )

SL_data_full <- left_join(SL_data_full, 
          select(df_sensor_dets, Timestamp, Transmitter, Sensor.Value.Cal, Units, Sensor.Type),
          by=c("Timestamp", "Transmitter"), multiple="first")

rm(SL_data_det); rm(df_timestamp_SunPosition); 
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name
gc() #Clear memory


#####Identify and remove dead fish################################----
#-------------------------------------------------------------#

###Identify dead fish to filter out
#Use data.table syntax for memory-intensive tasks
  temp_DeadFish <- SL_data_full %>%  as.data.table()
  temp_DeadFish <- setorder(temp_DeadFish, animal_id, Timestamp) #Order by fish then time
  temp_DeadFish[, unique_movement := rleid(animal_id, receiver_sn)] #unique_movement consecutive fish-receiver pairs

temp_DeadFish <- temp_DeadFish %>% 
 group_by(animal_id) %>%
 filter(unique_movement == last(unique_movement)) %>% #Only look at span of last receiver
 summarize(LastAlive = min(detection_timestamp_utc),
           unique_movement = max(unique_movement), 
           timespan = as.numeric(difftime(last(Timestamp), first(Timestamp), units = "days"))) 

#Histogram of residency at latest receiver. Parse normal residency vs possibly dead
ggplot(temp_DeadFish, aes(x=timespan))+
 geom_histogram(binwidth=1)+
 xlim(0,100)+
 ylim(0, 75)+
 geom_vline(xintercept = 15, colour="red")+
 xlab("Residency at latest reciever (Days)")+
 ylab("Number of fish")+
 ggtitle("Histogram of last reciever residency")

#Dead fish meet histogram threshold
temp_DeadFish2 <- temp_DeadFish %>% 
 filter(timespan > 14)

#Manually ID dead fish from plot below
#Dead fish rules:
# - Has not moved in last 15 days
# - Did not show residency with lifelike forays to other receivers before most recent receiver residency.
temp_list_deadfish <- c(
 "62997", "63176", "63247", "63259", "22999", "23014", "7093", "65023", "7293", "13704", "5193", #2023-06-22
"62342", "63237", "15496", "15592", "15594", "15598", "23004", "7084", "7097", "65021", #2023-07-11
"13705", "3560", "63181", "28067", "15590", "15603", "3578", "7087", "7098"    #2023-07-11
 )

#Plot fish that match histogram residency threshold. Label dead fish as red.
SL_data_full %>% 
 filter(animal_id %in% temp_DeadFish2$animal_id[01:20]) %>% 
 sample_n(100000) %>% 
 mutate(Dead = case_when(
         grepl(paste(temp_list_deadfish, collapse = "|"), animal_id) 
                ~ TRUE,
           TRUE ~ FALSE)) %>% 
 ggplot(aes(x=Timestamp, y=deploy_long, colour=Dead))+
   geom_point()+
   scale_colour_manual(values=c("Black", "Firebrick"))+
   facet_wrap(~animal_id, ncol=4, scales="free")+
  ggtitle("Apparently dead fish (C)")

#Collect all dead fish detections
temp_antijoin <- SL_data_full %>%
  group_by(animal_id) %>%
  filter((#Fish have been manually identified as dead
          grepl(paste(temp_list_deadfish, collapse = "|"), animal_id) & 
          #Detections occur after the last detected movememnt of the dead fish
          detection_timestamp_utc >= first(temp_DeadFish$LastAlive[temp_DeadFish$animal_id == first(animal_id)]))) 

SL_data_full <- anti_join(SL_data_full, temp_antijoin, by=c("animal_id", "detection_timestamp_utc")) %>% 
  ungroup()

#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name


#####dataframe of fish movements##################################----
#-------------------------------------------------------------#

SL_data_movements <- SL_data_full %>% 
 select(Timestamp, Transmitter, To_animal_id=animal_id, 
        To_station=station_no, To_lat=deploy_lat, To_lon=deploy_long) %>% 
 arrange(To_animal_id, Timestamp) %>% 
 mutate(From_station = lag(To_station), From_animal_id=lag(To_animal_id), 
        From_lat=lag(To_lat), From_lon=lag(To_lon)) %>% 
 filter(To_station!=From_station, To_animal_id==From_animal_id) %>% 
 left_join(select(SL_data_tags, TAG_SERIAL_NUMBER, COMMON_NAME_E, RELEASE_GROUP, RELEASE_LOCATION),
           by=c("To_animal_id"="TAG_SERIAL_NUMBER"))
 

#####Plot data####################################################----
#-------------------------------------------------------------#
###Prepare background maps
#----------------------------#
#Google basemap
# Retrieve Google API key from keyring (secure storage)
# To set key initially, run once: keyring::key_set("google_api", username = "Jake")
param_google_api_key <- keyring::key_get("google_api", username = "Jake")
ggmap::register_google(key = param_google_api_key)
SLmap <- get_googlemap(center = c(lon=mean(SL_data_rec_locs$deploy_long), lat=mean(SL_data_rec_locs$deploy_lat)),
                       zoom = 12, size = c(640, 640), scale = 4,
                       maptype = c("satellite"))

#Shapefile
SL_shapefile <- readOGR(dsn = file.path(                       
 "01 - Data/StoneyLake_polygon/stoney poly.shp"), stringsAsFactors = F)
SL_shapefile <- spTransform(SL_shapefile, CRS("+proj=longlat +datum=WGS84")) #Tranform projection


###Plot receivers
#----------------------------#
###map of rec
ggmap(SLmap, extent='normal')+
  scale_x_continuous(limits=c(min(SL_data_rec_locs$deploy_long)-0.01, max(SL_data_rec_locs$deploy_long)+0.01))+
  scale_y_continuous(limits=c(min(SL_data_rec_locs$deploy_lat)-0.01, max(SL_data_rec_locs$deploy_lat)+0.01))+
  ylab("Latitude") +
  xlab("deploy_longitude")+
  geom_point(data=SL_data_rec_locs, aes(x=deploy_long, y=deploy_lat),col="yellow")




