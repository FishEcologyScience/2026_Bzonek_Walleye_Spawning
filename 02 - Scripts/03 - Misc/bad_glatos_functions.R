
##### Broken Code ################################################----
#-------------------------------------------------------------#

### issues getting glatos detection_events function to work
#----------------------------#
#WARNING: THIS FUNCTION AVERAGES ACROSS LOCATIONS DOES NOT BIND LAT LONGS TO RECIEVERS.
data_events <- glatos::detection_events(det=data_det_raw, location_col = "transmitter_id", time_sep=3600)
data_events2 <- glatos::detection_events(det=data_det_raw, location_col = "transmitter_id")
data_events3 <- glatos::detection_events(det=data_det_raw)

 

### issues getting glatos REI function to work
#----------------------------#
df_spawn_aggregations <- glatos::REI(detections = mutate(data_spawn, 
                                                         detection_timestamp_utc = detection_timestamp_EST,
                                                         station_no = station,
                                                         common_name_e = "Walleye"),
                                     deployments = data_rec_locs)
  required_deployment_columns <-  c('station', 'deploy_date_time', 'recover_date_time')
  required_detection_columns <- c('station', 'common_name_e', 'animal_id', 'detection_timestamp_utc')

 
 test <- mutate(data_spawn, 
                detection_timestamp_utc = detection_timestamp_EST,
                station = station_no,
                common_name_e = "Walleye")
 test2 <- mutate(data_rec_locs,
                 recover_date_time = recover_date_time2)
 
unique(test$station); test$common_name_e; test$animal_id; test$detection_timestamp_utc
data_rec_locs$station; data_rec_locs$deploy_date_time; data_rec_locs$recover_date_time


glatos::REI(detections = test, deployments = test2)
