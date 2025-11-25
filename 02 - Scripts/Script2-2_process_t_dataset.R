## --------------------------------------------------------------#
## Script name: Script2-2_process_t_dataset
##
## Purpose of script: 
##    Process telemetry data to produce a modelling dataset (telemetry workflow)
##    to be used for downstream analysis
##    
## Dependencies: 
##    - Script1-2_format_data_t.R (data_det, data_fish, data_rec_locs)
##
## Author: Paul Bzonek 
##
## Date Created: 2024-12-30
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   2025-01-11: Renamed from Script2-X-1_process_t_m_dataset.R
## --------------------------------------------------------------#


#' Using spawning events as the dependent variable gives us a dataset of 1s
#'  To model absences, we will sample a non-event detection back into the dataset for each spawn
#'
#' To consider:
#'  - match absences to detections per fish
#'    - absences are currently pulled randomly from global pool
#'    
#'    

###Make datatable for analysis
#----------------------------#
m_data <- df_spawn %>% 
 left_join(df_key) %>% 
 left_join(df_hab, by=c("transect"="PointID")) %>% 
 select(-c(lat, lon)) %>% 
 mutate(spawn=1, timestamp = time_start)

   ###Get matching absence data
   #Format like df_spawn
   data_not_spawn <- anti_join(data_det, data_spawn) %>% 
    select(detection_timestamp_EST, animal_id, station_no, deploy_lat, deploy_long, length_total, mass) %>%
    #format like m_data
    left_join(df_key) %>% 
    left_join(df_hab, by=c("transect"="PointID")) %>% 
    select(-c(lat, lon)) %>% 
    mutate(spawn = 0, station=as.numeric(station), timestamp = detection_timestamp_EST)
   
   #Subsample to appropriate size
   data_not_spawn2 <- sample_n(data_not_spawn, nrow(m_data))
   
   #Match non-spawning dataset size to spawning dataset per animal_id
   #----------------------------#
   # Step 1: Count the number of rows per animal_id in m_data
   temp_m_data_count <- m_data %>%
     group_by(animal_id) %>%
     summarise(n = n()) # n is the number of rows per animal_id
   
   # Step 2: For each animal_id in data_not_spawn, sample the number of rows based on temp_m_data_count
   data_not_spawn3 <- data_not_spawn %>%
     group_by(animal_id) %>%
     group_map(~ { # For each group (animal_id), apply the following function
                  n_rows <- temp_m_data_count %>% filter(animal_id == .y$animal_id) %>% # Filter temp_m_data_count to the current animal_id
                            pull(n) # Extract the row count for the current animal_id
                  if(length(n_rows) > 0 && n_rows > 0) { # If the animal_id exists in dataA and has a positive count
                     loop_sampled_data <- sample_n(.x, size = n_rows)  # Sample n_rows rows from the current group in dataB
                     loop_sampled_data$animal_id <- .y$animal_id  # Ensure animal_id is preserved in the sampled data
                     loop_sampled_data  # Return the sampled data with the animal_id column
                  } else {.x[0,]} # In case there's no match or 0 rows in m_data, return an empty dataframe
                }) %>%
     bind_rows() %>% # Combine all the sampled groups back together into one data frame
     ungroup()
   rm(temp_m_data_count)
   #----------------------------#

#Combine datasets 
#m_data <- bind_rows(m_data, data_not_spawn2)
m_data <- bind_rows(m_data, data_not_spawn3) %>% 
           ungroup() %>% 
           droplevels()

#Format data
m_data <- m_data %>% 
 mutate(year = format(timestamp, "%Y"),
        spawn_score = mean(spawn),
        n=n()) %>% filter(n > 10)# %>% filter(n > 50)

m_data <- m_data %>% 
 select(spawn, deploy_lat, deploy_long, animal_id, year, timestamp,
        DepthCategory, DepthIndex, Light, SubstrateDiversity, 
        Fine, Silt, Sand, Gravel, Cobble, Rubble, 
        Boulder, Armourstone, Concrete, Terrestrial) %>% 
 na.omit() %>% 
 mutate(across(DepthIndex:Terrestrial, scale, .names = "scaled_{col}"),
        animal_id = as.factor(animal_id),
        year = as.factor(year))

###Split up data
m_data$nrow <- 1:length(m_data$timestamp)
head(m_data)
m_data.train <- m_data %>% sample_frac(0.7) # 70/30 split. 
m_data.test <- m_data[!(m_data$nrow %in% m_data.train$nrow),]
m_data.tune <- m_data.test %>% sample_frac(0.5) 
m_data.test <- m_data.test[!(m_data.test$nrow %in% m_data.tune$nrow),]

#Look at spawning probability per fish
m_data.train %>% 
 group_by(animal_id) %>% 
 summarize(spawn_score = mean(spawn),
         n=n()) %>% 
 arrange(spawn_score) %>% 
 print(n=45)
