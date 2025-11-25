## --------------------------------------------------------------#
## Script name: Script2-8_analysis_t_repeatability
##
## Purpose of script: 
##    Assess repeatability of spawning behaviours with rptR package (telemetry workflow)
##    
## Dependencies: 
##    - Script2-2_process_t_dataset.R (df_spawn)
##    - rptR package
##
## Author: Paul Bzonek 
##
## Date Created: 2024-08-12
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   2025-01-11: Renamed from Script2-Y_analysis_t_repeatability.R
## --------------------------------------------------------------#



#####Extract Interclass Correlation Coefficients #################----
#-------------------------------------------------------------#
names(data_spawn)
temp_model_data_filtered <- na.omit(df_spawn)

temp <- temp_model_data_filtered %>% 
 group_by(animal_id, year) %>% 
 reframe(n=n())

###Spawning duration
#----------------------------#
rpt_spawn_duration <- rpt(as.numeric(residence) ~ (1|animal_id) + (1|station_no), 
                          grname = "animal_id", data = df_spawn, datatype = "Gaussian",
                          nboot = 100, npermut = 0)

# rpt_spawn_duration <- rpt(as.numeric(residence) ~  water_level + week + (1|animal_id) + (1|station_no) + (1|year), 
#                           grname = "animal_id", data = df_spawn, datatype = "Gaussian",
#                           nboot = 100, npermut = 0)

summary(rpt_spawn_duration)
plot(rpt_spawn_duration)



library('partR2') #Variance partitioning in mixed-effects models
mod_spawn_duration2 <- lmer(as.numeric(residence) ~ water_level + week + (1|animal_id) + (1|station_no) + (1|year), 
                          data = temp_model_data_filtered)

mod_spawn_duration <- lmer(as.numeric(residence) ~ water_level + week + (1|animal_id), 
                          data = temp_model_data_filtered)

mod_spawn_duration <- lmer(as.numeric(residence) ~ water_level + length_total + (1|animal_id), 
                          data = temp_model_data_filtered)

partR2::partR2(mod_spawn_duration, 
               R2_type = "marginal", nboot=20,
               partvars = c("water_level", "week"))

#Model is fked
summary(mod_spawn_duration2)
performance::icc(mod_spawn_duration2) 
performance::r2(mod_spawn_duration2)
performance::check_model(mod_spawn_duration2)
performance::model_performance(mod_spawn_duration2)
































