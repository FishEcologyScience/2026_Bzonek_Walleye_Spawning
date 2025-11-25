## --------------------------------------------------------------#
## Script name: Script1-4_SecondaryAnalyses.R 
##
## Purpose of script: 
##    Start conducting more advanced analyses on fish movememnt
##     
##
## Author: Paul Bzonek
##
## Date Created: 2023-07-12
##
## --------------------------------------------------------------#  
## 
##   
## --------------------------------------------------------------#


#####function to categorize timestamps based on sunlight times####----
#-------------------------------------------------------------#
library(suncalc)
library(pbapply)

function_Timestamp_to_SunCategory <- function(i) {
 
 #Run prebuild function that grabs sunlight times (dawn, dusk, etc.) per day
  sunlight_times <- suncalc::getSunlightTimes(as.Date(i), lat = 43.29598, lon = -79.82892)
  
  # Use efficient syntax to assign sun category according to time
  SunCategory <- character(length(i))
  SunCategory[i >= sunlight_times$dawn & i <= sunlight_times$goldenHourEnd] <- "dawn"
  SunCategory[i > sunlight_times$goldenHourEnd & i < sunlight_times$goldenHour] <- "day"
  SunCategory[i >= sunlight_times$goldenHour & i <= sunlight_times$dusk] <- "dusk"
  SunCategory[i > sunlight_times$dusk | i < sunlight_times$dawn] <- "night"
  
  return(SunCategory)
}

###Apply the categorize_timestamp function to dataset
#----------------------------#
#NOTE: VERY SLOW AVOID RUNNING IF POSSIBLE
       # Using mapply -> pbmapply to display loading screen

df_SunCategory <- data.frame(
  Timestamp = data_det_raw$detection_timestamp_utc,
  SunCategory = pbapply::pbmapply(function_Timestamp_to_SunCategory, i=data_det_raw$detection_timestamp_utc)
)

sample(data_det_raw$detection_timestamp_utc, 10000)

#Times seem reasonable
df_SunCategory %>% group_by(as.Date(Timestamp), SunCategory) %>% 
 summarise(Start_UTM = min(Timestamp),
           Start_ET = with_tz(Start_UTM, tzone = "America/Toronto")) %>% head()

#Save as object to avoid lengthy processing times
saveRDS(df_SunCategory, "data_det_raw_SunPosition_2024-07-12.RDS")









