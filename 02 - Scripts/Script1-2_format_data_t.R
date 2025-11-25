## --------------------------------------------------------------#
## Script name: Script1-2_format_data_t
##
## Purpose of script:
##    Import and format raw telemetry data
##    Column formatting, type conversions, and basic joins only
##    NO filtering or analysis (moved to Script1-3)
##
##
## Author: Paul Bzonek
##
## Date Created: 2025-01-07
##
## --------------------------------------------------------------#
## Modification Notes:
##   2025-10-03: Split script - moved all filtering/analysis to Script1-3
## --------------------------------------------------------------#



### Required packages (using :: notation for non-tidyverse)
#----------------------------#
library(tidyverse)
library(data.table)
# readxl, sf, ggmap, ggExtra will use :: notation



### Import raw data
#----------------------------#
#Load the big telem file
cat("Loading detections data (",
    round(file.size("01 - Data/HH_detections_clipped_filtered_2015-2023_good.rds")/1024/1024, 1),
    "MB) - this will take a few minutes...\n")
#data_det_raw <- readRDS("01 - Data/HH_detections_clipped_filtered_2015-2023_good.rds")
data_det_raw <- readRDS("01 - Data/HH_detections_Walleye_optimized_indexed.rds")
cat("✓ Loaded", format(nrow(data_det_raw), big.mark = ","), "detection records\n")
beepr::beep(sound=2) #Completeness chime.

#Load the rest
data_rec_locs_raw <- read.csv("01 - Data/GLATOS_receiverLocations_20240122_234632.csv")
data_fish <- readxl::read_excel("01 - Data/HH_Fish_Workbook_Jun2024.xlsx")
df_key <- readxl::read_excel("01 - Data/receiver_transect_key2.xlsx")
df_SunCategory <- readRDS("01 - Data/data_det_raw_SunPosition_2024-07-12.rds")
#data_waterlevel <- read.csv("01 - Data/WaterLevel_02HB017.csv") #From https://wateroffice.ec.gc.ca/report/historical_e.html?stn=02HB017&dataType=Monthly&parameterType=Level&first_year=2016&last_year=2022&mode=Table&page=historical&year=2022&start_year=1850&end_year=2025

 

### Format raw detection data
#----------------------------#
setDT(data_det_raw)  # Convert in-place to data.table for speed

# Calculate calibrated sensor values using data.table's := operator (in-place, faster)
data_det_raw[, sensor_value.Cal := as.numeric(Intercept) + (as.numeric(sensor_value) * as.numeric(Slope))]

cat("Raw detections selected:", nrow(data_det_raw), "\n")



### Format receiver location data
#----------------------------#
# Hamilton Array only
data_rec_locs_raw <- data_rec_locs_raw %>%
  filter(glatos_array == "HAM")

# Format receiver datatable
data_rec_locs_raw <- data_rec_locs_raw %>%
  rowwise() %>%
  mutate(ins_serial_no = as.numeric(ins_serial_no),
         ins_model_no = as.factor(ins_model_no),
         station_no = as.factor(station_no),
         receiver_sn = paste(ins_model_no, ins_serial_no, sep = "-"),
         # Fix date formats
         deploy_date_time = as.POSIXct(deploy_date_time, format = "%Y-%m-%d %H:%M:%OS"),
         recover_date_time = as.POSIXct(recover_date_time, format = "%Y-%m-%d %H:%M:%OS"),
         # Overwrite Inf values
         recover_date_time2 = if_else(is.infinite(recover_date_time),
                                     true = as.POSIXct(Sys.time(), format = "%Y-%m-%d %H:%M:%OS"),
                                     false = as.POSIXct(recover_date_time))) %>%
  # Keep columns of interest
  select(receiver_sn, ins_serial_no, station_no, deploy_lat, deploy_long, ins_model_no, bottom_depth,
         deploy_date_time, recover_date_time, recover_date_time2, comments) %>%
  filter(!is.na(ins_serial_no)) %>%
  ungroup()

# Clean-up receiver dataframe with improved date handling
data_rec_locs_raw <- data_rec_locs_raw %>%
  mutate(deployDate = strftime(deploy_date_time, tz = "EST", format = "%Y-%m-%d"),
         deployDate = as.POSIXct(deployDate, tz = "EST", format = "%Y-%m-%d"),
         recoverDate = strftime(recover_date_time, tz = "EST", format = "%Y-%m-%d"),
         recoverDate = as.POSIXct(recoverDate, tz = "EST", format = "%Y-%m-%d")) %>%
  group_by(ins_serial_no) %>%
  mutate(receiver_deployment = row_number()) %>%
  group_by(station_no) %>%
  mutate(station_rec_count = n_distinct(ins_serial_no)) %>%
  select(station_no, receiver_sn, deploy_lat, deploy_long,
         bottom_depth, ins_model_no, ins_serial_no,
         deploy_date_time, recover_date_time, recover_date_time2,
         deployDate, recoverDate, receiver_deployment, station_rec_count, comments) %>%
  ungroup()



### Format fish data
#----------------------------#
data_fish <- data_fish %>%
  filter(COMMON_NAME_E == "Walleye", !is.na(`Pinger Full ID`)) %>%
  select(animal_id = `Pinger Full ID`,
         length_standard = `Standard (mm)`,
         length_fork = `Fork (mm)`,
         length_total = `Total (mm)`,
         mass = `Mass (g)`) %>%
  mutate(length_standard = as.numeric(length_standard),
         length_fork = as.numeric(length_fork),
         length_total = as.numeric(length_total))



### Format receiver-habitat key
#----------------------------#
df_key <- df_key %>%
  mutate(station_no = as.factor(station),
         transect = as.factor(transect))



### Add supplementary data (sun position)
#----------------------------#
setDT(df_SunCategory)

cat("\nScript1-2 complete: Raw data imported and formatted\n")
