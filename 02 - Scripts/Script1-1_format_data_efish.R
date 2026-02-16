## --------------------------------------------------------------#
## Script name: Script1-0_format_survey_data
##
## Purpose of script:
##    Read and combine walleye spawning survey data from 2022-2024,
##    then identify which sites were matched in data_habfish_raw
##
## Dependencies:
##    - Script0-1_load_packages.R (packages)
##    - Script1-1_format_data_h.R (data_habfish_raw)
##    - Raw survey files in 01 - Data/WalleyeSurveyTable/
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2026-02-06
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#


###Import survey data
#----------------------------#
#Read summarized efishing data
data_efish_obs <- read.csv("01 - Data/HH_WalleyeSpawningSurvey_2022-2024_SummarizedObservations.csv")
data_efish_fish <- read.csv("01 - Data/HH_WalleyeSpawningSurvey_2022-2024_SummarizedFishInfo2.csv")


# Read individual survey files
temp_survey_2022 <- read.csv("01 - Data/WalleyeSurveyTable/WE Spawn Survey Data - 2022 - Sample Info.csv")
temp_survey_2023 <- read.csv("01 - Data/WalleyeSurveyTable/WE Spawn Survey Data 2023 - Sample Info.csv")
temp_survey_2024 <- read.csv("01 - Data/WalleyeSurveyTable/WE Spawning Surveys 2024 - Sample Info.csv")


#####Format efishing data for paper analysis #####################----
#-------------------------------------------------------------#
#Set dates
data_efish_obs <- data_efish_obs %>% mutate(Date = as.Date(YMD, format = "%m/%d/%Y"))
data_efish_fish <- data_efish_fish %>% mutate(Date = as.Date(YMD, format = "%m/%d/%Y"))
str(data_efish_fish)


###Plot spawning events
#----------------------------#
plots$methods$efish_full <- ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #efishing
  geom_point(data = data_efish_obs,
             aes(x=Long, y=Lat, size=Total.Count), colour="firebrick", alpha=0.5)+
  #hab data
  geom_point(data = data_hab,
             aes(x=Start_Longitude, y=Start_Latitude), colour="burlywood", size=3, shape=21, stroke=1.5)+
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  #ggtitle("All walleye (red) and habitat data (gold)")+
  scale_fill_viridis_c()+
  theme_classic()

plots$methods$efish_full 


 
#####Format raw efishing data for supplementary figure #####################----
#-------------------------------------------------------------#

###Handle year-specific differences before combining
#----------------------------#
# 2022: Add missing Start.Time, duplicate Water.Temp to Low/High
temp_survey_2022 <- temp_survey_2022 %>%
  mutate(Start.Time = NA_character_,
         Water.Temp.Low = Water.Temp,
         Water.Temp.High = Water.Temp) %>% 
 select(Date, Sample=SAM.., Start.Lat, Start.Long, End.Lat, End.Long,
         Effort, Start.Time, End.Time, Power,
         Wave.Height, Wind.Direction, Water.Temp.Low, Water.Temp.High)

# 2023: Combine Low.Power and High.Power into Power
temp_survey_2023 <- temp_survey_2023 %>%
  mutate(Power = (Low.Power + High.Power)/2) %>% 
 select(Date, Sample=SAM.., Start.Lat, Start.Long, End.Lat, End.Long,
         Effort, Start.Time, End.Time, Power,
         Wave.Height, Wind.Direction, Water.Temp.Low, Water.Temp.High)

# 2024: Combine Low.Power and High.Power into Power
temp_survey_2024 <- temp_survey_2024 %>%
 select(Date, Sample=SAM.., Start.Lat, Start.Long, End.Lat, End.Long,
         Effort, Start.Time, End.Time, Power,
         Wave.Height, Wind.Direction, Water.Temp.Low, Water.Temp.High)

###Combine and standardize survey data
#----------------------------#
data_survey <- bind_rows(temp_survey_2022, temp_survey_2023, temp_survey_2024) %>%
  # Remove empty rows
  #filter(!is.na(SAM..) & SAM.. != "") %>%
  # Rename columns
  rename(Start_Lat = Start.Lat,
         Start_Long = Start.Long,
         End_Lat = End.Lat,
         End_Long = End.Long,
         Start_Time = Start.Time,
         End_Time = End.Time,
         Wave_Height = Wave.Height,
         Wind_Direction = Wind.Direction,
         Water_Temp_Low = Water.Temp.Low,
         Water_Temp_High = Water.Temp.High) %>%
  # Select columns and convert Date
  # select(Date, Sample, Start_Lat, Start_Long, End_Lat, End_Long,
  #        Effort, Start_Time, End_Time, Power,
  #        Wave_Height, Wind_Direction, Water_Temp_Low, Water_Temp_High) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))


###Match with data_habfish_raw
#----------------------------#
# Get unique Sample-Date combinations from data_habfish_raw
temp_habfish_samples <- data_habfish_raw %>%
  select(Sample, Date) %>%
  unique() %>%
  mutate(Matched_habfish = TRUE)

# Join to identify matched sites
data_survey <- data_survey %>%
  left_join(temp_habfish_samples, by = c("Sample", "Date")) %>%
  mutate(Matched_habfish = ifelse(is.na(Matched_habfish), FALSE, TRUE))

#writexl::write_xlsx(data_survey, path="04 - Outputs/efish_survery_summary.xlsx")

###Summary of matching
#----------------------------#
cat("\n--- SURVEY DATA SUMMARY ---\n")
cat("Total survey samples:", nrow(data_survey), "\n")
cat("Matched to data_habfish_raw:", sum(data_survey$Matched_habfish), "\n")
cat("Unmatched samples:", sum(!data_survey$Matched_habfish), "\n")

cat("\n--- MATCHING BY YEAR ---\n")
data_survey %>%
  mutate(Year = lubridate::year(Date)) %>%
  group_by(Year) %>%
  summarize(
    Total = n(),
    Matched = sum(Matched_habfish),
    Unmatched = sum(!Matched_habfish),
    Match_Rate = round(Matched / Total * 100, 1)
  ) %>%
  print()

cat("\n--- UNMATCHED SAMPLES ---\n")
data_survey %>%
  filter(!Matched_habfish) %>%
  select(Date, Sample) %>%
  print()


###Cleanup
#----------------------------#
rm(list = ls(pattern = "^temp_"))
cat("\nCleanup complete.\n")
