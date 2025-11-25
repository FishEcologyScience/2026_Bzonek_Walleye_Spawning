### Read and format data from S. Larocque 
#----------------------------#
HH_data_daily_sp <- readRDS("01 - Data/All_spp_fish_rcvr_dataset_June14_2024update_5fishdays.rds")
HH_data_daily_sp <- HH_data_daily_sp %>% 
                     rename(season=thermo_season, hard=hard2, species=common_name_e) %>% 
                     mutate(season = case_when(season=="Spring"~"spring",
                                               season=="Summer"~"summer",
                                               season=="Fall"~"fall",
                                               season=="Winter"~"winter"),
                            season=as.factor(season))

str(HH_data_daily_sp)

#Pick a species
HH_data_daily_sp<- filter(HH_data_daily_sp, species =="Walleye") %>% 
                   mutate(presence2 = as.numeric(presence)-1,
                          year2 = as.numeric(year),
                          season2 = as.numeric(season),
                          )
data_daily_April <- filter(HH_data_daily_sp, format(date, "%m") == "04")

###Daily dataset
#Date ranges
paste(paste("min", min(data_daily_April$date)),
      paste("median", median(data_daily_April$date)),
      paste("max", max(data_daily_April$date)),
      collapse ="\n")

#Simultaneous fish per year
data_daily_April %>% 
 group_by(year) %>%
 summarize(maxfish = max(n_fish))