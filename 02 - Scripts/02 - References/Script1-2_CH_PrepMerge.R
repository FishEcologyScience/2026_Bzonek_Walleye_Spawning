## --------------------------------------------------------------#
## Script name: Script1-2_PrepareConsHaltMerge.R 
##
## Purpose of script: 
##    Format and analyse Conservation Halton habitat data  
##    
##
## Author: Paul Bzonek
##
## Date Created: 2022-07-06
##
## --------------------------------------------------------------#  
## Modification Notes: Code modified from Brownscombe's "CH ss data.R"  
##   
## --------------------------------------------------------------#

#----------------------------#
###Prepare datasets for merging
#----------------------------#
#Trim down datasets to make site index
df_BI <- data_BI %>% 
  select(dateBI = Date, SiteCode) %>% 
  ungroup() %>% unique() %>%
  rowid_to_column("RowNumBI")
df_BQ <- data_BQ %>%  
  mutate(lon.Benthic = as.numeric(lon),
         lat.Benthic = as.numeric(lat)) %>% 
  select(dateBQ, SiteCode, lon.Benthic, lat.Benthic) %>% 
  ungroup() %>% unique() %>%
  rowid_to_column("RowNumBQ")
df_Fish <- data_CH_Fish %>% 
  select(dateFish = Date, SiteCode = Station, lon.Fish = lon, lat.Fish = lat) %>% 
  ungroup() %>% unique() %>%
  rowid_to_column("RowNumFish")

# df_WQ <- data_WaterQuality %>% 
#   select(Date, location, lon.WQ=lon, lat.WQ=lat) %>%  
#   unique() %>% ungroup() %>% 
#   rowid_to_column("RowNumWQ")
# 
# df_WQ %>% select(location, lon.WQ, lat.WQ) %>% unique()

#----------------------------#
###Make Site Code
#----------------------------#
#Make dataset of SiteCodes and their location. After Fixing BQ Site Error, location drift is negligible over time
df_SiteCode <- full_join(select(df_BQ, SiteCode, lon = lon.Benthic, lat=lat.Benthic),
                         select(df_Fish, SiteCode, lon = lon.Fish, lat=lat.Fish)) %>% 
  unique() %>% group_by(SiteCode) %>% slice(1) %>% ungroup()


#----------------------------#
###Make index of best-matching sites and times
#----------------------------#
#Idenify temporal;matches between sites
df_MatchIndexFull <- 
  #Combine df_BI and df_BQ
  left_join(df_BI,
            select(df_BQ, -lon.Benthic, -lat.Benthic), by="SiteCode") %>% 
  mutate(TimeLagBI_BQ = abs(difftime(dateBI, dateBQ, units = "days"))) %>%
  #Trim down combinations
  group_by(RowNumBI) %>% 
  filter(TimeLagBI_BQ == min(TimeLagBI_BQ)) %>% 
  ungroup() %>% 
  #Combine df_BIQ and df_Fish
  left_join(select(df_Fish, -lon.Fish, -lat.Fish), by="SiteCode") %>% 
  mutate(TimeLagBIQ_Fish = abs(difftime(dateBI, dateFish, units = "days"))) %>% 
  #Trim down combinations
  group_by(RowNumFish) %>% 
  filter(TimeLagBIQ_Fish == min(TimeLagBIQ_Fish),
         TimeLagBIQ_Fish < 360) %>% 
  ungroup() %>% 
  #Identify SS sites
  left_join(select(data_CH_SS, SiteCode=Station, dateFish=Date, SSAbund=number))

#Trim Index by lag constant
df_MatchIndex <- df_MatchIndexFull %>% 
  select(-SSAbund) %>% 
  filter(TimeLagBIQ_Fish < (TimeLagMax+1))
 

#####Plot the Data################################################----
#-------------------------------------------------------------# 
##############################################
#Map the location of all site locations
a <- ggmap(SSmap)+
  #Fish
  geom_point(data=df_Fish %>% 
               filter(year(dateFish) > 2000) %>% 
               unique() %>% 
               select(SiteCode, lon.Fish, lat.Fish),
             aes(x=lon.Fish, y = lat.Fish, colour = "Fish Sampling Sites"),
             shape=21, stroke=2, size=3.5, alpha=0.5)+
  #Benthics
  geom_point(data=unique(select(ungroup(df_BQ), SiteCode, lon.Benthic, lat.Benthic)), 
             aes(x=lon.Benthic, y = lat.Benthic, colour = "Benthic Sampling Sites"),
             shape=21, stroke=2, size=2)+
  # #Water Quality
  # geom_point(data=df_WQ, 
  #            aes(x=lon.WQ, y = lat.WQ, colour = "Water Quality Stations"),
  #            size=3,  alpha=0.6)+
  #Formatting
  scale_color_manual(
    values = c(#"Water Quality Stations" = "blue",
      "Benthic Sampling Sites" = "orange",
      "Fish Sampling Sites" = "magenta"))+
  ggtitle("Many Fish samples for each benthic sample")+
  guides(colour=guide_legend(title="Site Type"))
#suppressMessages(print(a))

##############################################
#Compare full fish and merged datasets
#Map full Fish Dataset
a <- 
  ggmap(SSmapBW, darken = c(0.2, "white"))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data=filter(df_stations_mean, pres ==0),
             aes(x=lon, y=lat), colour="black", shape = 21, size=3, stroke=1.5)+
  geom_point(data=filter(df_stations_mean, pres ==1),
             aes(x=lon, y=lat, colour=mean_number), shape = 22, size=3.5, stroke=2, alpha=0.8)+
  scale_color_viridis_c()+
  theme(legend.position = "none")+
  ggtitle("Full Fish Dataset")+
  #Map combined Dataset
ggmap(SSmapBW, darken = c(0.2, "white"))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data=filter(df_stations_mean, Station %in% df_MatchIndex$SiteCode, pres ==0),
             aes(x=lon, y=lat), colour="black", shape = 21, size=3, stroke=1.5)+
  geom_point(data=filter(df_stations_mean, Station %in% df_MatchIndex$SiteCode, pres ==1),
             aes(x=lon, y=lat, colour=mean_number), shape = 22, size=3.5, stroke=2, alpha=0.8)+
  scale_color_viridis_c()+
  ggtitle("Combined Fish and Benthics Dataset")
suppressMessages(print(a))

##############################################
#Time correlation between WQ and Benthic
a <- ggplot(df_MatchIndex, aes(x=dateBI, y=dateFish))+
  geom_point()+
  ggtitle("Note: There are many Benthic surverys assigned to each water quality Survey")
#suppressMessages(print(a))

##############################################
#Historgram of Lime Lag
a <- 
ggplot()+
  #All sites
  geom_freqpoly(aes(x=df_MatchIndexFull$TimeLagBIQ_Fish, y=cumsum(..count..)), 
                size=1.5, binwidth = 5)+
  #SS Sites
  geom_freqpoly(data = filter(df_MatchIndexFull, SSAbund>0), 
                aes(x=TimeLagBIQ_Fish, y=cumsum(..count..)), 
                size=1.5, binwidth = 5, colour = "firebrick")+
  #Current cutoff
  geom_vline(xintercept = TimeLagMax, colour="blue", size=2, alpha=0.5)+
  #Labels
  xlab("Time lag (days) between dataset site visits")+
  ylab("Number of Records in combined dataset")+
  ggtitle(paste(paste("Relationship btwn timelag and combined dataset size"),
                paste("Specified Max Time Lag:", TimeLagMax, "Days"),
                paste("Dataset Size:", nrow(df_MatchIndex), "Records"), 
                sep="\n"))+
  theme_bw()
suppressMessages(print(a))

 
#####Clean up################################################----
#-------------------------------------------------------------# 
rm(df_BI); rm(df_BQ); rm(df_Fish)
rm(df_MatchIndexFull)
