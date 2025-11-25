## --------------------------------------------------------------#
## Script name: Script1-1_TH_Explore Data.R 
##
## Purpose of script: 
##    Clean and fix Jake's source code from in-prep manuscript 
##    
##
## Author: Paul Bzonek and Jake Brownscombe
##
## Date Created: 2022-12-07
##
## --------------------------------------------------------------#  
## Modification Notes: Code modified from Brownscombe's "initial processing.R"  
##   
## --------------------------------------------------------------#


#load("workspaces/initial_processing_events.RData")
library(tidyverse) #dplyr, ggplot, etc
library(data.table) #memory efficient dataframes
library(ggmap) #google basemaps
library(geosphere)
library(patchwork) #add plots together


#####Pull and format raw data####################################----
#-------------------------------------------------------------#
###Import data
#----------------------------#
TH_data_det_raw <- readRDS(file="01 - Data/01 - Toronto Harbour/THFHA_detectionsWithLocs_20200526_203038.rds")
TH_data_rec_locs <- readRDS(file="01 - Data/01 - Toronto Harbour/GLATOS_receiverLocations_20200526_172814.rds")
TH_data_rec_envA <- readRDS(file="01 - Data/01 - Toronto Harbour/TH_Environmental_Clusters_Nov2017")
TH_data_rec_envB <- readRDS(file="01 - Data/01 - Toronto Harbour/TH_Receiver_Station_Links_28July2020.rds")
TH_data_tags <- readRDS(file="01 - Data/01 - Toronto Harbour/tags.rds") #This does not appear to be the correct tags file
#pre-processed files
#TH_data_nodes_daily_sp <- readRDS(file="01 - Data/01 - Toronto Harbour/dayspnode3.rds")
TH_data_nodes <- readRDS(file="01 - Data/01 - Toronto Harbour/nodes.rds")

###Fix formatting
#----------------------------#
#Summarize rec detections
TH_df_rec_sum <- TH_data_det_raw %>% group_by(station) %>% summarise(lat=mean(deploy_lat), lon=mean(deploy_long),dets=length(animal_id)) %>% as.data.frame()

#Combine both rec_environment files
TH_data_rec_env <-  merge(TH_data_rec_envB, TH_data_rec_envA, by=c("Station.Group"), all.x=TRUE)
TH_data_rec_env <- merge(TH_data_rec_env, TH_df_rec_sum, by="station", all.x=TRUE) #Add rec summary
rm(TH_data_rec_envA); rm(TH_data_rec_envB)

#Format TH_data_rec_locs 
TH_data_rec_locs$deploy_date_time <- as.POSIXct(TH_data_rec_locs$deploy_date_time, tz="UTC", format="%Y-%m-%d %H:%M:%S")
TH_data_rec_locs$recover_date_time <- as.POSIXct(TH_data_rec_locs$recover_date_time, tz="UTC", format="%Y-%m-%d %H:%M:%S")
TH_data_rec_locs$deployEST <- strftime(TH_data_rec_locs$deploy_date_time, tz="EST", format="%Y-%m-%d")
TH_data_rec_locs$recoverEST <- strftime(TH_data_rec_locs$recover_date_time, tz="EST", format="%Y-%m-%d")
TH_data_rec_locs$deployEST <- as.POSIXct(TH_data_rec_locs$deployEST, tz="EST", format="%Y-%m-%d")
TH_data_rec_locs$recoverEST <- as.POSIXct(TH_data_rec_locs$recoverEST, tz="EST", format="%Y-%m-%d")
#Find stations in TO:
TH_data_rec_locs$TOstation <- TH_data_rec_env$station[match(TH_data_rec_locs$station, TH_data_rec_env$station)]
TH_data_rec_locs <- TH_data_rec_locs %>% filter(!is.na(TOstation), !is.na(recoverEST))


###Filter detection data (Rough)
#----------------------------#
#Remove echos and erronious detections
TH_data_det <- TH_data_det_raw %>% 
  filter(min_lag>=30 & min_lag<=3600) 
#Add and filter by node data
TH_data_det$node <- TH_data_rec_env$Station.Group[match(TH_data_det$station, TH_data_rec_env$station)]
TH_data_det <- TH_data_det %>% filter(!is.na(node)) %>% droplevels()  
rm(TH_data_det_raw)
gc() #Clear memory

###OPTIONAL SUBSAMPLE
TH_data_det_full <- TH_data_det
TH_data_det <- sample_n(TH_data_det, size=100000)


#bring down to the day/node level, then assign habitat etc
#----------------------------#
TH_data_det$UTC <- as.POSIXct(TH_data_det$detection_timestamp_utc, tz="UTC", format="%Y-%m-%d %H:%M:%S")
TH_data_det$EST <- strftime(TH_data_det$UTC, tz="EST", format="%Y-%m-%d %H:%M:%S")
TH_data_det$EST <- as.POSIXct(TH_data_det$EST, tz="EST", format="%Y-%m-%d %H:%M:%S")
TH_data_det$day <- strftime(TH_data_det$EST, tz="EST", format="%Y-%m-%d")
TH_data_det$day <- as.POSIXct(TH_data_det$day, tz="EST", format="%Y-%m-%d")

str(TH_data_det)

###Make dataframes
#summarize node info
TH_df_node <- TH_data_rec_env %>% filter(!is.na(Mean.Exposure..m.)) %>% group_by(Station.Group) %>% 
  summarise(lat=mean(lat), lon=mean(lon), dets=mean(dets), exposure=mean(EXPOSURE), depth=mean(NEW.DEPTH)) %>% as.data.frame()
TH_df_node$SAV <- TH_data_rec_env$SAV[match(TH_df_node$Station.Group, TH_data_rec_env$Station.Group)]

TH_df_node_spp <- TH_data_det %>% 
  group_by(node, common_name_e) %>% 
  summarise(lat=mean(deploy_lat), 
            lon=mean(deploy_long), 
            IDcount=length(unique(animal_id)),
            dets=length(EST)) %>% as.data.frame()

###Plot receivers
#----------------------------#
#Set map data
# Retrieve Google API key from keyring (secure storage)
# To set key initially, run once: keyring::key_set("google_api", username = "Jake")
param_google_api_key <- keyring::key_get("google_api", username = "Jake")
ggmap::register_google(key = param_google_api_key)
TOmap <- get_googlemap(center = c(lon=mean(TH_data_rec_env$lon), lat=mean(TH_data_rec_env$lat)),
                       zoom = 12, size = c(640, 640), scale = 4,
                       maptype = c("satellite"))

###map of rec
ggmap(TOmap, extent='normal')+
  scale_x_continuous(limits=c(min(TH_data_rec_env$lon)-0.01, max(TH_data_rec_env$lon)+0.01))+
  scale_y_continuous(limits=c(min(TH_data_rec_env$lat)-0.01, max(TH_data_rec_env$lat)+0.01))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data=TH_data_rec_env, aes(x=lon, y=lat),col="yellow")


###map of nodes
ggmap(TOmap, extent='normal')+
  scale_x_continuous(limits=c(min(TH_data_rec_env$lon)-0.01, max(TH_data_rec_env$lon)+0.01))+
  scale_y_continuous(limits=c(min(TH_data_rec_env$lat)-0.01, max(TH_data_rec_env$lat)+0.01))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data=TH_df_node, aes(x=lon, y=lat, size=dets, col=SAV))+
  scale_color_continuous(low="yellow", high="red")

 


#####Mutate raw data##############################################----
#-------------------------------------------------------------#
###Detections to daily summaries
#----------------------------#
TH_data_daily <- data.table(TH_data_det)
TH_data_daily <- TH_data_daily[, .(dets=length(EST), IDcount=length(unique(animal_id))),  keyby=c("common_name_e", "day", "node")]
head(TH_data_daily)

###abacus:
ggplot(TH_data_daily, aes(x=day, y=common_name_e, col=IDcount))+
  geom_point()



###Summarize movement info
#----------------------------#
TH_data_det$datetime <- TH_data_det$EST
TH_data_det$FishID <- TH_data_det$animal_id
TH_data_det$lat <- TH_data_det$deploy_lat
TH_data_det$lon <- TH_data_det$deploy_long
TH_data_det$Transmitter <- as.factor(TH_data_det$transmitter_id)
head(TH_data_det)
str(TH_data_det)

#CUSTOM FUNCTION TO SUMMARIZE MOVEMENT INFO
TH_data_det2 <- MoveInfo(TH_data_det)

#CUSTOM FUNCTION TO ADD EVENTS DETAILS
Events(TH_data_det2, station="station", period=3600, condense=FALSE)
TH_data_det2 <- EventData
rm(EventData); rm(TH_data_det)

 
#Example of species-specific plots
#----------------------------#
# temp_CC <- TH_data_det2 %>% filter(common_name_e=="Common Carp") 
# ggplot(temp_CC, aes(day, deploy_lat))+
#   geom_path()+
#   geom_point(col="red", size=0.5)+
#   facet_wrap(~animal_id)+theme_bw()
# 
# Events(temp_CC, station="station", period=3600, condense=FALSE)
# temp_CC <- EventData
# 
# rm(temp_CC); rm(EventData)



#####Filter Dead Fish##############################################----
#-------------------------------------------------------------#
TH_data_det2$diedY <- "tracking"
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Bowfin 612" & TH_data_det2$day>"2014-07-01", "shed?", TH_data_det2$diedY)
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Carp 587" & TH_data_det2$day>"2013-06-19", "shed?", TH_data_det2$diedY)
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Bass 169" & TH_data_det2$day>"2014-01-01", "shed?", TH_data_det2$diedY)
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Bass 393" & TH_data_det2$day>"2013-11-22", "shed?", TH_data_det2$diedY)
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Perch 219" & TH_data_det2$day>"2013-05-22", "shed?", TH_data_det2$diedY)
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Perch 252" & TH_data_det2$day>"2012-10-01", "shed?", TH_data_det2$diedY)
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Pike 476" & TH_data_det2$day>"2011-09-01", "shed?", TH_data_det2$diedY)
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Pike 610", "shed?", TH_data_det2$diedY)
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Sucker 341", "shed?", TH_data_det2$diedY)
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Sucker 400" & TH_data_det2$day>"2017-11-13", "shed?", TH_data_det2$diedY)
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Sucker 403" & TH_data_det2$day>"2013-09-14", "shed?", TH_data_det2$diedY)
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Sucker 520" & TH_data_det2$day>"2018-05-07", "shed?", TH_data_det2$diedY)
TH_data_det2$diedY <- ifelse(TH_data_det2$animal_id=="Sucker 526" & TH_data_det2$day>"2013-07-01", "shed?", TH_data_det2$diedY)
#filter out 'shed' tags
TH_data_det2 <- TH_data_det2 %>% filter(diedY!="shed?")
TH_data_det2 %>% group_by(tag_model) %>% summarise()
gc() #Clear unused memory



#####Summary table of tracking metrics############################----
#-------------------------------------------------------------#
###Timeline
#make a time variable for whole study period:
TH_df_timeline <- data.frame(day=seq(from=min(TH_data_rec_locs$deployEST), to=max(TH_data_rec_locs$recoverEST), by="day"))
#combine times with station info:
TH_df_timeline <- merge(TH_df_timeline, TH_data_rec_locs, all=TRUE) 
#reduce down to just hours between deployment and recovery:
TH_df_timeline <- TH_df_timeline %>% filter(day>=deployEST & day<=recoverEST)
TH_df_timeline$node <- TH_data_rec_env$Station.Group[match(TH_df_timeline$station, TH_data_rec_env$station)]
TH_df_timeline <- TH_df_timeline %>% filter(node!="")
#ggplot(TH_df_timeline, aes(x=day, y=station))+geom_point()

###Track fish duration
temp_trackdur <- TH_data_det2 %>% group_by(animal_id) %>% summarise(first=min(EST),last=max(EST))

#Identify days tracked by fish
temp_daybyfish <- merge(unique(TH_df_timeline$day), temp_trackdur, all=TRUE) %>% rename(day=x)
temp_daybyfish <- temp_daybyfish %>% filter(day>=first & day<=last) %>% select(animal_id, day)
temp_daybyfish$species <- TH_data_det2$common_name_e[match(temp_daybyfish$animal_id, TH_data_det2$animal_id)]
temp_daybyfishcount <- temp_daybyfish %>% group_by(day, species) %>% summarise(count=length(unique(animal_id)))

ggplot(temp_daybyfishcount, aes(day, count))+geom_point()+xlab("Date")+ylab("Number of fish actively tracking")+facet_wrap(~species)+
  theme_bw()

#Identify days tracked by species
TH_df_daybyspecies <- merge(unique(TH_data_det2$day), unique(TH_data_det2$common_name_e)) %>% rename(day=x, species=y)
TH_df_daybyspecies <- merge(TH_df_daybyspecies, temp_daybyfishcount, by=c("day", "species"), all.x=TRUE)
TH_df_daybyspecies$count[is.na(TH_df_daybyspecies$count)]<-0
TH_df_daybyspecies <- TH_df_daybyspecies %>% filter(count>4)

#Make summary table
TH_df_summary_table <- TH_data_det2 %>% group_by(common_name_e) %>% 
  summarise(detections=length(EST), IDcount=length(unique(animal_id))) %>% 
  rename(species=common_name_e) %>% as.data.frame()
temp_track.range <- TH_df_daybyspecies %>% group_by(species) %>% summarise(start=min(day), end=max(day), range=max(day)-min(day)) %>% as.data.frame()
TH_df_summary_table <- merge(TH_df_summary_table, temp_track.range, by="species")

TH_df_summary_table

#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name








