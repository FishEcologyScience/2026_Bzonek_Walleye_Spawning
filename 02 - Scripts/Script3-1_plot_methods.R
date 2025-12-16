## --------------------------------------------------------------#
## Script name: Script3-1_plot_methods
##
## Purpose of script: 
##    Generate method plots and visualizations for manuscript
##    Combine telemetry and electrofishing spatial data
##    
## Dependencies: 
##    - Script1-1_format_data_h.R (shapefile, data_efish_obs, data_hab)
##    - Script2-2_process_t_dataset.R (df_spawn_plot)
##
## Author: Paul Bzonek 
##
## Date Created: 2024-09-11
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   2025-01-11: Renamed from Script4-1_methods_plot.R
## --------------------------------------------------------------#

###Generate combined results plot
#----------------------------#
 plots$maps$RawData <- ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #receivers
  geom_point(data = df_spawn_plot,
             aes(x = deploy_long, y = deploy_lat, size = countfish, fill=as.numeric(residence_median)),
             shape=21)+
  #efishing
  geom_point(data = data_efish_obs,
             aes(x=Long, y=Lat, size=Total.Count), colour="firebrick", alpha=0.5)+
  #hab data
  geom_point(data = data_hab,
             aes(x=Start_Longitude, y=Start_Latitude), colour="burlywood")+
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_viridis_c()+
  theme_classic()+
  theme(legend.position = "NA")
  
 plots$maps$RawData

###Total Count with Habitat
#----------------------------#
plots$maps$TotalCount <- ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #hab data
  geom_point(data = data_hab,
  aes(x=Start_Longitude, y=Start_Latitude), colour="burlywood", size=3)+
  #efishing
  geom_point(data = data_habfish_pseudo,
  aes(x=lon, y=lat, size=Total.Count), colour="firebrick", shape=21, stroke=1.15)+
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_classic()

plots$maps$TotalCount


###Methods Plot
#----------------------------#
# Summarize electrofishing data by year and site
temp_efish_summary <- data_habfish_pseudo %>%
  group_by(PointID) %>%
  summarise(Total.Count = sum(Total.Count, na.rm = TRUE),
            .groups = 'drop') %>% 
 left_join(.,
           select(df_hab, PointID, lat, lon))
temp_efish_summary

 plots$maps$LocationsMap <- ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #receivers
  geom_point(data = df_spawn_plot,
             aes(x = deploy_long, y = deploy_lat, fill = countfish, size=as.numeric(residence_median)),
             shape=21, stroke=1.25)+
  #efishing summary
  geom_point(data = temp_efish_summary,
             aes(x=lon, y=lat, colour = Total.Count), alpha=0.5, size=5, shape=18)+
  # #efishing
  # geom_point(data = data_habfish_pseudo,
  #            aes(x=lon, y=lat, colour = Total.Count), alpha=0.4, size=5, shape=18)+
  # #hab data
  # geom_point(data = data_hab,
  #            aes(x=Start_Longitude, y=Start_Latitude), colour="black", size=1)+
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
    scale_colour_viridis_c(begin=0.2,
                         name="Electrofishing Count",
                         guide = guide_colorbar(order = 1,
                                               #title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 8,
                                               barheight = 0.5))+
  scale_fill_viridis_c(#option="A",
                       begin=0.2,
                       name="Fish per Receiver",
                       guide = guide_colorbar(order = 2,
                                             #title.position = "top",
                                             title.hjust = 0.5,
                                             barwidth = 8,
                                             barheight = 0.5))+
  scale_size_continuous(range = c(2, 7.5),
                        breaks = scales::breaks_pretty(n = 3),
                        name="Median Residence (h)",
                        guide = guide_legend(order = 3,
                                            #title.position = "top",
                                            title.hjust = 0.5,
                                            nrow = 1))+
  # Add north arrow and scale bar
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                     pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                                     style = ggspatial::north_arrow_fancy_orienteering
                                    )+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.3)+
  theme_classic()+
  theme(legend.position = "top",
        legend.box = "vertical",
        legend.margin = margin(t = 0, b = 5),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 7),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 9))

  plots$maps$LocationsMap


# ###Look at efishing data
# #----------------------------#
# temp <- read.csv("01 - Data/HH_WalleyeSpawningSurvey_2022-2024_SummarizedObservations.csv")
# temp <- read.csv("01 - Data/HH_WalleyeSpawningSurvey_2022-2024_SummarizedFishInfo2.csv")





#####Plot efishing data ##########################################----
#-------------------------------------------------------------#

 
 ###Label habitat sites
#----------------------------#

plots$maps$HabSites <- ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #efishing
  geom_point(data = data_habfish_pseudo, 
             aes(x=lon, y=lat, size=Total.Count, colour = Total.Count), alpha=0.5, shape=18)+
  #hab data
  geom_label(data = df_hab,
             aes(x=lon, y=lat, label = PointID), colour="black")+
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_viridis_c(option="A", begin=0.2)+
  #scale_size_continuous(range = c(2, 7.5))+ 
  theme(legend.position = "top")+
  theme_classic()

plots$maps$HabSites
  

### Light Index Map
#----------------------------#
plots$maps$LightIndex <- ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #hab data with Light values
  geom_point(data = df_hab,
             aes(x=lon, y=lat, fill = Light),
             size=4, shape=21, colour="black", alpha=0.6)+
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_viridis_c(name = "Light Pollution Index")+
  theme_classic()+
  theme(legend.position = "right")

plots$maps$LightIndex

 

###Grab summary stats
#----------------------------#
with(data_habfish_raw, table(Total.Count, Year))
data_habfish_raw %>% 
 group_by(Year) %>% 
 summarize(TotalSum = sum(Total.Count),
           ObsSum = sum(Number.Captured),
           MissedSum = sum(Number.Missed),
           MassMean = mean(Weight, na.rm=TRUE),
           MassSD = sd(Weight, na.rm=TRUE),
           LengthMean = mean(Fork.Length, na.rm=TRUE),
           LengthD = sd(Fork.Length, na.rm=TRUE))


lm(Weight~Year, data = data_habfish_raw) %>% summary()
lm(Fork.Length~Year, data = data_habfish_raw) %>% summary()




###Plot efishing data
#----------------------------#
###Fish count
tempA <- data_habfish_raw %>%
 ggplot(aes(x = Total.Count, y = factor(Year), fill = factor(Year))) +  # Horizontal ridges by Year
 geom_density_ridges() +  # Adjust overlap with scale | scale = 1.5
 labs(y = "Year", x = "Total Count") +
 ggtitle("Total Walleye Observations")+
 scale_fill_manual(values = c("#f4a261", "#e76f51", "#9d4e3f"))+
 theme_bw()

###Fish weight
tempB <- data_habfish_raw %>%
 ggplot(aes(x = Weight, y = factor(Year), fill = factor(Year))) +  # Horizontal ridges by Year
 geom_density_ridges() +  # Adjust overlap with scale
 labs(y = "Year", x = "Mass (g)") +
 ggtitle("Walleye weight distribution")+
 scale_fill_manual(values = c("#f4a261", "#e76f51", "#9d4e3f"))+
 theme_bw()

###Fish length
tempC <- data_habfish_raw %>%
 ggplot(aes(x = Fork.Length, y = factor(Year), fill = factor(Year))) +  # Horizontal ridges by Year
 geom_density_ridges() +  # Adjust overlap with scale
 labs(y = "Year", x = "Fork Length (cm)") +
 ggtitle("Walleye fork length distribution")+
 scale_fill_manual(values = c("#f4a261", "#e76f51", "#9d4e3f"))+
 theme_bw()

#Combine plots
plots$methods$Walleye <- (tempA/tempB/tempC)+plot_layout(guides = "collect")
plots$methods$Walleye 

#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name


### Presence by Year
plots$maps$Walleye <- ggplot()+
  geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
  #efishing
  geom_point(data = data_habfish_pseudo, 
             aes(x=lon, y=lat, size=Total.Count, colour = Total.Count), alpha=0.7, shape=18)+
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  facet_wrap(~Year, nrow=3)+
  scale_colour_viridis_c(begin=0.2)+
  scale_size_continuous(range = c(2, 7.5))+ 
  theme(legend.position = "top")+
  theme_classic()


plots$maps$Walleye







