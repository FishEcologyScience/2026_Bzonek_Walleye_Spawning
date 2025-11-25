#Script1-4_NetworkAnalysis
# #Detection data
# temp_NW_data <- data_det_raw %>%
#  mutate(FishID = as.numeric(as.factor(animal_id)),
#         ReceiverID=location,
#         lat=deploy_lat,
#         long=deploy_long)

temp_NW_data <- data_events %>% 
 filter(animal_id == "A69-9006-14516") %>% 
 mutate(FishID = as.numeric(as.factor(animal_id)),
        ReceiverID=location,
        lat=mean_latitude,
        long=mean_longitude)

gc() #Clear memory

temp_NW_df <- telemetrytoolsFESL::network_summary(
                 data = temp_NW_data,
                 FishID=temp_NW_data$FishID,
                 ReceiverID=temp_NW_data$ReceiverID,
                 lat = temp_NW_data$lat,
                 long = temp_NW_data$long)

temp_NW_df$receiver.locations
temp_NW_df$individual.moves
temp_NW_df$plot.data


temp_NW_plot <- telemetrytoolsFESL:::network_plot(data = temp_NW_df,
                     Min.traffic=3, #specify if you want ignore any lines below traffic threshold
                     shapefile=HH_shapefile_WGS84,  #specify shapefile, and potential domain limits
                     #colour.gradient.low="grey", colour.gradient.high="purple", #pick your own colour scale
                     line.min=1, line.max=3, #pick your own line weights
                     #receiver.shape=22, receiver.size=6, receiver.stroke=4, #customize the look of your nodes
                     plot.title="Network plot", #speficy optional figure title (useful if looping across fish)
                     #labels=TRUE, label.size=1.8, label.transparency=0.5, label.nudge=0.00009, #add labels, and specify details
                     )
temp_NW_plot <- temp_NW_plot+
  scale_x_continuous(limits=c(min(temp_NW_df$plot.data$to.x)-0.1, min(temp_NW_df$plot.data$to.x)+0.2))+
  scale_y_continuous(limits=c(min(temp_NW_df$plot.data$to.y)-0.01, max(temp_NW_df$plot.data$to.y)+0.01))

temp_NW_plot
