## --------------------------------------------------------------#
## Script name: Script2-7_analysis_t_network
##
## Purpose of script: 
##    Filter data to events then produce network plots (telemetry workflow)
##    Call network function from telemetrytoolsFESL package
##
## Dependencies: 
##    - Script2-2_process_t_dataset.R (event data)
##    - telemetrytoolsFESL package
##
## Author: Paul Bzonek 
##
## Date Created: 2025-01-10
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   2025-01-11: Renamed from Script2-Y_analysis_t_network_plot.R
## --------------------------------------------------------------#

#####Reduce dataset down to movements ############################----
#-------------------------------------------------------------#

### TEMPORARY: Subset data to a more manageable size
#----------------------------#
data_det_yr <- data_det_raw %>% 
 filter(detection_timestamp_utc > as.POSIXct("2021-01-01 01:00:00"),
        detection_timestamp_utc < as.POSIXct("2023-01-01 01:00:00"),
        #animal_id %in% c("A69-9004-15774", "A69-9006-12558", "A69-1105-79", "A69-1105-83")
        )


#Use Jakes fucntion to summarize movements down to events
#----------------------------#
temp_df <- mutate(data_det_yr, 
               FishID =animal_id,
               lat = deploy_lat,
               lon = deploy_long,
               datetime = detection_timestamp_utc)

df_events <- Events(temp_df)

#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name
   



#####Plot recievers and general detections ############################----
#-------------------------------------------------------------#
###map of rec
#----------------------------#
ggmap(HHmap, extent='normal')+
 #ggplot()+
   geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
   scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
   scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
   ylab("Latitude")+
   xlab("Longitude")+
   #Receivers
   geom_point(data=data_rec_locs_raw, aes(x=deploy_long, y=deploy_lat, colour=station_no),  shape=21, size=8)+
   geom_text(data=slice_head(data_rec_locs_raw, n=1, by=station_no),
              aes(x=deploy_long, y=deploy_lat, label = station_no), colour= "firebrick", size=3)+
   #data_events
   geom_point(data=sample_n(df_events, 1000), aes(x=mean_lonitude, y=mean_latitude), colour="burlywood")+
   labs(title = "Map of receiver locations",
                subtitle = paste("Rings represent receiver locations, colour by station number",
                                 "Dots represent detections",
                                 sep = "\n"))+
   theme(legend.position="none")


 

##### Network Analysis ###########################################----
#-------------------------------------------------------------#
temp_NW_data <- df_events %>% 
 filter(individual == "A69-9006-12558") %>% 
 mutate(FishID = as.numeric(as.factor(individual)),
        ReceiverID=location,
        lat=mean_latitude,
        long=mean_lonitude)

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

#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name
   


#####Network plot + spawing loop across individuals######################----
#-------------------------------------------------------------#
gc() #Clear memory

#Individual loop
loop_NW_data_plot <- data.frame()
loop_NW_plot <- list()
loop_df_spawn <- data.frame()
loop_list_bad_id <- c()


#for (i in unique(c("A69-9006-12558", "A69-1105-79", "A69-1105-83"))){
for (i in unique(df_events$individual)){

     cat("Starting loop for animal_id:", i, "\n") #Name finished animal_id

     #filter the data
     loop_temp_NW_data <- df_events %>% 
      filter(individual == i) %>% 
      mutate(FishID = as.numeric(as.factor(individual)),
             ReceiverID=location,
             lat=mean_latitude,
             long=mean_lonitude)
     
     #Data quality checks
     #----------------------------#
     ### conditional to test data set size
     if (nrow(loop_temp_NW_data) == 0){
      cat(" Failure: No data for this fish", "\n", "\n")
      loop_list_bad_id <- append(loop_list_bad_id, i)
     } else {
     
        tryCatch({
        #Main function logic
          #Run the Network Analysis
          #----------------------------#
          #run network summary
          loop_temp_NW_df <- telemetrytoolsFESL::network_summary(
                                                   data = loop_temp_NW_data,
                                                   FishID=loop_temp_NW_data$FishID,
                                                   ReceiverID=loop_temp_NW_data$ReceiverID,
                                                   lat = loop_temp_NW_data$lat,
                                                   long = loop_temp_NW_data$long
                                                   )
          
          
          #run network plot
          loop_temp_NW_plot <- telemetrytoolsFESL:::network_plot(data = loop_temp_NW_df,
                                                      Min.traffic=3, #specify if you want ignore any lines below traffic threshold
                                                      shapefile=HH_shapefile_WGS84,  #specify shapefile, and potential domain limits
                                                      #colour.gradient.low="grey", colour.gradient.high="purple", #pick your own colour scale
                                                      line.min=1, line.max=3, #pick your own line weights
                                                      #receiver.shape=22, receiver.size=6, receiver.stroke=4, #customize the look of your nodes
                                                      plot.title=paste("animal_id: ", i), #speficy optional figure title (useful if looping across fish)
                                                      #labels=TRUE, label.size=1.8, label.transparency=0.5, label.nudge=0.00009, #add labels, and specify details
                                                      )
          
          #Modify plot
          loop_temp_NW_plot_trimmed <- loop_temp_NW_plot+
                                 scale_x_continuous(limits=c( -79.93, -79.725))+
                                 scale_y_continuous(limits=c(43.25, 43.35))
          
          
          #Spawning data
          #----------------------------#
          loop_temp_spawn <- df_spawn %>% 
           filter(animal_id == i)
          
          
          #Summarize spawning events per receiver
          loop_temp_spawn <- loop_temp_spawn %>% 
           group_by(station_no) %>% 
           summarize(deploy_lat = mean(deploy_lat),
                     deploy_long = mean(deploy_long),
                     depth = mean(meanDepth),
                     residence_mean = mean(residence),
                     residence_sum = sum(residence),
                     residence_median = median(residence),
                     detcount_mean = mean(detcount),
                     detcount_sum = sum(detcount),
                     detcount_median = median(detcount),
                     countspawn = n(),
                     countfish = data.table::uniqueN(animal_id))
          
          #Plot spawning events
          loop_temp_spawn_plot <- ggmap(HHmap, extent='normal')+
            scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
            scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
            ylab("Latitude")+
            xlab("Longitude")+
            geom_point(data = loop_temp_spawn,
                       aes(x = deploy_long, y = deploy_lat, size = countspawn, fill=as.numeric(residence_median)),
                       shape=21)+
           scale_fill_viridis_c()+
           ggtitle(paste("spawning data for:", i))+
           theme_minimal()
          
          
          
          #Combine plots
          #----------------------------#
          loop_temp_NW_spawn_plot <- loop_temp_NW_plot_trimmed+
           geom_point(data = loop_temp_spawn, aes(x = deploy_long, y = deploy_lat, colour=countspawn), 
                      #size=5, shape=15, alpha=0.9)+
                      size=5, shape=22, stroke=2)+
           scale_colour_viridis_c(option="D")
           print(loop_temp_NW_spawn_plot)
           
          #Append Data
          #----------------------------#
          ### Data
          # loop_NW_data_plot
          loop_temp_A <- loop_temp_NW_df$plot.data
          loop_temp_A$animal_id <- i
          loop_NW_data_plot <- rbind(loop_NW_data_plot, loop_temp_A)
          
          # loop_df_spawn
          loop_temp_spawn$animal_id <- i
          loop_df_spawn <- rbind(loop_df_spawn, loop_temp_spawn)
          
          ### Plots
          # loop_temp_NW_plot
          loop_NW_plot[[i]]$NW <- loop_temp_NW_plot
          
          # loop_temp_spawn_plot
          loop_NW_plot[[i]]$spawn <- loop_temp_spawn_plot
          
          # loop_temp_NW_spawn_plot
          loop_NW_plot[[i]]$combined <- loop_temp_NW_spawn_plot
     
           
          #Clean-up
          #----------------------------#
          rm(list = paste(ls(pattern="loop_temp"))) #Remove environment objects with 'loop_temp' in name
          gc() #Clear memory
          cat(" Success: Loop completed for this fish", "\n", "\n")
        
        }, #End main trycatch logic
        
        error=function(e){cat(" Failure: The loop failed somewhere within the code base", "\n", "\n")
                          cat(e$message)
                         } #End error Logic
        ) #End tryCatch  
     } #End else loop
} #End loop

 

#####Visualize plots #############################################----
#-------------------------------------------------------------#



#loop_NW_plot[["A69-9006-12558"]][["combined"]]
#loop_NW_plot[["A69-9001-18965"]][["combined"]]

#Null
loop_NW_plot[["A69-1105-79"]][["combined"]]

#Lots of movement, lots of spawning, did not spawn at residence locations
loop_NW_plot[["A69-9006-12559"]][["combined"]]
loop_NW_plot[["A69-9006-14171"]][["combined"]]
loop_NW_plot[["A69-9006-16052"]][["combined"]]

#Spawned where they resided
loop_NW_plot[["A69-9006-12548"]][["combined"]]

#No spawning
loop_NW_plot[["A69-9001-18965"]][["combined"]]
loop_NW_plot[["A69-9001-18967"]][["combined"]]
loop_NW_plot[["A69-9002-4495"]][["combined"]]

#Data for year but not during spawning
loop_NW_plot[["A69-9006-12555"]][["combined"]]

#Identified by k-means clustering - lots of movement with lots of spawning
loop_NW_plot[["A69-9006-14169"]][["combined"]]

loop_NW_plot[[18]][["combined"]]




