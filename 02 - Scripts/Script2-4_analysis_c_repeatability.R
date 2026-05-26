## --------------------------------------------------------------#
## Script name: Script2-4_analysis_c_repeatability
##
## Purpose of script: 
##    Combine movement metrics and spawning behavior analysis (combined workflow)
##    Calculate repeatability of behavioral traits using mixed models
##    Generate behavioral clustering and individual consistency plots
##
## Dependencies: 
##    - Script1-2_format_data_t.R (data_det)
##    - Script2-2_process_t_dataset.R (df_spawn)
##    - Script1-1_format_data_h.R (data_waterlevel, data_fish)
##
## Author: Paul Bzonek 
##
## Date Created: 2024-12-03
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   2025-01-11: Renamed from Script2-Y_repeatability_telemetry.R
## --------------------------------------------------------------#

# Grab movement metrics
#####Minimum convex polygon values ###############################----
#-------------------------------------------------------------#

# Look at full data
#----------------------------#
temp_metrics <- data_det %>% 
 mutate(year = year(detection_timestamp_utc)) %>% 
 group_by(animal_id, year) %>% #Give up on week
 summarize(station_count = length(unique(station_no)),
           mcp95 = {tryCatch(#Add a tryCatch to keep code running during error
                                   {# Create a spatial points object for this grouping
                                    temp_group <- data.frame(deploy_long = deploy_long, deploy_lat = deploy_lat)
                                    coordinates(temp_group) <- c("deploy_long", "deploy_lat")
                                    proj4string(temp_group) <- CRS("+proj=longlat +datum=WGS84")
                                    
                                    # Calculate MCP for the group
                                    temp_mcp <- adehabitatHR::mcp(temp_group, percent = 95, unout = "m2")
                                    
                                    # Extract the MCP area (in square meters) as a numeric value
                                    as.numeric(temp_mcp$area)
                                  },
                                  error = function(e) {NA}
                         )}, #End tryCatch
             )

temp_metrics


# Look at spawning data
#----------------------------#
temp_metrics_spawn <- df_spawn %>% 
 group_by(animal_id, year) %>% 
 summarize(station_count_spawn = length(unique(station_no)),
           mcp95_spawn = {tryCatch(#Add a tryCatch to keep code running during error
                                   {# Create a spatial points object for this grouping
                                    temp_group <- data.frame(deploy_long = deploy_long, deploy_lat = deploy_lat)
                                    coordinates(temp_group) <- c("deploy_long", "deploy_lat")
                                    proj4string(temp_group) <- CRS("+proj=longlat +datum=WGS84")
                                    
                                    # Calculate MCP for the group
                                    temp_mcp <- adehabitatHR::mcp(temp_group, percent = 95, unout = "m2")
                                    
                                    # Extract the MCP area (in square meters) as a numeric value
                                    as.numeric(temp_mcp$area)
                                  },
                                  error = function(e) {NA}
                         )}, #End tryCatch
             )

temp_metrics_spawn

df_behaviour <- left_join(temp_metrics, temp_metrics_spawn, by=c("animal_id", "year"))

#Combine Spawning and non-spawning behaviour
#----------------------------#
df_behaviour <- df_behaviour %>% 
 mutate(station_count_spawn = case_when(is.na(station_count_spawn) ~ 0, #Add zeros
                                        TRUE ~ station_count_spawn),#Ratio of reciever detections
        station_count_ratio = station_count_spawn/station_count,
        station_count_ratio = case_when(is.na(station_count_ratio) ~ 0,
                                        TRUE ~ station_count_ratio),
        #Ration of homerange area
        mcp95_ratio = mcp95_spawn/mcp95,
        mcp95_ratio = case_when(is.na(mcp95_ratio) ~ 0,
                                is.infinite(mcp95_ratio) ~ 0,
                                TRUE ~ mcp95_ratio)
        )

#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name


#####Build-out df_behaviour dataset ##############################----
#-------------------------------------------------------------#
df_behaviour <- df_behaviour %>%
 left_join(data_waterlevel, by = join_by(year)) %>% 
 left_join(select(data_fish, animal_id, length_total), by = join_by(animal_id))

  #Summarize dataset detections
  temp_detcount <- data_det %>% 
   mutate(year = year(detection_timestamp_utc)) %>% 
   group_by(animal_id, year) %>% 
   reframe(detcount_all_sum = n())
  
  df_behaviour <- left_join(df_behaviour, temp_detcount, by = join_by(animal_id, year))
  
  #Summarize spawning detections
  temp_spawn <- df_spawn %>% 
   group_by(animal_id, year) %>% 
   summarize(#Weighted means
             depth_mean = weighted.mean(meanDepth, w=detcount),
             #Unweighted 
             residence_mean = mean(residence),
             residence_sum = sum(residence),
             detcount_mean = mean(detcount),
             detcount_sum = sum(detcount)
             )
  df_behaviour <- left_join(df_behaviour, temp_spawn, by = join_by(animal_id, year))

df_behaviour <- df_behaviour %>% 
 mutate(#Overwrite non-spawning NAs with 0s
        across(residence_mean:detcount_sum, ~ ifelse(is.na(.), 0, .)),
        #Log whether fish spawned or not
        SpawnBinary = case_when(is.na(detcount_sum)~FALSE,
                                detcount_sum == 0 ~ FALSE,
                                TRUE ~ TRUE),
        length_total = as.numeric(length_total)) %>%
 #filter(length_total > 400) %>% Removing as we now have the age threshold
 ungroup()



#  ## REVISION - PIPELINE ENRICHMENT (predicted_FL) ──────────────────────────
### Von Bertalanffy growth parameters (fork length, mm)
#----------------------------#
# Values from Brooks et al. 2025

# Lake Ontario model: "FL = 601.41 - e^(-0.31*(age+1.006))"
# FL = L_inf * (1 - exp(-K * (age - t0)))
# where t0 = -1.006 so (age - t0) = (age + 1.006)
# temp_vb_linf <- 601.41   # asymptotic fork length (mm)
# temp_vb_k    <- 0.31     # growth coefficient
# temp_vb_t0   <- -1.006   # theoretical age at length zero (years)

# Hamilton Harbour Model: "FL = 642.5 - e^(-0.375*(age+0.5))"
# FL = L_inf * (1 - exp(-K * (age - t0)))
temp_vb_linf <- 642.5   # asymptotic fork length (mm)
temp_vb_k    <- 0.375   # growth coefficient
temp_vb_t0   <- -0.5    # theoretical age at length zero (years)

df_behaviour <- df_behaviour %>%
  left_join(
    select(data_fish, animal_id, tag_year, age_at_tag, length_fork),
    by = "animal_id"
  ) %>%
  mutate(
    year_numeric    = as.integer(as.character(year)),  # df_behaviour$year is a factor; direct as.integer() returns level indices, not values
    years_since_tag = year_numeric - tag_year,
    age_pred        = age_at_tag + years_since_tag,
    predicted_FL    = temp_vb_linf * (1 - exp(-temp_vb_k * (age_pred - temp_vb_t0))),
    predicted_FL    = round(pmax(predicted_FL, length_fork), 1)  # prevents fish above L∞ from appearing to shrink toward the asymptote
  ) %>%
  filter(age_pred > param_age_maturity)  # retain mature fish-years only (males mature at age 2)

#  ## END REVISION ──────────────────────────────────────────────────────────────
  
#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name


#Scale the data
#----------------------------#
df_behaviour_scaled <- df_behaviour %>%
  mutate(across(c(station_count:detcount_sum, age_pred, predicted_FL), ~ scale(.) %>% as.numeric())
         )

#####Plot data ###################################################----
#-------------------------------------------------------------#
#Spawning by Size
plots$behaviour$spawn_by_size <- df_behaviour %>% 
 mutate(predicted_FL = round(predicted_FL, digits=-1)) %>% 
 ggplot(aes(x=predicted_FL, fill=SpawnBinary, color=SpawnBinary)) +
   geom_histogram(position="identity", alpha=0.5)
plots$behaviour$spawn_by_size 


plots$behaviour$station_count <- df_behaviour %>%
  group_by(animal_id) %>%
  summarise(mean_station_count = mean(station_count, na.rm = TRUE)) %>%
  arrange(mean_station_count) %>%
  mutate(rank = row_number()) %>%
  right_join(df_behaviour, by = "animal_id") %>%   # **Add rank back to the original dataframe**
  # Use the numeric rank for the x-axis instead of the animal_id
  ggplot(aes(x = as.factor(rank), y = station_count)) +
    geom_boxplot(outlier.shape = NA, colour = "darkgrey") +
    geom_jitter() +
    ylab("station count") +
    xlab("individuals (ranked by mean score per metric)") +
    labs(title = "Count of receivers with spawning behaviour")

plots$behaviour$station_proportion <- df_behaviour %>%
  group_by(animal_id) %>%
  summarise(mean_station_count_ratio = mean(station_count_ratio, na.rm = TRUE)) %>%
  arrange(mean_station_count_ratio) %>%
  mutate(rank = row_number()) %>%
  right_join(df_behaviour, by = "animal_id") %>%   # **Add rank back to the original dataframe**
  # Use the numeric rank for the x-axis instead of the animal_id
  ggplot(aes(x = as.factor(rank), y = station_count_ratio)) +
    geom_boxplot(outlier.shape = NA, colour = "darkgrey") +
    geom_jitter() +
    ylab("proportion of stations") +
    xlab("individuals (ranked by mean score per metric)") +
    labs(title = "Proportion of receivers visited with spawning behaviour")

plots$behaviour$residence_duration <- df_behaviour %>%
  group_by(animal_id) %>%
  summarise(mean_residence_mean = mean(residence_mean, na.rm = TRUE)) %>%
  arrange(mean_residence_mean) %>%
  mutate(rank = row_number()) %>%
  right_join(df_behaviour, by = "animal_id") %>%   # **Add rank back to the original dataframe**
  filter(SpawnBinary == TRUE) %>%
  # Use the numeric rank for the x-axis instead of the animal_id
  ggplot(aes(x = as.factor(rank), y = residence_mean)) +
    geom_boxplot(outlier.shape = NA, colour = "darkgrey") +
    geom_jitter() +
    #scale_y_log10()+
    ylab("cumulative hours")+ xlab("individuals (ranked by mean score per metric)")+
    labs(title="Mean spawning duration (h)")

plots$behaviour$spawning_depth <- df_behaviour %>%
  group_by(animal_id) %>%
  summarise(mean_depth_mean = mean(depth_mean, na.rm = TRUE)) %>%
  arrange(mean_depth_mean) %>%
  mutate(rank = row_number()) %>%
  right_join(df_behaviour, by = "animal_id") %>%   # **Add rank back to the original dataframe**
  # Use the numeric rank for the x-axis instead of the animal_id
  ggplot(aes(x = as.factor(rank), y = depth_mean)) +
    geom_boxplot(outlier.shape = NA, colour = "darkgrey") +
    geom_jitter() +
    ylab("mean depth") +
    xlab("individuals (ranked by mean score per metric)") +
    labs(title="Mean spawning depth (m)")

#Combine the repeatability plots
plots$behaviour$CombinedRepeatability <-
                      (plots$behaviour$station_count /
                       plots$behaviour$station_proportion /
                       plots$behaviour$residence_duration /
                       plots$behaviour$spawning_depth)

plots$behaviour$CombinedRepeatability

# ### Save outputs
# #----------------------------#
# # Save the repeatability plot
# ggplot2::ggsave("04 - Outputs/2025_09_03_GroupByYear/plot_repeatability.png", plot_repeatability, 
#                 width = 10, height = 12, dpi = 300)
# 
# # Save the behaviour dataframe
# saveRDS(df_behaviour, paste0("04 - Outputs/2025_09_03_GroupByYear/df_behaviour_", Sys.Date(), ".rds"))
# cat("Saved plot_repeatability.png and df_behaviour to 04 - Outputs/\n")
# 
#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name




##### Analysze repeatability #####################################----
#-------------------------------------------------------------#

rpt_behaviour_station <- rptR::rpt(station_count_spawn ~ water_level + predicted_FL + detcount_all_sum + (1|animal_id) + (1|year),
                             #station_count_spawn ~ predicted_FL + (1|animal_id),
                          grname = "animal_id", data = df_behaviour_scaled, datatype = "Gaussian",
                          nboot = 100, npermut = 0)
summary(rpt_behaviour_station)
plot(rpt_behaviour_station)


rpt_behaviour_station_ratio <- rptR::rpt(station_count_ratio ~ water_level + predicted_FL + detcount_all_sum + (1|animal_id) + (1|year),
                          grname = "animal_id", data = df_behaviour_scaled, datatype = "Gaussian",
                          nboot = 100, npermut = 0)
summary(rpt_behaviour_station_ratio)
plot(rpt_behaviour_station_ratio)


rpt_behaviour_residence <- rptR::rpt(residence_mean ~ predicted_FL + detcount_all_sum + (1|animal_id) + (1|year),
                          grname = "animal_id", 
                          data = filter(df_behaviour_scaled, SpawnBinary == TRUE), 
                          datatype = "Gaussian",
                          nboot = 100, npermut = 0)
summary(rpt_behaviour_residence)
plot(rpt_behaviour_residence)

rpt_behaviour_depth <- rptR::rpt(depth_mean ~ water_level + predicted_FL + detcount_all_sum + (1|animal_id) + (1|year),
                          grname = "animal_id", data = df_behaviour_scaled, datatype = "Gaussian",
                          nboot = 100, npermut = 0)
summary(rpt_behaviour_depth)
plot(rpt_behaviour_depth)

  #  ## REVISION - REVIEWER COMMENT 32 ──────────────────────────
  temp_size_metrics <- c("station_count", "station_count_ratio",
                          "residence_mean", "depth_mean")

  temp_summary_size_cor <- purrr::map_dfr(temp_size_metrics, function(metric) {
    temp_test <- cor.test(df_behaviour$predicted_FL, df_behaviour[[metric]],
                           method = "spearman", exact = FALSE)
    data.frame(
      metric  = metric,
      rho     = round(temp_test$estimate, 2),
      p_value = round(temp_test$p.value, 4),
      n       = sum(!is.na(df_behaviour$predicted_FL) & !is.na(df_behaviour[[metric]]))
    )
  })

  cat("--- Spearman Correlations: Total Length vs. Behaviour Metrics ---\n")
  print(temp_summary_size_cor)

  rm(list = ls(pattern = "^temp_summary_size"))
  rm(temp_size_metrics)
  cat("Cleanup complete.\n")
  #  ## END REVISION ──────────────────────────────────────────────


 
 



