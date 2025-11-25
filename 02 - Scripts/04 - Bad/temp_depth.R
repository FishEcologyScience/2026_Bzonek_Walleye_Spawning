
#Make new events dataset
temp_data_daily_var <- df_movement %>% 
  filter(To_animal_id==From_animal_id) %>% 
  group_by(To_animal_id, From_station, moveID) %>% 
  mutate(detcount=n(),
            time_start = min(From_timestamp),
            time_end = max(detection_timestamp_utc),
            residence = difftime(time_end, time_start, units = c("hours"))) %>% 
# Calculate days per fish per year
  mutate(
    #detection_timestamp_utc = time_start,
    station_no = To_station,
    year = year(detection_timestamp_utc),
    date = as.Date(detection_timestamp_utc)
  ) %>%
  rename(animal_id = To_animal_id) %>% 
  group_by(animal_id, year) %>%
  mutate(
    days_per_fish_year = n_distinct(date)
  ) %>%
  ungroup() %>% 
  left_join(data_det, by=join_by(detection_timestamp_utc, animal_id, station_no)) %>% 
  group_by(moveID) %>% 
  mutate(meanDepth = mean(-sensor_value.Cal)) %>% 
 select(-c(mass.x, length_total.x, glatos_array, transmitter_codespace, tag_model, tag_serial_number, 
           sex, release_group, glatos_caught_date, DST, tag_type))

temp_data_daily_var %>% 
 group_by(animal_id, date) %>% 
 summarize(depth_mean = mean(meanDepth),
           depth_sd = sd(meanDepth),
           residence_mean = as.numeric(mean(residence)),
           residence_sd = as.numeric(sd(residence))
           )

temp_data_daily_var %>% 
# Filter for fish+year combinations with >15 days
  filter(days_per_fish_year > 15) %>%
  # Now continue with your plot
  sample_n(20000) %>%
  ggplot(aes(y=-sensor_value.Cal, x=detection_timestamp_utc, colour = animal_id))+
   geom_line(alpha = 0.6)+
   geom_hline(yintercept=-2)+
   facet_wrap(~year(detection_timestamp_utc), scales = "free_x")+
   theme(legend.position = "none")




