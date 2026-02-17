### Sun Category Times for April (Hamilton Harbour)
  #----------------------------#
library(suncalc)
  # Generate April dates for the study years
  temp_april_dates <- expand.grid(
    year = c(2015:2023),
    day = 1:30
  ) %>%
    mutate(date = as.Date(paste(year, "04", day, sep = "-")))

  # Calculate sun times for each date
  temp_sun_times <- suncalc::getSunlightTimes(
    date = temp_april_dates$date,
    lat = 43.29598,
    lon = -79.82892,
    tz = "America/Toronto"
  )

  # Summarize the time ranges
  temp_sun_summary <- temp_sun_times %>%
    mutate(
      dawn_time = format(dawn, "%H:%M"),
      goldenHourEnd_time = format(goldenHourEnd, "%H:%M"),
      goldenHour_time = format(goldenHour, "%H:%M"),
      dusk_time = format(dusk, "%H:%M")
    ) %>%
    summarise(
      dawn_start = min(dawn_time),
      dawn_end = max(dawn_time),
      goldenHourEnd_min = min(goldenHourEnd_time),
      goldenHourEnd_max = max(goldenHourEnd_time),
      goldenHour_min = min(goldenHour_time),
      goldenHour_max = max(goldenHour_time),
      dusk_start = min(dusk_time),
      dusk_end = max(dusk_time)
    )

  # Create formatted summary table
  temp_category_table <- tibble(
    Category = c("dawn", "day", "dusk", "night"),
    Start_Time = c(
      paste0(temp_sun_summary$dawn_start, " - ", temp_sun_summary$dawn_end),
      paste0(temp_sun_summary$goldenHourEnd_min, " - ", temp_sun_summary$goldenHourEnd_max),
      paste0(temp_sun_summary$goldenHour_min, " - ", temp_sun_summary$goldenHour_max),
      paste0(temp_sun_summary$dusk_start, " - ", temp_sun_summary$dusk_end)
    ),
    End_Time = c(
      paste0(temp_sun_summary$goldenHourEnd_min, " - ", temp_sun_summary$goldenHourEnd_max),
      paste0(temp_sun_summary$goldenHour_min, " - ", temp_sun_summary$goldenHour_max),
      paste0(temp_sun_summary$dusk_start, " - ", temp_sun_summary$dusk_end),
      paste0(temp_sun_summary$dawn_start, " - ", temp_sun_summary$dawn_end)
    ),
    Definition = c(
      "dawn → goldenHourEnd",
      "goldenHourEnd → goldenHour",
      "goldenHour → dusk",
      "dusk → dawn (next day)"
    )
  )

  temp_category_table

  # Cleanup
  rm(temp_april_dates, temp_sun_times, temp_sun_summary, temp_category_table)