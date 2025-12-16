## --------------------------------------------------------------#
## Script name: plot_buffered_transect
##
## Purpose of script:
##    Buffer walleye survey transect lines to segments that fall
##    within param_min_dist of habitat sites
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2025-12-09
##
## --------------------------------------------------------------#
## Modification Notes:
##   - Follows same approach as Script1-1 for matching spatial data
## --------------------------------------------------------------#


### Load walleye transect shapefile
#----------------------------#
shapefile_Walleye <- sf::read_sf("01 - Data/WalleyeSurveysLines/HH_WalleyeSpawningSurveyLines.shp")

# Project shapefile to WGS84
shapefile_Walleye <- sf::st_transform(shapefile_Walleye, "WGS84")


### Visualize raw data
#----------------------------#
ggplot() +
  geom_sf(data = shapefile, colour = "black", fill = NA, inherit.aes = FALSE) +
  geom_sf(data = shapefile_Walleye, colour = "firebrick", fill = NA,
          linewidth = 1.2, inherit.aes = FALSE) +  # Increased line thickness
  geom_point(data = data_hab, aes(x = Start_Longitude, y = Start_Latitude),
             colour = "burlywood", size = 4, shape=21, alpha=0.5, stroke=2) +
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  labs(title = "Walleye Transect Lines (red) and Habitat Sites (gold)") +
  theme_classic()


### Segment transect lines by proximity to habitat sites
#----------------------------#
cat("=== BUFFERING WALLEYE TRANSECT LINES ===\n")

# Step 1: Extract unique habitat site coordinates from data_hab
temp_hab_sites <- data_hab %>%
  select(PointID, lon = Start_Longitude, lat = Start_Latitude) %>%  # Keep only location columns
  unique() %>%  # Remove duplicate sites
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%  # Convert to spatial points (WGS84)
  sf::st_transform(crs = 32618)  # Reproject to UTM Zone 18N for accurate meter-based distances

cat("Habitat sites:", nrow(temp_hab_sites), "\n")



# Step 2: Create circular buffers around each habitat site
# Each buffer has radius = param_min_dist 
temp_hab_buffered <- temp_hab_sites %>%
  sf::st_buffer(dist = param_min_dist)  # Creates 25m radius circles around each habitat point



# Step 3: Prepare transect lines for spatial intersection
temp_transect_utm <- shapefile_Walleye %>%
  sf::st_transform(crs = 32618) %>%  # Reproject to UTM (same CRS as habitat buffers)
  sf::st_cast("LINESTRING")  # Convert MULTILINESTRING to individual LINESTRING features



# Step 4: Sample ALL transect lines into points first (before filtering)
# This ensures we capture all potential points, then filter by distance

# Sample points along each line (creates MULTIPOINT geometries)
temp_sampled <- temp_transect_utm %>%
  sf::st_cast("LINESTRING", warn = FALSE) %>%  # Ensure LINESTRING format
  sf::st_line_sample(density = 1) #1/5 # Sample one point every 5 meters along ALL lines

# Extract all individual points from MULTIPOINT geometries properly
# st_coordinates extracts X, Y coords from all points in MULTIPOINT
temp_coords <- sf::st_coordinates(temp_sampled)
cat("Total individual points extracted:", nrow(temp_coords), "\n")

# Convert coordinate matrix back to sf POINT object
temp_all_points <- temp_coords %>%
  as.data.frame() %>%
  sf::st_as_sf(coords = c("X", "Y"), crs = 32618) %>%  # Create POINT features from coordinates
  mutate(point_id = row_number())  # Add unique ID to each individual point

cat("Total points sampled from all transects:", nrow(temp_all_points), "\n")



# Step 5: Calculate distance and identify nearest habitat site for each point
temp_all_points_dist <- temp_all_points %>%
  mutate(
    # Calculate distance to nearest habitat site
    nearest_hab_dist = as.numeric(
      sf::st_distance(sf::st_geometry(.), sf::st_geometry(temp_hab_sites), by_element = FALSE) %>%  # Distance matrix to all habitat sites
        apply(1, min)  # For each point, find minimum distance to any habitat site
    ),
    # Identify which habitat site is nearest
    nearest_hab_id = temp_hab_sites$PointID[
      sf::st_nearest_feature(geometry, temp_hab_sites)
    ]
  )

# Keep only points within the distance threshold
temp_nearby_points <- temp_all_points_dist %>%
  filter(nearest_hab_dist <= param_min_dist)

cat("Points within", param_min_dist, "m of habitat:", nrow(temp_nearby_points),
    "(", round(100 * nrow(temp_nearby_points) / nrow(temp_all_points), 1), "% of total)\n")



# Step 6: Verify distances are correct (should all be ≤ param_min_dist)
cat("Distance verification - min:", round(min(temp_nearby_points$nearest_hab_dist, na.rm=T), 1),
    "m, max:", round(max(temp_nearby_points$nearest_hab_dist, na.rm=T), 1), "m\n")



# Step 7: Create segment IDs for consecutive points near the same habitat site
# This groups points into line segments based on spatial continuity and habitat association
temp_nearby_points <- temp_nearby_points %>%
  mutate(
    # Detect when habitat association changes between consecutive points
    habitat_change = nearest_hab_id != lag(nearest_hab_id, default = first(nearest_hab_id)),
    # Create segment ID that increments when habitat changes
    segment_id = cumsum(habitat_change)
  )

cat("Unique line segments identified:", n_distinct(temp_nearby_points$segment_id), "\n")

ggplot() +
  geom_sf(data = shapefile, colour = "black", fill = NA, inherit.aes = FALSE) +
  # Original full transect lines (gray, faded)
  geom_sf(data = shapefile_Walleye, colour = "brown", alpha = 0.3,
          linewidth = 1.2, inherit.aes = FALSE) +
  # Buffered segments within param_min_dist (red, prominent)
  geom_sf(data = temp_nearby_points, colour = "firebrick",
          size = 2.5, inherit.aes = FALSE) +
  # # Habitat sites (gold points)
  # geom_point(data = data_hab, aes(x = Start_Longitude, y = Start_Latitude),
  #            colour = "burlywood", size = 3) +
  labs(title = paste0("Walleye Transect Segments within ", param_min_dist,
                      "m of Habitat Sites"),
       subtitle = "Gray = full transects, Red = buffered segments, Gold = habitat sites") +
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_classic()



# Step 8: Convert point groups back to line segments
# Each segment represents a continuous portion of transect near a specific habitat site
# Note: temp_nearby_points is already in UTM (32618), so calculate length before transforming to WGS84
shapefile_Walleye_buffered_lines <- temp_nearby_points %>%
  group_by(segment_id, nearest_hab_id) %>%
  filter(n() >= 2) %>%  # Need at least 2 points to make a line
  summarise(
    n_points = n(),
    mean_dist_to_hab = mean(nearest_hab_dist),
    .groups = 'drop'
  ) %>%
  sf::st_cast("LINESTRING") %>%  # Convert grouped points to line segments
  mutate(length_m = as.numeric(sf::st_length(geometry))) %>%  # Calculate length in UTM 
  sf::st_transform(crs = 4326)  # Transform to WGS84 to match other data (length_m already calculated)

cat("Line segments created:", nrow(shapefile_Walleye_buffered_lines), "\n")
cat("Segments by habitat site:\n")
print(table(shapefile_Walleye_buffered_lines$nearest_hab_id))

# Summarize line segment lengths
cat("\n--- LINE SEGMENT LENGTHS ---\n")
cat("Total buffered transect length:", round(sum(shapefile_Walleye_buffered_lines$length_m), 1), "m\n")
cat("Mean segment length:", round(mean(shapefile_Walleye_buffered_lines$length_m), 1), "m\n")
cat("Median segment length:", round(median(shapefile_Walleye_buffered_lines$length_m), 1), "m\n")
cat("Range:", round(min(shapefile_Walleye_buffered_lines$length_m), 1), "-",
    round(max(shapefile_Walleye_buffered_lines$length_m), 1), "m\n")

# Summarize by habitat site
temp_length_by_habitat <- shapefile_Walleye_buffered_lines %>%
  sf::st_drop_geometry() %>%
  group_by(nearest_hab_id) %>%
  summarise(
    n_segments = n(),
    total_length_m = sum(length_m),
    mean_length_m = mean(length_m),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_length_m))

cat("\n--- TRANSECT LENGTH BY HABITAT SITE ---\n")
print(temp_length_by_habitat, n = Inf)



# Step 9: Also create point version for backward compatibility
shapefile_Walleye_buffered <- temp_nearby_points %>%
  select(-c(nearest_hab_dist, habitat_change)) %>%  # Remove temporary columns
  sf::st_transform(crs = 4326)


### Visualize buffered segments
#----------------------------#
temp_plot_buffered <- ggplot() +
  geom_sf(data = shapefile, colour = "black", fill = NA, inherit.aes = FALSE) +
  # Original full transect lines (brown, faded)
  geom_sf(data = shapefile_Walleye, colour = "brown", alpha = 0.3,
          linewidth = 1.2, inherit.aes = FALSE) +
  # Buffered LINE segments within param_min_dist (colored by habitat site)
  geom_sf(data = shapefile_Walleye_buffered_lines,
          #aes(colour = nearest_hab_id),
          colour="firebrick",
          linewidth = 2, inherit.aes = FALSE) +
  # # Habitat sites (gold points)
  # geom_point(data = data_hab, aes(x = Start_Longitude, y = Start_Latitude),
  #            colour = "burlywood", size = 3) +
  labs(title = paste0("Walleye Transect Segments within ", param_min_dist,
                      "m of Habitat Sites"),
       subtitle = "Brown = full transects, Colored lines = segments by nearest habitat site",
       colour = "Nearest\nHabitat ID") +
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_classic()

print(temp_plot_buffered)
plotly::ggplotly(temp_plot_buffered)

# Alternative plot showing points colored by habitat
temp_plot_points <- ggplot() +
  geom_sf(data = shapefile, colour = "black", fill = NA, inherit.aes = FALSE) +
  # Original full transect lines (brown, faded)
  geom_sf(data = shapefile_Walleye, colour = "brown", alpha = 0.3,
          linewidth = 1.2, inherit.aes = FALSE) +
  # Buffered POINTS colored by habitat site
  geom_sf(data = shapefile_Walleye_buffered,
          aes(colour = nearest_hab_id),
          size = 1.5, inherit.aes = FALSE) +
  labs(title = paste0("Walleye Transect Points within ", param_min_dist,
                      "m of Habitat Sites"),
       subtitle = "Brown = full transects, Colored points = sampled points by nearest habitat",
       colour = "Nearest\nHabitat ID") +
  #formatting
  scale_x_continuous(limits=c(min(data_hab$Start_Longitude)-0.01, max(data_hab$Start_Longitude)+0.01))+
  scale_y_continuous(limits=c(min(data_hab$Start_Latitude)-0.01, max(data_hab$Start_Latitude)+0.01))+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_classic()

print(temp_plot_points)
plotly::ggplotly(temp_plot_points)

### Summary statistics
#----------------------------#
cat("\n--- BUFFERING SUMMARY ---\n")
cat("Distance threshold:", param_min_dist, "m\n")
cat("Original transect lines:", nrow(shapefile_Walleye), "\n")
cat("Habitat sites:", nrow(temp_hab_sites), "\n")
cat("Total sample points from transects:", nrow(temp_all_points), "\n")
cat("Sample points within buffer:", nrow(temp_nearby_points), "\n")
cat("Proportion retained:", round(100 * nrow(temp_nearby_points) / nrow(temp_all_points), 1), "%\n")
cat("\nDistance verification (m):\n")
cat("  Mean distance to nearest habitat:", round(mean(temp_nearby_points$nearest_hab_dist, na.rm=T), 1), "\n")
cat("  Min distance:", round(min(temp_nearby_points$nearest_hab_dist, na.rm=T), 1), "\n")
cat("  Max distance:", round(max(temp_nearby_points$nearest_hab_dist, na.rm=T), 1), "\n")


### Combine shoreline length with annual electrofishing data
#----------------------------#
cat("\n=== COMBINING TRANSECT LENGTH WITH ELECTROFISHING DATA ===\n")

# Summarize electrofishing data by year and site
temp_efish_annual <- data_habfish_pseudo %>%
  group_by(PointID, Year) %>%
  summarise(
    Total.Count = sum(Total.Count, na.rm = TRUE),
    .groups = 'drop'
  )

# Combine with transect length data
df_transect_efish <- temp_length_by_habitat %>%
  rename(PointID = nearest_hab_id) %>%  # Match column name with electrofishing data
  left_join(temp_efish_annual, by = "PointID") %>%
  mutate(
    # Calculate catch per unit effort (CPUE) - fish per meter of transect
    cpue_per_m = Total.Count / total_length_m,
    # Also calculate per 100m for easier interpretation
    cpue_per_100m = (Total.Count / total_length_m) * 100
  )

cat("Combined dataset rows:", nrow(df_transect_efish), "\n")
cat("Sites with transect data:", n_distinct(df_transect_efish$PointID), "\n")
cat("Years in dataset:", paste(sort(unique(df_transect_efish$Year)), collapse = ", "), "\n")

# Summary of CPUE
cat("\n--- CPUE SUMMARY ---\n")
cat("Mean CPUE (fish per 100m):", round(mean(df_transect_efish$cpue_per_100m, na.rm = TRUE), 2), "\n")
cat("Median CPUE (fish per 100m):", round(median(df_transect_efish$cpue_per_100m, na.rm = TRUE), 2), "\n")
cat("Range:", round(min(df_transect_efish$cpue_per_100m, na.rm = TRUE), 2), "-",
    round(max(df_transect_efish$cpue_per_100m, na.rm = TRUE), 2), "\n")



### Cleanup
#----------------------------#
rm(list = ls(pattern = "^temp_"))
cat("\nWalleye transect buffering complete!\n")
