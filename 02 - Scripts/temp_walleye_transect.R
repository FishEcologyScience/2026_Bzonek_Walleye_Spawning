## --------------------------------------------------------------#
## Script name: temp_walleye_transect
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
  sf::st_line_sample(density = 1/5)  # Sample one point every 5 meters along ALL lines

cat("Sampled features:", nrow(temp_sampled), "\n")

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

# Step 5: Filter points to keep only those within param_min_dist of any habitat site
# Calculate distance from each point to nearest habitat site
temp_all_points_dist <- temp_all_points %>%
  mutate(
    nearest_hab_dist = as.numeric(
      sf::st_distance(sf::st_geometry(.), sf::st_geometry(temp_hab_sites), by_element = FALSE) %>%  # Distance matrix to all habitat sites
        apply(1, min)  # For each point, find minimum distance to any habitat site
    )
  )

# Keep only points within the distance threshold
temp_nearby_points <- temp_all_points_dist %>%
  filter(nearest_hab_dist <= param_min_dist)

cat("Points within", param_min_dist, "m of habitat:", nrow(temp_nearby_points),
    "(", round(100 * nrow(temp_nearby_points) / nrow(temp_all_points), 1), "% of total)\n")

# Step 6: Verify distances are correct (should all be ≤ param_min_dist)
cat("Distance verification - min:", round(min(temp_nearby_points$nearest_hab_dist, na.rm=T), 1),
    "m, max:", round(max(temp_nearby_points$nearest_hab_dist, na.rm=T), 1), "m\n")

# Step 7: Transform final result back to WGS84 (matches other datasets in project)
shapefile_Walleye_buffered <- temp_nearby_points %>%
  select(-nearest_hab_dist) %>%  # Remove distance column for cleaner output
  sf::st_transform(crs = 4326)


### Visualize buffered segments
#----------------------------#
temp_plot_buffered <- ggplot() +
  geom_sf(data = shapefile, colour = "black", fill = NA, inherit.aes = FALSE) +
  # Original full transect lines (gray, faded)
  geom_sf(data = shapefile_Walleye, colour = "brown", alpha = 0.3,
          linewidth = 1.2, inherit.aes = FALSE) +
  # Buffered segments within param_min_dist (red, prominent)
  geom_sf(data = shapefile_Walleye_buffered, colour = "firebrick",
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



print(temp_plot_buffered)


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


### Cleanup
#----------------------------#
rm(list = ls(pattern = "^temp_"))
cat("\nWalleye transect buffering complete!\n")
