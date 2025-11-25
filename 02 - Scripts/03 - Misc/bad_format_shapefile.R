###Read in shapefile
shapefile <- read_sf("01 - Data/HH_shapefile/HH_Water_Poly.shp") #st_read()
shapefile <- st_transform(shapefile, crs = 3857) #Also try 'EPSG:32724'

 


### Turn shapefile into dataframe
#----------------------------#

# Convert the sf object to a dataframe with coordinates
shapefile_df <- shapefile %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%  # Ensure it's in latitude/longitude (WGS84) if necessary
  st_cast("MULTIPOLYGON") %>%    # Cast to MULTIPOLYGON if needed
  st_coordinates() %>%
  as.data.frame()

# Extract the IDs and groupings
shapefile_df <- shapefile %>%
  mutate(group = as.character(1:n())) %>%
  cbind(shapefile_df)

# Rename the columns for ggplot
colnames(shapefile_df)[1:2] <- c("long", "lat")

# Add a group for polygons if needed
shapefile_df <- shapefile_df %>%
  group_by(L1)


###map of rec
ggmap(HHmap, extent='normal')+
   geom_polygon(data=shapefile_df, aes(x = long, y = lat, group = L1), colour = "firebrick", fill=NA, inherit.aes = FALSE)+
   coord_equal()




### Plot shapefile with geom_sf
#----------------------------#
shapefile <- read_sf("01 - Data/HH_shapefile/HH_Water_Poly.shp") #st_read()
st_crs(shapefile)

shapefile <- st_transform(shapefile, crs = 4326) #Also try 'EPSG:32724'
st_crs(shapefile)

###map of rec
ggmap(HHmap, extent='normal')+
   geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
 coord_sf()



### Other shapefiles and transformations
#----------------------------#
shapefile <- read_sf("01 - Data/HH_shapefile/HH_Water_Poly.shp") #st_read()
shapefile <- st_transform(shapefile, "WGS84") #Also try "EPSG:4326"; 'EPSG:32724'
shapefile <- st_transform(shapefile, "EPSG:4326") #Also try 'EPSG:32724'
shapefile <- st_transform(shapefile, "+proj=longlat +datum=WGS84 +no_defs") #Also try "EPSG:4326"; 'EPSG:32724'

shapefile2 <- st_transform(shapefile2, "+proj=longlat +datum=WGS84 +no_defs") #Also try "EPSG:4326"; 'EPSG:32724'






