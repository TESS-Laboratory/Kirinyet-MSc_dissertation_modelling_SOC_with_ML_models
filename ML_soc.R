# load packages 
library(raster)
library (sf) 
library(sp)
library(rgeos)
library(ncdf4)
library(terra)
library(readxl)
library(janitor)
library(rasterVis) 
library(elevatr)
library(RColorBrewer)
library(leaflet)
library(tidyverse)
library(MODIStsp)
library(viridis)
library(rgee)

# Read the shapefile of the study area and other related data
inpath<-"C:\\workspace\\Kirinyet-development\\data\\CSA_Baseline_SOC\\"
CAA<-st_read(paste0(inpath,"Milcah\\CSA_Conservation_Agreement_Areas.shp"))
CSA_NL <-st_read(paste0(inpath, "Milcah\\CSA_NAtional_Landscapes.shp"))
SOC_locations<-st_read(paste0(inpath, "Milcah\\SOC_GPS_Locations.shp"))

mzimvubu <- CSA_NL[CSA_NL$Name == "Mzimvubu Landscape", ] #subset NLA to mzimvubu
plot(mzimvubu["Name"], main= "Mzimvubu")

SOC_points<-SOC_locations["Name"] #
plot(SOC_points)


plot(st_geometry(mzimvubu), col = 'red', pch = 16, cex = 1.3, main = "Mzimvubu and sampling Points")
plot(st_geometry(SOC_points), col = 'blue', pch = 16, cex = 1.3, add = TRUE)


# Reading the data points file
data <- read_excel(paste0(inpath,"Milcah\\Mitsubishi_SOC_Baseline_December_2022.xlsx"))
str(data)
data_clean<- janitor::clean_names(data)
View(data_clean)


# convert the (x,y)degrees, minutes and seconds into decimal degrees format 
convert_DMS_to_DD <- function(dms_string) {
  
  dms_string <- trimws(dms_string)# Trim white spaces
  
  # Check if the input string is already in decimal degrees format
  if (!grepl("’", dms_string)) {
    # Remove the degree symbol if present
    dms_string <- gsub("°", "", dms_string)
    return(as.numeric(dms_string))
  }
  
  # Extract degrees, minutes, and seconds using regular expressions
  degrees <- as.numeric(gsub("^([0-9]+)°.*", "\\1", dms_string))
  minutes <- as.numeric(gsub(".*°([0-9]+)’.*", "\\1", dms_string))
  seconds <- as.numeric(gsub(".*’([0-9.]+)’’.*", "\\1", dms_string))
  
  # Convert to decimal degrees
  decimal_degrees <- degrees + minutes / 60 + seconds / 3600
  
  # Check if it's South, which should be negative
  if (grepl("S", dms_string, ignore.case = TRUE)) {
    decimal_degrees <- -decimal_degrees
  }
  
  return(decimal_degrees)
}

# Convert latitude and longitude values to decimal degrees
ocs_ec_df <- data_clean %>% 
  mutate(lat_decimal = sapply(lat, convert_DMS_to_DD),
         long_decimal = sapply(long, convert_DMS_to_DD))

# Show the updated dataframe
print(ocs_ec_df)
str(ocs_ec_df)

#Remove the missing values in x,y and convert to spatialpointdataframe
ocs_ec_df <- ocs_ec_df[!is.na(ocs_ec_df$long_decimal) & !is.na(ocs_ec_df$lat_decimal), ]
coordinates(ocs_ec_df) <- ~long_decimal + lat_decimal
str(ocs_ec_df)


# Convert the Eastern Cape shapefile and data points to an sf object
mzimvubu_sf <- as(mzimvubu, "sf")
ocs_ec_sf <- st_as_sf(ocs_ec_df) 
# use WGS 84 (EPSG:4326) to set the Coordinate Reference System
st_crs(mzimvubu_sf) <- 4326
st_crs(ocs_ec_sf) <- 4326

plot(density(ocs_ec_sf$soc_tha))#check the density distribution

# Visualize the data - C_percent scaled by 10 (g/kg)
data_points <- ggplot() +
  geom_sf(data = mzimvubu_sf, fill = "lightgray") +
  geom_sf(data = ocs_ec_sf, aes(shape = treatment, color = c_g_kg)) + 
  ggtitle("Soil Organic Carbon content in g/kg by Treatment") +
  scale_color_viridis(name = "C_g_kg") + 
  scale_shape_manual(values = c("RESTED" = 16, "GRAZED" = 17, "-" = 15)) + # 16 is a solid circle, 17 is a triangle, 15 is a square
  labs(x = "Longitude", y = "Latitude", shape = "Treatment Type") +
  theme_minimal()

# Print the plot
print(data_points)

#  c_g_kg vs BD
plot <- ggplot(ocs_ec_sf, aes( x = soilbd,y = c_g_kg)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "C_g_kg", y = "BD", title = "Relationship between C_g_kg and BD") +
  theme_minimal()

print(plot)

# c_g_kg vs  ph  
plot1 <- ggplot(ocs_ec_sf, aes(x = c_g_kg, y = p_h)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "C_g_kg", y = "pH", title = "Relationship between C_g_kg and pH") +
  theme_minimal()
print(plot1)

# Plot the point ocs
ggplot() +
  geom_sf(data = mzimvubu_sf, fill = "lightgray") +
  geom_sf(data = ocs_ec_sf, aes(geometry = geometry, color = soc_tha), size = 4) +
  scale_color_viridis(name = "OCS (t/ha)") + 
  labs(title = "Soil Organic Carbon Stock at point",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()


#### interactive maps for datapoints and c_percent ####
# Extract longitude and latitude from geometry column

coords <- st_coordinates(ocs_ec_sf$geometry)
ocs_ec_sf$longitude <- coords[, 1]
ocs_ec_sf$latitude <- coords[, 2]

# Create default OpenStreetMap tile layer map using leaflet
map <- leaflet() %>%
  addTiles()  

color_palette <- colorNumeric(palette = "viridis", domain = ocs_ec_sf$c_g_kg)

# Add points to the map
map <- map %>%
  addCircleMarkers(
    data = ocs_ec_sf,
    lng = ~longitude,
    lat = ~latitude,
    color = ~color_palette(c_g_kg),
    popup = ~paste0("Treatment: ", treatment, "<br>",
                    "c_g_kg: ", c_g_kg) # Adding a popup to show info
  )

# Add administrative boundary to the map 
map <- map %>%
  addPolygons(data = mzimvubu, weight = 2, color = "#FF5733", fillOpacity = 0)

# Print the map
print(map)


####Covariates####

#Soils

soils<- st_read("C:\\workspace\\mzimvubusoils.shp")
summary(soils)
print(soils$DOMSOI)

#  plot
ggplot() +
  geom_sf(data = soils, aes(fill = DOMSOI)) + 
  geom_sf(data = SOC_points, color = "blue") +
  theme_minimal() +
  labs(fill = "Dominant Soil")



#### ISRIC legacy maps ####
# Load each shapefile Using SF package

inpath_legacy<-"C:\\Users\\milcah\\OneDrive\\EC_project\\Eastern Cape data\\SOTWIS_SAF\\SOTWIS_SAF\\GIS\\"
parametersestimates <- st_read(paste0(inpath_legacy, "SOTWIS\\SOTWIS_0-20cm_parametersestimates\\SOTWIS_SAFv1_t1s1d1.shp"))
soterunitcomposition <- st_read(paste0(inpath_legacy, "SOTWIS\\SOTWIS_soterunitcomposition\\SOTWIS_SAFv1_soterunitcomposition.shp"))
terrainproperties <- st_read(paste0(inpath_legacy, "SOTWIS\\SOTWIS_terrainproperties\\SOTWIS_SAFv1_terrainproperties.shp"))
primarydata <- st_read(paste0(inpath_legacy,"SOTER_primarydata\\SOTERSAFv1_newsuid.shp"))

for (dataset in list(parametersestimates, soterunitcomposition, terrainproperties, primarydata)) {
  str(dataset)
  print(head(dataset, n = 5))
  print(summary(dataset)) 
}


# Plot the shapefiles samples
ggplot() +
  geom_sf(data = parametersestimates, aes(fill =TOTC)) +
  theme_minimal() +
  ggtitle("Parameters Estimates") +
  scale_fill_viridis_c() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = terrainproperties, aes(fill = SOILS)) +
  theme_minimal() +
  ggtitle("Terrain Properties") +
  scale_fill_viridis_d() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = terrainproperties, aes(fill =LITHOLOGY)) +
  theme_minimal() +
  ggtitle("terrainproperties") +
  scale_fill_viridis_d() +
  theme(legend.position = "bottom")

# Now crop the properties to the mzimvubu region 
terrainproperties_cropped <- st_crop(terrainproperties, mzimvubu)
parametersestimates_cropped <- st_crop(parametersestimates, mzimvubu)
soterunitcomposition_cropped <- st_crop(soterunitcomposition,mzimvubu)


for (dataset in list(terrainproperties_cropped,parametersestimates_cropped,soterunitcomposition_cropped)) {
  str(dataset)
  print(head(dataset, n = 5))
  print(summary(dataset)) 
}

#  plot the cropped terrainproperties and parameterestimates
ggplot() +
  geom_sf(data = terrainproperties_cropped, aes(fill = SOILS)) +
  theme_minimal() +
  ggtitle("Terrain Properties - Mzimvubu soils") +
  scale_fill_viridis_d() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = parametersestimates_cropped, aes(fill = TOTC)) +
  theme_minimal() +
  ggtitle("Parameter Estimates - Mzimvubu") +
  scale_fill_viridis_c() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = parametersestimates_cropped, aes(fill =BULK)) +
  theme_minimal() +
  ggtitle("Parameters Estimates-Mzimvubu") +
  scale_fill_viridis_c() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = parametersestimates_cropped, aes(fill =TOTN)) +
  theme_minimal() +
  ggtitle("Parameters Estimates-total nitrogen (g kg-1)") +
  scale_fill_viridis_c() +
  theme(legend.position = "bottom")

summary(parametersestimates_cropped$BULK) ## compare to estimated bulk density


###########################

# Build a raster stack of the covariates
covariate_stack <- stack(elevation, slope, aspect, min_temp, max_temp, rainfall, land_use)

# Use the raster stack for modelling with point data 
# ...m


####mean SOC \(Venter et al 2021)####

file_path <- paste0(inpath_onedrive, "\\SOC_sa\\SOC_mean_30m_1.tif")
raster_layer <- raster::raster(file_path)
cropped_layer <- raster::crop(raster_layer, mzimvubu)
plot(cropped_layer,main = "Soil Organic Carbon (SOC) Trend", 
     xlab = "Longitude", ylab = "Latitude")

