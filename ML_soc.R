# load packages 
library (sf) 
library(readxl)
library(janitor)
library(leaflet)
library(tidyverse)
library(viridis)
library(raster)
# Read the shapefile of the study area and other related data
inpath<-"C:\\workspace\\Kirinyet-development\\data\\CSA_Baseline_SOC\\"
CAA<-st_read(paste0(inpath,"Milcah\\CSA_Conservation_Agreement_Areas.shp"))
CSA_NL <-st_read(paste0(inpath, "Milcah\\CSA_NAtional_Landscapes.shp"))
SOC_locations<-st_read(paste0(inpath, "Milcah\\SOC_GPS_Locations.shp"))

mzimvubu <- CSA_NL[CSA_NL$Name == "Mzimvubu Landscape", ] #subset NLA to mzimvubu
plot(mzimvubu["Name"], main= "Mzimvubu")

SOC_points<-SOC_locations["Name"] #
plot(st_geometry(mzimvubu), col = 'red', pch = 16, cex = 1.3, main = "Mzimvubu and sampling Points")
plot(st_geometry(SOC_points), col = 'blue', pch = 16, cex = 1.3, add = TRUE)


# Reading the data points file
data <- read_excel(paste0(inpath,"Milcah\\Mitsubishi_SOC_Baseline_December_2022.xlsx"))
str(data)
data_clean<- janitor::clean_names(data)
View(data_clean)

# convert the (x,y)degrees, minutes and seconds into decimal degrees format 
convert_DMS_to_DD <- function(dms_string) {
  dms_string <- trimws(dms_string)
  if (!grepl("’", dms_string)) {
    return(as.numeric(gsub("°", "", dms_string)))
  }
  degrees <- as.numeric(gsub("^([0-9]+)°.*", "\\1", dms_string))
  minutes <- as.numeric(gsub(".*°([0-9]+)’.*", "\\1", dms_string))
  seconds <- as.numeric(gsub(".*’([0-9.]+)’’.*", "\\1", dms_string))
  decimal_degrees <- degrees + minutes / 60 + seconds / 3600
  if (grepl("S", dms_string, ignore.case = TRUE)) {
    decimal_degrees <- -decimal_degrees
  }
  return(decimal_degrees)
}

# Convert latitude and longitude values to decimal degrees
ocs_ec_df <- data_clean %>% 
  mutate(lat_decimal = sapply(lat, convert_DMS_to_DD),
         long_decimal = sapply(long, convert_DMS_to_DD))

# check the updated dataframe
print(ocs_ec_df)
str(ocs_ec_df)

#Remove the missing values in x,y and convert to spatialpointdataframe
ocs_ec_df <- ocs_ec_df[!is.na(ocs_ec_df$long_decimal) & !is.na(ocs_ec_df$lat_decimal), ]
coordinates(ocs_ec_df) <- ~long_decimal + lat_decimal
str(ocs_ec_df)


# Convert data points to an sf object
ocs_ec_sf <- st_as_sf(ocs_ec_df) 
# use WGS 84 (EPSG:4326) to set the Coordinate Reference System
st_crs(mzimvubu) <- 4326
st_crs(ocs_ec_sf) <- 4326

plot(density(ocs_ec_sf$soc_tha))#check the density distribution

# Visualize the data - C_percent scaled by 10 (g/kg)
data_points <- ggplot() +
  geom_sf(data = mzimvubu, fill = "lightgray") +
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
  geom_sf(data = mzimvubu, fill = "lightgray") +
  geom_sf(data = ocs_ec_sf, aes(geometry = geometry, color = soc_tha), size = 4) +
  scale_color_viridis(name = "OCS (t/ha)") + 
  labs(title = "Soil Organic Carbon Stock at point",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()


#### interactive maps for datapoints and c_percent ####
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

soils<- st_read("C:\\workspace\\Kirinyet-development\\data\\mzimvubusoils.shp")
summary(soils)
print(soils$DOMSOI)

#  plot
ggplot() +
  geom_sf(data = soils, aes(fill = DOMSOI)) + 
  #geom_sf(data = SOC_points, color = "blue") +
  theme_minimal() +
  labs(fill = "Dominant Soil")



#### ISRIC legacy maps ####
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

summary(parametersestimates_cropped$BULK) ## compare to estimated bulk density


#### Extract relevant covariates
cols_terrain <- c("LITHOLOGY", "LANDFORM")
terrain_selected <- terrainproperties_cropped[, cols_terrain]

cols_parameters <- c("CECC", "PHAQ", "TOTC", "TOTN", "ECEC", "STPC", "CLPC", "BULK", "CECS", "SDTO")
parameters_selected <- parametersestimates_cropped[, cols_parameters]

save_sf_as_shp_masked <- function(sf_object, mzimvubu, output_directory) {
  masked_sf <- st_intersection(sf_object, mzimvubu)
  object_name <- deparse(substitute(sf_object))
  full_path <- file.path(output_directory, paste0(object_name, ".shp"))
  st_write(masked_sf, full_path)
}

output_path_sf <- "C:\\workspace\\covariate\\soils\\"
save_sf_as_shp_masked(terrain_selected, mzimvubu, output_path_sf)
save_sf_as_shp_masked(parameters_selected, mzimvubu, output_path_sf)

####mean SOC \(Venter et al 2021)####
inpath_onedrive <- "C:\\Users\\milcah\\OneDrive\\EC_project\\Eastern Cape data"
file_path <- paste0(inpath_onedrive, "\\SOC_sa\\mean_soc.tif")
raster_layer <- raster::raster(file_path)
plot(raster_layer,main = "Soil Organic Carbon mean 1984 - 2019 (kg C m-2)", 
     xlab = "Longitude", ylab = "Latitude")

### Global soc map at 250m resolution

GSOC<-raster(paste0(inpath_onedrive,"\\Isric data\\GSOC_m_30cm_250m.tif"))
plot(GSOC,main = "Subset of Global soil organic carbon map(t/ha)",
     xlab = "Longitude", ylab = "Latitude") 
