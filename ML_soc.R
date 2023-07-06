# load packages 
library(raster)
library (sf) 
library (caret)
library (randomForest)
library(sp)
library(rgeos)
library(gganimate)
library(ncdf4)
library(mlr3verse)
library(terra)
library(readxl)
library(janitor)
library(rasterVis) 
library(geodata)
library(elevatr)
library(RColorBrewer)
library(leaflet)
library(gstat)
library(rstac)
library(gdalcubes)
library(tidyverse)
library(MODIStsp)
library(viridis)
library(viridis)
library(rgee)

# Read the shapefile of the administrative boundaries
mzimvubu <- st_read("data\\mzimvubu.shp")
plot(mzimvubu, main="Mzimvubu")

# Reading the data points file
data <- read_excel("data\\ocs_ec_data.xls") 
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


# Exporting sample locations to  shapefile
#st_write(ocs_ec_sf, "data/soilsamplepoints_4326_crs.shp")

# Visualize the data - C_percent scaled by 10 (g/kg)
data_points <- ggplot() +
  geom_sf(data = mzimvubu_sf, fill = "lightgray") +
  geom_sf(data = ocs_ec_sf, aes(shape = treatment, color = c_percent * 10)) + 
  ggtitle("Soil Organic Carbon content in g/kg by Treatment") +
  scale_color_viridis(name = "C percent") + 
  scale_shape_manual(values = c("RESTED" = 16, "GRAZED" = 17, "-" = 15)) + # 16 is a solid circle, 17 is a triangle, 15 is a square
  labs(x = "Longitude", y = "Latitude", shape = "Treatment Type") +
  theme_minimal()

# Print the plot
print(data_points)

#### interactive maps for datapoints and c_percent ####
# Extract longitude and latitude from geometry column

coords <- st_coordinates(ocs_ec_sf$geometry)
ocs_ec_sf$longitude <- coords[, 1]
ocs_ec_sf$latitude <- coords[, 2]

# Create default OpenStreetMap tile layer map using leaflet
map <- leaflet() %>%
  addTiles()  

color_palette <- colorNumeric(palette = "viridis", domain = ocs_ec_sf$c_percent * 10)

# Add points to the map
map <- map %>%
  addCircleMarkers(
    data = ocs_ec_sf,
    lng = ~longitude,
    lat = ~latitude,
    color = ~color_palette(c_percent * 10),
    popup = ~paste0("Treatment: ", treatment, "<br>",
                    "C percent: ", c_percent) # Adding a popup to show info
  )

# Add administrative boundary to the map 
map <- map %>%
  addPolygons(data = mzimvubu, weight = 2, color = "#FF5733", fillOpacity = 0)

# Print the map
print(map)


#### Bulk density estimation and OCS ####
#estimating bulk density BERNOUX et al.1998/souza et al 2016 #replace with actual dataset
ocs_ec_sf$BD <- 1.524 - (0.0046*ocs_ec_sf$clay_percent) - (0.051*ocs_ec_sf$c_percent)- (0.0045*ocs_ec_sf$ph_k_cl) + (0.001*ocs_ec_sf$sand_percent)

# Create a scatter plot of c_percent vs BD
plot <- ggplot(ocs_ec_sf, aes(x = c_percent, y = BD)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "C Percent", y = "BD", title = "Relationship between C Percent and BD") +
  theme_minimal()

print(plot)


#Estimating the organic carbon stock 
depth = 40 #cm
ocs_ec_sf$ocs<- ocs_ec_sf$c_percent * ocs_ec_sf$BD * depth  #t/ha seboko et al

# Plot the point ocs
# Plot the point ocs
ggplot() +
  geom_sf(data = mzimvubu_sf, fill = "lightgray") +
  geom_sf(data = ocs_ec_sf, aes(geometry = geometry, color = ocs), size = 4) +
  scale_color_viridis(name = "OCS (t/ha)") + 
  labs(title = "Soil Organic Carbon Stock at point",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

#### Elevation,Apect,TWI,Slope ####
#Elevation data and slope #https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html

mzimvubu_sp <- mzimvubu_sf[!st_is_empty(mzimvubu_sf$geometry),]#remove empty geometries
elevation <- elevatr::get_elev_raster(locations = mzimvubu_sp, z = 7)
plot(elevation, main="DEM (meters)")
plot(mzimvubu_sp,col="NA",border="black", add = TRUE)


slope = terrain(elevation,opt='slope', unit='degrees')
aspect = terrain(elevation,opt='aspect',unit='degrees')

#Plot slope
plot(slope,main="Slope for mzimvubu (degrees)", col=topo.colors(6,alpha=0.6))
plot(mzimvubu_sp, add=TRUE,col="NA",border="black", lwd=0.5)


# Calculate Topographic Wetness Index (TWI)
slope_radians <- terrain(elevation, opt = "slope", unit = "radians")
area <- terrain(elevation, opt = "TPI")
area <- exp(area)
twi <- log(area / tan(slope_radians))

# Plot Topographic Wetness Index
plot(twi, main="Topographic Wetness Index for mzimvubu", col=topo.colors(6, alpha=0.6))
plot(mzimvubu_sp, add=TRUE, col="NA", border="black", lwd=0.5)


#interactive mapping

leaflet() %>% 
  addMarkers(lng = 28, lat = -30, 
             popup = "The view from here is amazing!") %>% 
  addProviderTiles("Esri.WorldImagery") 


## Color ramps
pal1 <- colorNumeric("YlGnBu", domain = ocs_ec_sf$c_percent)
pal2 <- colorNumeric("YlOrBr", domain = values(elevation), na.color = "transparent")


# Reduce the resolution of the raster
elevation_resampled <- aggregate(elevation, fact=5) # change factor based on the size 

leaflet() %>%
  # Add raster layer for elevation
  addProviderTiles("Esri.WorldImagery") %>%
  addRasterImage(elevation_resampled, colors = pal2, opacity = 0.7) %>%
  addLegend("topright", opacity = 0.8, pal = pal2, values = values(elevation_resampled),
            title = "Elevation") %>%
  # Add points for Soil Organic Carbon
  addCircleMarkers(data = ocs_ec_sf, color = ~pal1(c_percent),
                   popup = ~sprintf("Soil Organic Carbon: %.2f%%<br>Treatment: %s", c_percent, treatment)) %>%
  addLegend("bottomright", pal = pal1, values = ocs_ec_sf$c_percent,
            title = "Soil Organic Carbon (%)", opacity = 0.8)


#### Bioclim ####
inpath <- "C:\\Users\\milcah\\OneDrive\\EC_project\\Eastern Cape data"
bioclim_path <- file.path(inpath, "covariate", "wc2-5", "bio_2-5m_bil")
file_list <- list.files(path = bioclim_path, pattern = "\\.bil$", full.names = TRUE)#list datafiles
bioclim <- stack(file_list)

# Crop the raster
mzimvubu_spatial <- as(mzimvubu_sp, "Spatial")
bioclim_cropped <- raster::crop(bioclim, mzimvubu_spatial)

layers_to_plot <- c(1, 12, 14, 19) #select variable
par(mfrow = c(2, 2)) # Set up a 2x2 plot layout

for (i in layers_to_plot) {
  plot(bioclim_cropped[[i]], main = names(bioclim)[i])
}


#### LULC ####

# Load the .tif files as raster objects using 
file_path1 <- file.path(inpath, "LULC", "35J_20220101-20230101.tif")
file_path2 <- file.path(inpath, "LULC", "35H_20220101-20230101.tif")
ESRIlulc1 <- raster(file_path1)
ESRIlulc2 <- raster(file_path2)




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

# Now crop the properties to the Eastern Cape region 
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


#### temp and rainfall 

prec_data <- raster("C:\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_prec_2020-2021\\wc2.1_2.5m_prec_2020-01.tif", layer = "south_africa")
tmax_data <- raster("C:\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_tmax_2020-2021\\wc2.1_2.5m_tmax_2020-01.tif", layer = "south_africa")
tmin_data <- raster("C:\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_tmin_2020-2021\\wc2.1_2.5m_tmin_2020-01.tif", layer = "south_africa")


# Generate a list of file paths for the 2020 data (prec)
prec_files_2020 <- paste0("C:\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_prec_2020-2021\\wc2.1_2.5m_prec_2020-", sprintf("%02d", 1:12), ".tif")

# Load and crop each raster file
prec_list_2020 <- lapply(prec_files_2020, function(file) {
  raster <- raster(file)
  raster_cropped <- crop(raster, extent(mzimvubu))
  return(raster_cropped)
})

# Set up the plot layout
par(mfrow = c(3, 4), mar = c(2, 2, 2, 2))

# Plot each cropped raster
for(i in seq_along(prec_list_2020)){
  plot(prec_list_2020[[i]], main = paste("Month", i), cex.main = 0.8)
}


# Generate a list of file paths for the 2020 data(tmin)
tmin_files_2020 <- paste0("C:\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_tmin_2020-2021\\wc2.1_2.5m_tmin_2020-", sprintf("%02d", 1:12), ".tif")

# Load and crop each raster file
tmin_list_2020 <- lapply(tmin_files_2020, function(file) {
  raster <- raster(file)
  raster_cropped <- crop(raster, extent(Mzimvubu))
  return(raster_cropped)
})

# Set up the plot layout
par(mfrow = c(3, 4), mar = c(2, 2, 2, 2))

# Plot each cropped raster
for(i in seq_along(tmin_list_2020)){
  plot(tmin_list_2020[[i]], main = paste("Month", i), cex.main = 0.8)
}

# Generate a list of file paths for the 2020 data(tmax)
tmax_files_2020 <- paste0("C:\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_tmax_2020-2021\\wc2.1_2.5m_tmax_2020-", sprintf("%02d", 1:12), ".tif")

# Load and crop each raster file
tmax_list_2020 <- lapply(tmax_files_2020, function(file) {
  raster <- raster(file)
  raster_cropped <- crop(raster, extent(Mzimvubu))
  return(raster_cropped)
})

# Set up the plot layout
par(mfrow = c(3, 4), mar = c(2, 2, 2, 2))

# Plot each cropped raster
for(i in seq_along(tmax_list_2020)){
  plot(tmax_list_2020[[i]], main = paste("Month", i), cex.main = 0.8)
}


ZA_soter<- st_read("C:\\Eastern Cape data\\ZA-SOTER\\SOTER_ZA\\GIS\\SOTER\\ZA_SOTERv1.shp")
print(names(ZA_soter))


####mean SOC \(Venter et al 2021)####

file_path_1 <- file.path(inpath, "SOC_sa", "SOC_mean_30m_1.tif")
file_path_2 <- file.path(inpath, "SOC_sa", "SOC_mean_30m_2.tif")
file_path_3 <- file.path(inpath, "SOC_sa", "SOC_mean_30m_3.tif")
file_path_4 <- file.path(inpath, "SOC_sa", "SOC_mean_30m_4.tif")

# Read the .tif files as raster objects
soc_mean_1 <- raster(file_path_1)
soc_mean_2 <- raster(file_path_2)
soc_mean_3 <- raster(file_path_3)
soc_mean_4 <- raster(file_path_4)

# Merge the 4 raster files
merged_raster <- merge(soc_mean_1, soc_mean_3,soc_mean_2,soc_mean_4)
#soc_cropped<- raster::crop(merged_raster,extent(mzimvubu_spatial))

# Plot the merged raster file
plot(soc_cropped, main = "SOC Mean annual SOC(kg C m-2) predictions between 1984 and 2019", 
     xlab = "Longitude", ylab = "Latitude")

soc_trend_1 <- raster("C:\\Eastern Cape data\\SOC_sa\\SOC_trend_30m_4.tif")
soc_trend_2 <- raster("C:\\Eastern Cape data\\SOC_sa\\SOC_trend_30m_3.tif")
soc_trend_3 <- raster("C:\\Eastern Cape data\\SOC_sa\\SOC_trend_30m_1.tif")
soc_trend_4 <- raster("C:\\Eastern Cape data\\SOC_sa\\SOC_trend_30m_2.tif")

merged_raster_trend <- merge(soc_trend_1, soc_trend_2, soc_trend_3, soc_trend_4)
soc_trend_cropped <- crop(merged_raster_trend,extent(Mzimvubu))

plot(soc_trend_cropped,main = "Soil Organic Carbon (SOC) Trend", 
     xlab = "Longitude", ylab = "Latitude")



###########################################


#### NDVI/EVI ####
#https://cran.r-project.org/web/packages/MODIStsp/vignettes/MODIStsp.html

MODIStsp_get_prodlayers("M*D13A2")

xmin <- 27.28346
xmax <- 30.20073
ymin <- -32.45875
ymax <- -29.91200

extent <- c(xmin, xmax, ymin, ymax)

# Launch the processing
MODIStsp(
  gui              = FALSE,
  out_folder       = "ndvi_images",
  start_date       = "2022.01.01",
  end_date         = "2022.12.31",
  selprod          = "Vegetation_Indexes_16Days_1Km (M*D13A2)",
  bandsel          = c("NDVI", "EVI"),
  extent           = extent,
  out_proj4        = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
  user             = "mstp_test",
  password         = "MSTP_test_01",
  verbose          = FALSE,
  parallel         = FALSE
)

# Check the output
list.files(paste0(ndvi_images, "/VI_16Days_1Km_v6/NDVI"))


###########################

#############################################

# Read in the covariate rasters

min_temp <- raster("min_temp.tif")
max_temp <- raster("max_temp.tif")
rainfall <- raster("rainfall.tif")
land_use <- raster("land_use.tif")

# Build a raster stack of the covariates
covariate_stack <- stack(elevation, slope, aspect, min_temp, max_temp, rainfall, land_use)

# Crop the raster stack to the study area extent
study_area <- st_bbox(mzimvubu)
covariate_stack_cropped <- crop(covariate_stack, extent(study_area))

# Use the raster stack for soil carbon modeling
# ...m
