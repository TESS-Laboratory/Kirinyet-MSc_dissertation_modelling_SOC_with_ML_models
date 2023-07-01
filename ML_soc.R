# load packages 
library(raster)
library (sf) 
library (caret)
library (randomForest)
library(rgdal) 
library(dplyr)
library(sp)
library(plyr)
library(purrr)
library(ggplot2)
library(rgeos)
library(gganimate)
library(ncdf4)
library(mlr3verse)
library(terra)
library(readxl)
library(janitor)
library(tidyr)
library(rasterVis) 
library(geodata)
library(elevatr)
library(RColorBrewer)
library(leaflet)
library(gstat)




##########################################################
# Read the shapefile of study area
mzimvubu <- st_read("C:\\Eastern Cape data\\ZAF_adm (1)\\mzimvubu.shp")
plot(mzimvubu, main="Mzimvubu")

# Reading the data points file
data <- read_excel("C:/workspace/ocs_ec_data.xls")
str(data)
data_clean<- janitor::clean_names(data)
View(data_clean)

# convert the (x,y)degrees, minutes and seconds into decimal degrees format for computational purposes
convert_DMS_to_DD <- function(dms_string) {
  # Trim white spaces
  dms_string <- trimws(dms_string)
  
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


# Write to shapefile
st_write(ocs_ec_sf, "C:\\workspace\\ec_data_shp//ec_4326_crs.shp")

data_check<-readOGR("C:\\workspace\\ec_data_shp//ec_4326_crs.shp")
head(data_check)# Display the first few rows of the data


#data ranges for visualization
bin_labels <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50-55", "55-60", "60-65", "65-70")

# Specify distinct colors for each bin label 
bin_colors <- c("0-5" = "#9400D3", "5-10" = "#4B0082", "10-15" = "#0000FF", "15-20" = "#00FF00",
                "20-25" = "#FFFF00", "25-30" = "#FF7F00", "30-35" = "#FF0000", "35-40" = "#FF1493",
                "40-45" = "#8B4513", "45-50" = "#9ACD32", "50-55" = "#FFD700", "55-60" = "#D2691E",
                
                "60-65" = "#F08080", "65-70" = "#ADFF2F")

# Create bins for c_percent_scaled
ocs_ec_sf$c_percent_scaled_bins <- cut(ocs_ec_df$c_percent * 10, 
                                       breaks = seq(0, 70, by = 5), 
                                       include.lowest = TRUE, 
                                       labels = bin_labels)

# Create the ggplot
data_points <- ggplot() +
  geom_sf(data = mzimvubu_sf, fill = "lightgray") +
  geom_sf(data = ocs_ec_sf, aes(shape = treatment, color = c_percent_scaled_bins)) + # Map color to c_percent_scaled_bins
  ggtitle("Soil Organic Carbon content in g/kg by Treatment") +
  scale_color_manual(values = bin_colors) + # Using distinct colors for each bin
  scale_shape_manual(values = c("RESTED" = 16, "GRAZED" = 17, "-" = 15)) + # 16 is a solid circle, 17 is a triangle, 15 is a square
  labs(x = "Longitude", y = "Latitude", color = "C percent", shape = "Treatment Type") +
  theme_minimal() 

# Print the plot
print(data_points)

##########################################################
#interactive maps for datapoints and c_percent
# Extract longitude and latitude from geometry column
coords <- st_coordinates(ocs_ec_sf$geometry)
ocs_ec_sf$longitude <- coords[, 1]
ocs_ec_sf$latitude <- coords[, 2]

# Create the base map
map <- leaflet() %>%
  addTiles()  # Adds a default OpenStreetMap tile layer

# Define a color palette for C percent values
color_palette <- colorFactor(bin_colors, domain = ocs_ec_sf$c_percent_scaled_bins)

# Add points to the map
map <- map %>%
  addCircleMarkers(
    data = ocs_ec_sf,
    lng = ~longitude,
    lat = ~latitude,
    color = ~color_palette(c_percent_scaled_bins),
    popup = ~paste0("Treatment: ", treatment, "<br>",
                    "C percent: ", c_percent) # Adding a popup to show info
  )
# Add Mzimvubu boundary to the map
map <- map %>%
  addPolygons(data = mzimvubu, weight = 2, color = "#FF5733", fillOpacity = 0)

print(map)



##########################################
#estimating bulk density BERNOUX et al.1998/souza et al 2016
ocs_ec_sf$BD <- 1.524 - (0.0046*ocs_ec_sf$clay_percent) - (0.051*ocs_ec_sf$c_percent)- (0.0045*ocs_ec_sf$ph_k_cl) + (0.001*ocs_ec_sf$sand_percent)

# Create a scatter plot of c_percent vs BD
plot <- ggplot(ocs_ec_sf, aes(x = c_percent, y = BD)) +
  geom_point() +  # Use points to represent each data
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "C Percent", y = "BD", title = "Relationship between C Percent and BD") +
  theme_minimal()

print(plot)


#Estimating the total cabon stock 
depth = 40 #cm
ocs_ec_sf$ocs<- ocs_ec_sf$c_percent * ocs_ec_sf$BD * depth  #t/ha seboko et al

# Plot the point ocs

ggplot() +
  geom_sf(data = mzimvubu_sf, fill = "lightgray") +
  geom_sf(data = ocs_ec_sf, aes(geometry = geometry, color = ocs), size = 4) +
  scale_color_gradientn(colors = c("blue", "green", "yellow", "red"),
                        name = "OCS (t/ha)") +
  labs(title = "Soil Organic Carbon Stock at point",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() 


##################################
#Elevation data and slope #https://rpubs.com/ials2un/boyaca_dem
head(mzimvubu)
mzimvubu_sf <- mzimvubu_sf[!st_is_empty(mzimvubu_sf),]
elevation <- get_elev_raster(mzimvubu_sf,z=10)
elevation

mzimvubu_sp <- as(mzimvubu_sf, "Spatial")
plot(elevation, main=" DEM [meters]")
plot(mzimvubu_sp,col="NA",border="black", add=TRUE)
text(coordinates(mzimvubu_sp), labels=as.character(mzimvubu_sf$MPIO_CNMBR), 
     col="black", cex=0.20)

hist(elevation)

slope = terrain(elevation,opt='slope', unit='degrees')
aspect = terrain(elevation,opt='aspect',unit='degrees')

#Plot slope
plot(slope,main="Slope for mzimvubu [degrees]", col=topo.colors(6,alpha=0.6))
plot(mzimvubu, add=TRUE,col="NA",border="black", lwd=0.5)
text(coordinates(mzimvubu_rep), labels=as.character(mzimvubu_rep$MPIO_CNMBR), cex=0.2)

#Plot aspect.
plot(aspect,main="Aspect for mzimvubu [degrees]", col=rainbow(10,alpha=0.7))
plot(mzimvubu, add=TRUE,col="NA",border="black", lwd=0.5)
text(coordinates(mzimvubu_rep), labels=as.character(mzimvubu_rep$MPIO_CNMBR), cex=0.2)


#interactive mapping

leaflet() %>% 
  addMarkers(lng = 28, lat = -30, 
             popup = "The view from here is amazing!") %>% 
  addProviderTiles("Esri.WorldImagery") 

library(leaflet)

## Color ramps
pal1 <- colorNumeric("YlGnBu", domain = ocs_ec_sf$c_percent)
pal2 <- colorNumeric("YlOrBr", domain = values(elevation), na.color = "transparent")

# Create interactive map
leaflet() %>%
  # Add raster layer for elevation
  addProviderTiles("Esri.WorldImagery") %>%
  addRasterImage(elevation, colors = pal2, opacity = 0.7) %>%
  addLegend("topright", opacity = 0.8, pal = pal2, values = values(elevation), 
            title = "Elevation") %>%
  # Add points for Soil Organic Carbon
  addCircleMarkers(data = ocs_ec_sf, color = ~pal1(c_percent),
                   popup = ~sprintf("Soil Organic Carbon: %.2f%%<br>Treatment: %s", c_percent, treatment)) %>%
  addLegend("bottomright", pal = pal1, values = ocs_ec_sf$c_percent,
            title = "Soil Organic Carbon (%)", opacity = 0.8)



############################

dir_path <- "C:/workspace/Kirinyet-development/Data/covariate/wc2-5/bio_2-5m_bil"

# List all the .bil files in the directory
file_list <- list.files(path = dir_path, pattern = "\\.bil$", full.names = TRUE)

# Create a raster stack from the list of  files
bioclim <- stack(file_list)

# Transform the CRS to match the raster
mzimvubu <- st_transform(mzimvubu, st_crs(bioclim))

# Now try cropping the raster
cropped_bioclim <- crop(bioclim, extent(mzimvubu))

layers_to_plot <- c(1, 12, 14, 19)
par(mfrow = c(2, 2))# Set up a 2x2 plot layout

for (i in layers_to_plot) {
  plot(cropped_bioclim[[i]])
}





###################################LULC
# Import raster layers
ESRIlulc1 <- raster("C:\\Eastern Cape data\\LULC\\35J_20220101-20230101.tif")
ESRIlulc2 <- raster("C:\\Eastern Cape data\\LULC\\35H_20220101-20230101.tif")

# Merge raster layers into a single raster stack
merged_raster <- merge(ESRIlulc1, ESRIlulc2)

# Define area of interest using an extent object
aoi_extent <- extent(xmin = 27, xmax = 29, ymin = -31, ymax = -30.1)

# Crop the merged raster stack to the area of interest
cropped_raster <- crop(merged_raster, aoi_extent)

# Save the cropped raster stack as a file
writeRaster(cropped_raster, "C:\workspace\Kirinyet-development\Data\covariate"/cropped_lulc_raster.tif, format = "GTiff")

############################################

inpath <- "C:\\Eastern Cape data\\SOTWIS_SAF\\SOTWIS_SAF\\GIS\\SOTWIS\\"

# Load each shapefile Using SF package
parametersestimates <- st_read(paste0(inpath, "SOTWIS_0-20cm_parametersestimates\\SOTWIS_SAFv1_t1s1d1.shp"))
soterunitcomposition <- st_read(paste0(inpath, "SOTWIS_soterunitcomposition\\SOTWIS_SAFv1_soterunitcomposition.shp"))
terrainproperties <- st_read(paste0(inpath, "SOTWIS_terrainproperties\\SOTWIS_SAFv1_terrainproperties.shp"))
primarydata <- st_read("C:\\Eastern Cape data\\SOTWIS_SAF\\SOTWIS_SAF\\GIS\\SOTER_primarydata\\SOTERSAFv1_newsuid.shp")

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
terrainproperties_cropped <- st_crop(terrainproperties, eastern_cape)
parametersestimates_cropped <- st_crop(parametersestimates, eastern_cape)
soterunitcomposition_cropped <- st_crop(soterunitcomposition,eastern_cape)



for (dataset in list(terrainproperties_cropped,parametersestimates_cropped,soterunitcomposition_cropped)) {
  str(dataset)
  print(head(dataset, n = 5))
  print(summary(dataset)) 
}

#  plot the cropped terrainproperties and parameterestimates
ggplot() +
  geom_sf(data = terrainproperties_cropped, aes(fill = SOILS)) +
  theme_minimal() +
  ggtitle("Terrain Properties - Eastern Cape soils") +
  scale_fill_viridis_d() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = parametersestimates_cropped, aes(fill = TOTC)) +
  theme_minimal() +
  ggtitle("Parameter Estimates - Eastern Cape") +
  scale_fill_viridis_c() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = parametersestimates_cropped, aes(fill =BULK)) +
  theme_minimal() +
  ggtitle("Parameters Estimates-Eastern Cape") +
  scale_fill_viridis_c() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = parametersestimates_cropped, aes(fill =TOTN)) +
  theme_minimal() +
  ggtitle("Parameters Estimates-total nitrogen (g kg-1)") +
  scale_fill_viridis_c() +
  theme(legend.position = "bottom")

summary(parametersestimates_cropped$BULK)##
# Read the shapefiles
prec_data <- raster("C:\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_prec_2020-2021\\wc2.1_2.5m_prec_2020-01.tif", layer = "south_africa")
tmax_data <- raster("C:\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_tmax_2020-2021\\wc2.1_2.5m_tmax_2020-01.tif", layer = "south_africa")
tmin_data <- raster("C:\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_tmin_2020-2021\\wc2.1_2.5m_tmin_2020-01.tif", layer = "south_africa")


# Generate a list of file paths for the 2020 data (prec)
prec_files_2020 <- paste0("C:\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_prec_2020-2021\\wc2.1_2.5m_prec_2020-", sprintf("%02d", 1:12), ".tif")

# Load and crop each raster file
prec_list_2020 <- lapply(prec_files_2020, function(file) {
  raster <- raster(file)
  raster_cropped <- crop(raster, extent(eastern_cape))
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
  raster_cropped <- crop(raster, extent(eastern_cape))
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
  raster_cropped <- crop(raster, extent(eastern_cape))
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


soc_mean_3 <- raster("C:\\Eastern Cape data\\SOC_sa\\SOC_mean_30m_3.tif")
soc_mean_1 <- raster("C:\\Eastern Cape data\\SOC_sa\\SOC_mean_30m_1.tif")
soc_mean_2 <- raster("C:\\Eastern Cape data\\SOC_sa\\SOC_mean_30m_2.tif")
soc_mean_4 <- raster("C:\\Eastern Cape data\\SOC_sa\\SOC_mean_30m_4.tif")

# Merge the two raster files
merged_raster <- merge(soc_mean_1, soc_mean_3,soc_mean_2,soc_mean_4)
soc_cropped<- crop(merged_raster,extent(eastern_cape))

# Plot the merged raster file
plot(soc_cropped, main = "SOC Mean annual SOC(kg C m-2) predictions between 1984 and 2019", 
     xlab = "Longitude", ylab = "Latitude")

soc_trend_1 <- raster("C:\\Eastern Cape data\\SOC_sa\\SOC_trend_30m_4.tif")
soc_trend_2 <- raster("C:\\Eastern Cape data\\SOC_sa\\SOC_trend_30m_3.tif")
soc_trend_3 <- raster("C:\\Eastern Cape data\\SOC_sa\\SOC_trend_30m_1.tif")
soc_trend_4 <- raster("C:\\Eastern Cape data\\SOC_sa\\SOC_trend_30m_2.tif")

merged_raster_trend <- merge(soc_trend_1, soc_trend_2, soc_trend_3, soc_trend_4)
soc_trend_cropped <- crop(merged_raster_trend,extent(eastern_cape))

plot(soc_trend_cropped,main = "Soil Organic Carbon (SOC) Trend", 
     xlab = "Longitude", ylab = "Latitude")



###########################################


#####################################




########################NDVI

# Initialize Earth Engine
ee_Initialize()


# Load and process the MODIS ImageCollection
col <- ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')

col <- col$map(function(img) {
  doy <- ee$Date(img$get('system:time_start'))$getRelative('day', 'year')
  img$set('doy', doy)
})

distinctDOY <- col$filterDate('2021-01-01', '2023-01-01')

filter <- ee$Filter$equals(leftField = 'doy', rightField = 'doy')
join <- ee$Join$saveAll('doy_matches')
joinCol <- ee$ImageCollection(join$apply(distinctDOY, col, filter))

comp <- joinCol$map(function(img) {
  doyCol = ee$ImageCollection$fromImages(img$get('doy_matches'))
  doyCol$reduce(ee$Reducer$median())
})

# Define RGB visualization parameters
visParams = list(
  min = 0.0,
  max = 9000.0,
  bands = "NDVI_median",
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)

rgbVis <- comp$map(function(img) {
  img$visualize(visParams) %>% # Note: Removed do.call
    ee$Image$clip(clip_geometry = mzimvubu_sp) # Pass mzimvubu as an argument
})


# Define parameters for image export
imgParams = list(
  region = mzimvubu,
  dimensions = 600,
  crs = 'EPSG:3857',
  framesPerSecond = 10
)

# Create a folder to save the images
#dir.create("ndvi_images")

# Save each image to a file
i <- 0
rgbVis$map(function(image) {
  i <<- i + 1
  fileName <- sprintf("ndvi_images/ndvi_%03d.png", i)
  ee_as_image(image, name = fileName, params = imgParams)
})



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
covariate_stack_cropped <- crop(covariate_stack, extent(study_area_bbox))

# Use the raster stack for soil carbon modeling
# ...m
