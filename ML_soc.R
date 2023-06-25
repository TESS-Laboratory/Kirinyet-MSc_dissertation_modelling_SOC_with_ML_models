# load packages 
library(raster)
library (sf) # for handling spatial objects
library (caret)
library (randomForest)
library(rgdal) # for reading shapefiles
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


##‘GADM’ returns the global administrative boundaries- use to get za/ec(study area)
iso3 <- geodata::country_codes()

iso3 %>%
  as.data.frame() %>%
  filter(NAME == "South Africa")

za=getData('GADM', country='ZAF', level=1)
plot(za)

##lets check the lower adm boundaries
za@data

eastern_cape=subset(za,NAME_1=="Eastern Cape")
plot(eastern_cape)


# Now crop the properties to the Eastern Cape region 
terrainproperties_cropped <- st_crop(terrainproperties, eastern_cape)
parametersestimates_cropped <- st_crop(parametersestimates, eastern_cape)
soterunitcomposition_cropped <- st_crop(soterunitcomposition,eastern_cape)

#  plot the cropped terrainproperties and parameterestimates
ggplot() +
  geom_sf(data = terrainproperties_cropped, aes(fill = SOILS)) +
  theme_minimal() +
  ggtitle("Terrain Properties - Eastern Cape") +
  scale_fill_viridis_d() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = parametersestimates_cropped, aes(fill = TOTC)) +
  theme_minimal() +
  ggtitle("Parameter Estimates - Eastern Cape") +
  scale_fill_viridis_c() +
  theme(legend.position = "bottom")


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



ESRIlulc1 <- raster("C:\\Eastern Cape data\\LULC\\35H_20170101-20180101.tif")
ESRIlulc2 <- raster("C:\\Eastern Cape data\\LULC\\35J_20170101-20180101.tif")


ESRIlulc <- resample(ESRIlulc1, ESRIlulc2,  method='bilinear')  # or method='nearest')
plot(ESRIlulc)

##########################################################################################################
##eastern cape data (points)
# Reading the file
data <- read_excel("C:/workspace/ocs_ec_data.xls")
str(data)
ocs_ec<- janitor::clean_names(data)
View(ocs_ec)

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
ocs_ec <- ocs_ec %>% 
  mutate(lat_decimal = sapply(lat, convert_DMS_to_DD),
         long_decimal = sapply(long, convert_DMS_to_DD))

# Show the updated dataframe
print(ocs_ec)







#####################################
## monthly precipitation (Pr) from TerraClimate:https://r-spatial.github.io/rgee/
# Load required libraries
library(tidyverse)
library(rgee)
library(sf)

library(rgee)

# Load shapefile
south_africa <- st_read("C:\\Eastern Cape data\\ZAF_adm (1)\\ZAF_adm2.shp")
eastern_cape <- south_africa[south_africa$NAME_1 == "Eastern Cape", ]


# Load ImageCollection and apply transformations
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate("2021-01-01", "2023-01-01") %>%
  ee$ImageCollection$map(function(img) {
    img$select(0:11)$rename(sprintf("PP_%02d",1:12)) # Select the first 12 bands and rename
  }) %>%
  ee$ImageCollection$toBands() # Convert ImageCollection to Image

# Extract the data
ee_eastern_cape_rain <- ee_extract(x = terraclimate, y = eastern_cape, sf = FALSE)

# Extract column names for precipitation data
cols <- grep("^X202", names(ee_eastern_cape_rain), value = TRUE)

ee_eastern_cape_rain %>%
  pivot_longer(cols = all_of(cols), names_to = "month", values_to = "pr") %>%
  mutate(month = gsub("PP_", "", month)) %>%
  ggplot(aes(x = month, y = pr, group = NAME_2, color = pr)) +
  geom_line(alpha = 0.4) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal()



########################NDVI
# Load required libraries
library(magick)
library(rgee)
library(sf)

# Initialize Earth Engine
#ee_Initialize()

# Load the Eastern Cape shapefile
mask <- st_read("C:\\Eastern Cape data\\ZAF_adm (1)\\ZAF_adm2.shp")
eastern_cape <- mask[mask$NAME_1 == "Eastern Cape", ] %>% sf_as_ee()
region <- eastern_cape$geometry()$bounds()

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
  do.call(img$visualize, visParams) %>%
    ee$Image$clip(eastern_cape)
})

# Define parameters for image export
imgParams = list(
  region = region,
  dimensions = 600,
  crs = 'EPSG:3857',
  framesPerSecond = 10
)

# Create a folder to save the images
dir.create("ndvi_images")

# Save each image to a file
i <- 0
rgbVis$map(function(image) {
  i <<- i + 1
  fileName <- sprintf("ndvi_images/ndvi_%03d.png", i)
  ee_as_image(image, name = fileName, params = imgParams)
})



###########################

