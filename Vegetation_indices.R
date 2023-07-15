library(MODIStsp)
library(rgeoboundaries)
library(sf)
library(raster)
library(here)
library(viridis)
library(tidyverse)


# Read the shapefile of the study area and other related data
inpath<-"C:\\workspace\\Kirinyet-development\\data\\CSA_Baseline_SOC\\"
CAA<-st_read(paste0(inpath,"Milcah\\CSA_Conservation_Agreement_Areas.shp"))
CSA_NL <-st_read(paste0(inpath, "Milcah\\CSA_NAtional_Landscapes.shp"))
SOC_locations<-st_read(paste0(inpath, "Milcah\\SOC_GPS_Locations.shp"))

mzimvubu <- CSA_NL[CSA_NL$Name == "Mzimvubu Landscape", ] #subset NLA to mzimvubu
plot(mzimvubu["Name"], main= "Mzimvubu")


#### NDVI/EVI ####
#https://rspatialdata.github.io/vegetation.html
#https://cran.r-project.org/web/packages/MODIStsp/vignettes/MODIStsp.html

mzimvubu_sf <- as(mzimvubu, "sf")

#MODIStsp_get_prodlayers("M*D13A2") #1km resolution
MODIStsp_get_prodlayers("M*D13Q1") #250m resolution preferred 

map_boundary <- geoboundaries("South Africa")
spatial_filepath <- "ndvi_images/South Africa.shp"
#st_write(map_boundary, paste0(spatial_filepath))

# Launch the processing
MODIStsp(
  gui              = FALSE,
  out_folder       = "ndvi_images",
  start_date       = "2022.01.01",
  end_date         = "2022.12.31",
  selprod          = "Vegetation Indexes_16Days_250m (M*D13Q1)",
  bandsel          = c("NDVI", "EVI"),
  out_proj4        = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
  user             = "mstp_test",
  password         = "MSTP_test_01",
  verbose          = FALSE,
  parallel         = FALSE,
  spatmeth = "file",
  spafile = spatial_filepath,
  out_format = "GTiff"
)

#### NDVI ####

# Reading in the downloaded NDVI raster data
NDVI_raster <- raster(here::here("C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa\\VI_16Days_250m_v6\\NDVI\\MOD13Q1_NDVI_2022_001.tif"))
NDVI_raster <- projectRaster(NDVI_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")# Transforming the data

NDVI_raster <- raster::mask(NDVI_raster, mzimvubu_sf)
gain(NDVI_raster) <- 0.0001 # Dividing values by 10000 to have NDVI values between -1 and 1

# Converting the raster object into a dataframe
NDVI_df <- as.data.frame(NDVI_raster, xy = TRUE, na.rm = TRUE)
rownames(NDVI_df) <- c()

# Visualising 
ggplot() +
  geom_raster(
    data = NDVI_df,
    aes(x = x, y = y, fill = MOD13Q1_NDVI_2022_001)
  ) +
  geom_sf(data = mzimvubu_sf, inherit.aes = FALSE, fill = NA) +
  scale_fill_viridis(name = "NDVI") +
  labs(
    title = "NDVI (Normalized Difference Vegetation Index) in Mzimvubu",
    subtitle = "01-01-2022",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()


# crop and stack them for further preprocessing
files <- list.files(path = "C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa\\VI_16Days_250m_v6\\NDVI\\", pattern = "MOD13Q1_NDVI_.*\\.tif$", full.names = TRUE)
dir.create("C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa\\VI_16Days_250m_v6\\Processed_NDVI")
mzimvubu_extent <- extent(mzimvubu_sf)

# Loop over each file
for(i in seq_along(files)) {

  date <- gsub(pattern = "^.*MOD13Q1_NDVI_(\\d{4}_\\d{3}).*.tif$", replacement = "\\1", x = basename(files[i]))
  
  # Reading in the downloaded NDVI raster data
  NDVI_raster <- raster(files[i])
  NDVI_raster <- projectRaster(NDVI_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")# Transforming the data
  NDVI_raster <- raster::mask(NDVI_raster, mzimvubu_sf)
  NDVI_raster <- crop(NDVI_raster, mzimvubu_extent) # Crop to mzimvubu_extent
  gain(NDVI_raster) <- 0.0001 # Dividing values by 10000 to have NDVI values between -1 and 1
  
  # Save processed raster file
  writeRaster(NDVI_raster, filename = paste0("C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa\\VI_16Days_250m_v6\\Processed_NDVI\\NDVI_", date, ".tif"), format = "GTiff")
}

# Get list of all processed TIFF files
processed_files <- list.files(path = "C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa\\VI_16Days_250m_v6\\Processed_NDVI", pattern = "NDVI_.*\\.tif$", full.names = TRUE)
NDVI_stack <- stack(processed_files)
mean_annual_NDVI <- mean(NDVI_stack) # Calculate mean annual NDVI
writeRaster(mean_annual_NDVI, filename = "C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa\\VI_16Days_250m_v6\\Processed_NDVI\\mean_annual_NDVI.tif", format = "GTiff")


# Converting the mean annual NDVI raster into a dataframe
mean_annual_NDVI_df <- as.data.frame(mean_annual_NDVI, xy = TRUE)
mean_annual_NDVI_df <- na.omit(mean_annual_NDVI_df)# Removing NA values

# Plotting the data
ggplot() +
  geom_raster(
    data = mean_annual_NDVI_df,
    aes(x = x, y = y, fill = layer)  # layer column contains the mean NDVI values
  ) +
  geom_sf(data = mzimvubu_sf, inherit.aes = FALSE, fill = NA) +
  scale_fill_viridis(name = "Mean Annual NDVI") +
  labs(
    title = "Mean Annual NDVI (2022) in Mzimvubu",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()



#### EVI ####

dir.create("C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa\\VI_16Days_250m_v6\\Processed_EVI")
EVI_files <- list.files(path = "C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa\\VI_16Days_250m_v6\\EVI\\", pattern = "MOD13Q1_EVI_.*\\.tif$", full.names = TRUE)

# Loop over each file
for(i in seq_along(EVI_files)) {
  
  date <- gsub(pattern = "^.*MOD13Q1_EVI_(\\d{4}_\\d{3}).*.tif$", replacement = "\\1", x = basename(EVI_files[i]))
  
  # Reading in the downloaded EVI raster data
  EVI_raster <- raster(EVI_files[i])
  EVI_raster <- projectRaster(EVI_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Transforming the data
  EVI_raster <- raster::mask(EVI_raster, mzimvubu_sf)
  EVI_raster <- crop(EVI_raster, mzimvubu_extent) # Crop to mzimvubu_extent
  gain(EVI_raster) <- 0.0001 # Dividing values by 10000 to have EVI values between -1 and 1
  
  # Save processed raster file
  writeRaster(EVI_raster, filename = paste0("C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa\\VI_16Days_250m_v6\\Processed_EVI\\EVI_", date, ".tif"), format = "GTiff")
}

# Get list of all processed TIFF files
processed_EVI_files <- list.files(path = "C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa\\VI_16Days_250m_v6\\Processed_EVI", pattern = "EVI_.*\\.tif$", full.names = TRUE)
EVI_stack <- stack(processed_EVI_files)
mean_annual_EVI <- mean(EVI_stack) # Calculate mean annual EVI
writeRaster(mean_annual_EVI, filename = "C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa\\VI_16Days_250m_v6\\Processed_EVI\\mean_annual_EVI.tif", format = "GTiff")


# Converting the mean annual EVI raster into a dataframe
mean_annual_EVI_df <- as.data.frame(mean_annual_EVI, xy = TRUE)
mean_annual_EVI_df <- na.omit(mean_annual_EVI_df) # Removing NA values

# Plotting the data
ggplot() +
  geom_raster(
    data = mean_annual_EVI_df,
    aes(x = x, y = y, fill = layer)  # layer column contains the mean EVI values
  ) +
  geom_sf(data = mzimvubu_sf, inherit.aes = FALSE, fill = NA) +
  scale_fill_viridis(name = "Mean Annual EVI") +
  labs(
    title = "Mean Annual EVI (2022) in Mzimvubu",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()







# Joining the data
#df_joined <- df %>%
 # left_join(mean_annual_NDVI_df, by = c("Longitude" = "x", "Latitude" = "y"), suffix = c("", ".NDVI")) %>%
  #left_join(mean_annual_EVI_df, by = c("Longitude" = "x", "Latitude" = "y"), suffix = c("", ".EVI"))
