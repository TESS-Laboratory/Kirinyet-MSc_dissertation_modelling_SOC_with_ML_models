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

mzimvubu_extent <- raster::extent(mzimvubu)
print(mzimvubu_extent)
extent <- list(xmin = 28.20686, 
               xmax = 30.19472, 
               ymin = -31.29833, 
               ymax = -30.0018)
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


#resample the temporal distribution from 16day to annual mean
# Set file path
filepath <- "C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa\\VI_16Days_250m_v6\\NDVI\\"
file_list <- list.files(path = filepath, pattern = "*.tif$", full.names = TRUE)

# Read and crop each raster file
raster_list <- map(file_list, ~ {
  r <- raster(.x)
  r <- projectRaster(r, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  r <- raster::mask(r, mzimvubu_sf)
  gain(r) <- 0.0001
  r
})

# Stack all rasters
raster_stack <- stack(raster_list)
annual_mean_raster <- calc(raster_stack, fun = mean, na.rm = TRUE)# Calculate annual mean raster


writeRaster(annual_mean_raster, filename = "annual_mean_raster.tif", format = "GTiff", overwrite = TRUE)# Save the annual mean raster

# Convert the annual mean raster to a dataframe for visualization
annual_mean_df <- as.data.frame(annual_mean_raster, xy = TRUE)
colnames(annual_mean_df) <- c("x", "y", "annual_mean_ndvi")

# Visualize the annual mean raster
ggplot() +
  geom_raster(data = annual_mean_df, aes(x = x, y = y, fill = annual_mean_ndvi)) +
  scale_fill_viridis_c(name = "Annual mean NDVI") +
  labs(
    title = "Annual mean NDVI in Mzimvubu",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()
