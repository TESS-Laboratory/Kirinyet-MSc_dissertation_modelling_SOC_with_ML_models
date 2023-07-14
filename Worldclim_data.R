library(raster)
library(sp)
library(tidyverse)
library(viridis)

# Read the shapefile of the study area and other related data
inpath<-"C:\\workspace\\Kirinyet-development\\data\\CSA_Baseline_SOC\\"
CAA<-st_read(paste0(inpath,"Milcah\\CSA_Conservation_Agreement_Areas.shp"))
CSA_NL <-st_read(paste0(inpath, "Milcah\\CSA_NAtional_Landscapes.shp"))
SOC_locations<-st_read(paste0(inpath, "Milcah\\SOC_GPS_Locations.shp"))

mzimvubu <- CSA_NL[CSA_NL$Name == "Mzimvubu Landscape", ] #subset NLA to mzimvubu
plot(mzimvubu["Name"], main= "Mzimvubu")

mzimvubu_sf <- as(mzimvubu, "sf")

#### temp and rainfall ####
inpath_prec <- "C:\\Users\\milcah\\OneDrive\\EC_project\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_prec_2020-2021"

# Generate a list of file names for the 24 files
file_names <- paste(inpath_prec, "\\wc2.1_2.5m_prec_", 
                    c(paste("2020", sprintf("-%02d", 1:12), sep=""), 
                      paste("2021", sprintf("-%02d", 1:12), sep="")), ".tif", sep="")


raster_list <- list() # Create an empty raster stack

# crop them using the mzimvubu 
for(file in file_names){
  raster_layer <- raster::raster(file)
  cropped_layer <- raster::crop(raster_layer, mzimvubu)
  raster_list <- c(raster_list, list(cropped_layer))
}


raster_stack <- raster::stack(raster_list) # Stack the cropped rasters

plot(raster_stack, 1:4)


####
inpath_tmax<- "C:\\Users\\milcah\\OneDrive\\EC_project\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_tmax_2020-2021"

file_names <- paste(inpath_tmax, "\\wc2.1_2.5m_tmax_", 
                    c(paste("2020", sprintf("-%02d", 1:12), sep=""), 
                      paste("2021", sprintf("-%02d", 1:12), sep="")), ".tif", sep="")

raster_list <- list()

# Loop through the file names, crop them using the mzimvubu polygon, and add them to the list
for(file in file_names){
  raster_layer <- raster::raster(file)
  cropped_layer <- raster::crop(raster_layer, mzimvubu)
  raster_list <- c(raster_list, list(cropped_layer))
}


raster_stack <- raster::stack(raster_list) # Stack the cropped rasters

plot(raster_stack, 1:4)


####
inpath_tmin <- "C:\\Users\\milcah\\OneDrive\\EC_project\\Eastern Cape data\\wc2.1_cruts4.06_2.5m_tmin_2020-2021"

file_names <- paste(inpath_tmin, "\\wc2.1_2.5m_tmin_", 
                    c(paste("2020", sprintf("-%02d", 1:12), sep=""), 
                      paste("2021", sprintf("-%02d", 1:12), sep="")), ".tif", sep="")

raster_list <- list()

for(file in file_names){
  raster_layer <- raster::raster(file)
  cropped_layer <- raster::crop(raster_layer, mzimvubu_sf)
  raster_list <- c(raster_list, list(cropped_layer))
}

raster_stack <- raster::stack(raster_list)

plot(raster_stack, 1:4)

#### Bioclim #### long term averages 1970-2000
inpath_onedrive <- "C:\\Users\\milcah\\OneDrive\\EC_project\\Eastern Cape data"
bioclim_path <- file.path(inpath_onedrive, "covariate", "wc2-5", "bio_2-5m_bil")
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
