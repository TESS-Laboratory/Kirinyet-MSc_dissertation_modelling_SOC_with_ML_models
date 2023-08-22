# The data in this script were downloaded from ESRI and preprocessed in QGIS however, the merging and reprojecting to WGS48
# caused a mismatch in the the extraction to the extent of mzimvubu boundaries, the data here were not include in the modelling though 
#they were used for visual examination of land use change. No significant difference was noted for the data period 2017-2022
library(terra)
library(raster)


#### LULC ####
inpath <- "C:/Users/milcah/OneDrive/EC_project/Eastern Cape data/LULC"
filenames <- c(
  "20170101-20180101_merged.tif",
  "20180101-20190101_merged.tif",
  "20190101-20200101_merged.tif",
  "20200101-20210101_merged.tif",
  "20210101-20220101_merged.tif",
  "20220101-20230101_merged.tif"
  
)

# Load and plot each file
for (filename in filenames) {
  file_path <- file.path(inpath, filename)
  raster_data <- raster(file_path)
  plot(raster_data, main = filename)
}

par(mfrow = c(2, 3))


for (filename in filenames) {
  file_path <- file.path(inpath, filename)
  raster_data <- raster(file_path)
  plot(raster_data, main = filename)
}

####################
