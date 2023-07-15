library(elevatr)
library(viridis)
library(tidyverse)
library(raster)
library(terra)

# Read the shapefile of the study area and other related data
inpath<-"C:\\workspace\\Kirinyet-development\\data\\CSA_Baseline_SOC\\"
CAA<-st_read(paste0(inpath,"Milcah\\CSA_Conservation_Agreement_Areas.shp"))
CSA_NL <-st_read(paste0(inpath, "Milcah\\CSA_NAtional_Landscapes.shp"))
SOC_locations<-st_read(paste0(inpath, "Milcah\\SOC_GPS_Locations.shp"))

mzimvubu <- CSA_NL[CSA_NL$Name == "Mzimvubu Landscape", ] #subset NLA to mzimvubu
plot(mzimvubu["Name"], main= "Mzimvubu")

mzimvubu_sf <- as(mzimvubu, "sf")
#### Elevation,Apect,TWI,Slope ####
#https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html
south_africa_sp <- rgdal::readOGR("C:\\workspace\\Kirinyet-development\\ndvi_images\\South Africa.shp") # larger extent is better
# Get elevation data for South Africa
elevation <- elevatr::get_elev_raster(locations = south_africa_sp, z = 10)
elevation_cropped <- crop(elevation, extent(mzimvubu_sf))
elevation_masked <- mask(elevation_cropped, mzimvubu_sf)

# Compute slope and aspect
slope <- terrain(elevation_masked, opt='slope', unit='degrees')
aspect <- terrain(elevation_masked, opt='aspect',unit='degrees')

# Calculate Topographic Wetness Index (TWI)
slope_radians <- terrain(elevation_masked, opt = "slope", unit = "radians")
area <- terrain(elevation_masked, opt = "TPI")
area <- exp(area)
twi <- log(area / tan(slope_radians))

# Stack the rasters
stacked_rasters <- stack(elevation_masked, slope, aspect, twi)
names(stacked_rasters) <- c("Elevation", "Slope", "Aspect", "TWI")
output_folder <- "C:\\workspace\\covariate\\"

# Define file pattern
filename_pattern <- "C:\\workspace\\covariate\\Mzimvubu_%s.tif"

# Write the rasters to files
for(i in 1:nlayers(stacked_rasters)) {
  layer <- stacked_rasters[[i]]
  output_filename <- sprintf(filename_pattern, names(stacked_rasters)[i])
  
  # Write the layer to a file
  writeRaster(layer, filename = output_filename, format = "GTiff")
}

# Plotting
par(mfrow = c(2, 2))
plot(elevation_masked, main="DEM (meters) for mzimvubu")
plot(slope, main="Slope for mzimvubu (degrees)", col=topo.colors(6,alpha=0.6))
plot(aspect, main="Aspect for mzimvubu (degrees)", col=topo.colors(6,alpha=0.6))
plot(twi, main="Topographic Wetness Index for mzimvubu", col=topo.colors(6, alpha=0.6))



