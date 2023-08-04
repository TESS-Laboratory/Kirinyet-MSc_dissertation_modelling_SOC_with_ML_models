library(elevatr)
library(viridis)
library(tidyverse)
library(raster)
library(terra)

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



