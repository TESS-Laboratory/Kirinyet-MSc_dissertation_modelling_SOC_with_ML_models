library(raster)
library(sp)
library(tidyverse)
library(viridis)
library(sf)

#### Bioclim #### 
inpath_onedrive <- "C:\\Users\\milcah\\OneDrive\\EC_project\\Eastern Cape data"
bioclim_path <- file.path(inpath_onedrive, "covariate", "wc2.1_30s_bio")
file_list <- list.files(path = bioclim_path, pattern = "\\.tif$", full.names = TRUE)
cropped_rasters <- list()

# crop raster
for(i in 1:length(file_list)) {
  r <- raster(file_list[i])
  r_cropped <- crop(r, extent(mzimvubu_sf))
  r_masked <- mask(r_cropped, mzimvubu_sf)
  cropped_rasters[[i]] <- r_masked
}


bioclim_cropped <- stack(cropped_rasters)
output_folder <- "C:\\workspace\\covariate\\"

# Loop through each layer in the stack
for(i in 1:nlayers(bioclim_cropped)) {
  layer <- bioclim_cropped[[i]]
  output_filename <- file.path(output_folder, paste0("Bioclim_data_", i, ".tif"))# Specify output filename
  writeRaster(layer, filename = output_filename, format = "GTiff")# Write the layer to a file
}

# Plotting
layers_to_plot <- 1:nlayers(bioclim_cropped) #plot all layers
n_row <- ceiling(sqrt(length(layers_to_plot)))
n_col <- ceiling(length(layers_to_plot) / n_row)

# Adjust margins
par(mfrow = c(n_row, n_col), mar = c(2, 2, 2, 2))

# Plotting each layer
for (i in layers_to_plot) {
  plot(bioclim_cropped[[i]], main = names(bioclim_cropped)[i])
}


#### temp and rainfall ####
#monthly precipitation
prec_path <- "C:\\Users\\milcah\\OneDrive\\EC_project\\Eastern Cape data\\covariate\\wc2.1_30s_prec"
output_folder <- "C:\\workspace\\covariate\\prec"
file_list <- list.files(path = prec_path, pattern = "\\.tif$", full.names = TRUE)

cropped_rasters <- list()

# Loop through each file in the list, crop and mask the raster
for(i in 1:length(file_list)) {
  r <- raster(file_list[i])
  r_cropped <- crop(r, extent(mzimvubu_sf))
  r_masked <- mask(r_cropped, mzimvubu_sf)
  
  output_filename <- file.path(output_folder, paste0("prec_", basename(file_list[i])))
  writeRaster(r_masked, filename = output_filename, format = "GTiff")
  
  # Add to the list of cropped rasters
  cropped_rasters[[i]] <- r_masked
}


stacked_rasters <- stack(cropped_rasters)
brick_rasters <- brick(stacked_rasters)
writeRaster(brick_rasters, filename = file.path(output_folder, "Stacked_prec_rasters.tif"), format = "GTiff")

# Plot each layer in the brick
for(i in 1:nlayers(brick_rasters)) {
  plot(brick_rasters[[i]], main = paste("Layer", i))
}


# Maximum temperature
tmax_path <- "C:\\Users\\milcah\\OneDrive\\EC_project\\Eastern Cape data\\covariate\\wc2.1_30s_tmax"
output_folder <- "C:\\workspace\\covariate\\tmax"
file_list <- list.files(path = tmax_path, pattern = "\\.tif$", full.names = TRUE)

cropped_rasters <- list()

#  crop and mask each raster
for(i in 1:length(file_list)) {
  r <- raster(file_list[i])
  r_cropped <- crop(r, extent(mzimvubu_sf))
  r_masked <- mask(r_cropped, mzimvubu_sf)
  
  # Save the masked and cropped raster to file
  output_filename <- file.path(output_folder, paste0("tmax_", basename(file_list[i])))
  writeRaster(r_masked, filename = output_filename, format = "GTiff")
  cropped_rasters[[i]] <- r_masked
}

# Stack the rasters
stacked_rasters <- stack(cropped_rasters)
brick_rasters <- brick(stacked_rasters)
writeRaster(brick_rasters, filename = file.path(output_folder, "Stacked_tmax_rasters.tif"), format = "GTiff")

# Plot each layer in the brick
for(i in 1:nlayers(brick_rasters)) {
  plot(brick_rasters[[i]], main = paste("Layer", i))
}


# Minimum monthly temperature
tmin_path <- "C:\\Users\\milcah\\OneDrive\\EC_project\\Eastern Cape data\\covariate\\wc2.1_30s_tmin"
output_folder <- "C:\\workspace\\covariate\\tmin"
file_list <- list.files(path = tmin_path, pattern = "\\.tif$", full.names = TRUE)

cropped_rasters <- list()

# Loop through each file in the list, crop and mask the raster
for(i in 1:length(file_list)) {
  r <- raster(file_list[i])
  r_cropped <- crop(r, extent(mzimvubu_sf))
  r_masked <- mask(r_cropped, mzimvubu_sf)
  
  # Save the masked and cropped raster to file
  output_filename <- file.path(output_folder, paste0("tmin_", basename(file_list[i])))
  writeRaster(r_masked, filename = output_filename, format = "GTiff")
  
  # Add to the list of cropped rasters
  cropped_rasters[[i]] <- r_masked
}

# Stack the rasters
stacked_rasters <- stack(cropped_rasters)
brick_rasters <- brick(stacked_rasters)
writeRaster(brick_rasters, filename = file.path(output_folder, "Stacked_tmin_rasters.tif"), format = "GTiff")

# Plot each layer in the brick
for(i in 1:nlayers(brick_rasters)) {
  plot(brick_rasters[[i]], main = paste("Layer", i))
}
