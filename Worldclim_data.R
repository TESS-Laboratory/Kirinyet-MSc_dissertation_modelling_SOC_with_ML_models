# This script  details how the climatic dataset was preprocessed
library(raster)
library(sp)
library(tidyverse)
library(viridis)
library(sf)
library(geodata)

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


