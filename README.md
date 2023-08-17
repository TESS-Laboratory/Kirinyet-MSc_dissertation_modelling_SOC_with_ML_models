# Kirinyet-development
This repository contains R scripts and data used in the modelling of soil organic carbon for the mzimvubu catchment based on other environmental covariates.  
The data folder has study area shapefiles and the soil sample data.  
The DEM_covariates.R is an R script detailing how the terrain data ie slope, TWI were generated

*The ML_soc.R* is a script for preprocessing the soil observations and generating the inputs for the machine learning analysis.

*mlr3.R* is the script with the main analysis on prediction based different algorithmns
vegetation_indices.r, worldclim_data.R and lulc.R, are all scripts used to preprocess and load the different vegetation indices,climatic data and land use and land cover data respectively.
