# Kirinyet-development
This repository contains R scripts and data used in the modelling of soil organic carbon for the Umzimvubu catchment based on other environmental covariates presented for the MSc in Applied Data Science (Environment and Sustainability) by Milcah Kirinyet (mk767@exeter.ac.uk) at the University of Exeter; dissertation project titled "utilizing Machine Learning to map soil organic carbon in Rangelands: A case study of Umzimvubu, Eastern Cape, South Africa" in partnership with Conservation South Africa.  

The folder **data** contains study area shapefiles and the soil sample data.  
The **DEM_covariates.R** is an R script detailing how the terrain data ie slope, TWI were generated
The **ML_soc.R** is a script for preprocessing the soil observations and generating the legacy soil parameter inputs for the machine learning analysis.
The **Vegetation_indices.R** is a script detailing how the anual NDVI and EVI indices were generated
The **mlr3.R** is the script with the main analysis on prediction based different algorithmns and how the models were evaluated.
The **worldclim_data.R** is the script showing how the 19 bioclimatic indicators from WorldClim were preprocessed
 lulc.R,Msavi.R are scripts used to preprocess and load the different land use and land cover data and preprocessing of the MSAVI index respectively.
