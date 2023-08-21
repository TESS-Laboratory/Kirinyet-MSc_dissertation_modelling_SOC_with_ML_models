
## This script details the fitting of the different learners to predict soil carbon in tha based on the covariates detailed. This is still work in progress
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3pipelines)
library(terra)
library(mlr3spatiotempcv)
library(raster)
library(tidyverse)
library(rasterVis)
library(viridis)
library(gstat)
library(sp)
library(mlr3viz)
library(terra)

# Read raster files
raster_files <- c(
  "Covariates\\Bioclim\\Bioclim_data_2.tif",
  "Covariates\\Bioclim\\Bioclim_data_3.tif",
  "Covariates\\Bioclim\\Bioclim_data_4.tif",
  "Covariates\\Bioclim\\Bioclim_data_5.tif",
  "Covariates\\Bioclim\\Bioclim_data_6.tif",
  "Covariates\\Bioclim\\Bioclim_data_7.tif",
  "Covariates\\Bioclim\\Bioclim_data_8.tif",
  "Covariates\\Bioclim\\Bioclim_data_9.tif",
  "Covariates\\Bioclim\\Bioclim_data_10.tif",
  "Covariates\\Bioclim\\Bioclim_data_11.tif",
  "Covariates\\Bioclim\\Bioclim_data_12.tif",
  "Covariates\\Bioclim\\Bioclim_data_13.tif",
  "Covariates\\Bioclim\\Bioclim_data_14.tif",
  "Covariates\\Bioclim\\Bioclim_data_15.tif",
  "Covariates\\Bioclim\\Bioclim_data_16.tif",
  "Covariates\\Bioclim\\Bioclim_data_17.tif",
  "Covariates\\Bioclim\\Bioclim_data_18.tif",
  "Covariates\\Bioclim\\Bioclim_data_19.tif",
  "Covariates\\Bioclim\\Bioclim_data_1.tif",
  "Covariates\\DEM_covariates\\resampled_Mzimvubu_TWI.tif",
  "Covariates\\DEM_covariates\\resampled_mean_annual_EVI.tif",
  "Covariates\\DEM_covariates\\resampled_mean_annual_NDVI.tif",
  "Covariates\\DEM_covariates\\resampled_Mzimvubu_Aspect.tif",
  "Covariates\\DEM_covariates\\resampled_Mzimvubu_Elevation.tif",
  "Covariates\\DEM_covariates\\resampled_Mzimvubu_Slope.tif"
  
)

covariates_stack <- terra::rast(raster_files)
plot(covariates_stack[[17:25]])
#legacy soil data
soils <- shapefile("data\\mzimvubusoils.shp")
legacy_parameters <-shapefile("Covariates\\soils\\parameters_selected.shp")
legacy_lithology <- shapefile("Covariates\\soils\\terrain_selected.shp")

soils@data$DOMSOI <- as.factor(soils@data$DOMSOI)
legacy_lithology@data$LITHOLOGY<-as.factor(legacy_lithology@data$LITHOLOGY)
legacy_lithology@data$LANDFORM<-as.factor(legacy_lithology@data$LANDFORM)
#str(soils)

#########
coords <- st_coordinates(ocs_ec_sf$geometry)
ocs_ec_sf$longitude <- coords[, 1]
ocs_ec_sf$latitude <- coords[, 2]

covariates_values <- terra::extract(covariates_stack, ocs_ec_sf)
ocs_ec_vect <- terra::vect(ocs_ec_sf) # Convert sf object to SpatVector
soils_vect <- terra::vect(soils)
soil_classes <- terra::extract(soils_vect, ocs_ec_vect)
legacy_parameters_vect <- terra::vect(legacy_parameters)
legacy_param <- terra::extract(legacy_parameters_vect, ocs_ec_vect)
legacy_lithology_vect<- terra::vect(legacy_lithology)
legacy_litho<- terra::extract(legacy_lithology_vect,ocs_ec_vect)
data_for_model <- cbind(as.data.frame(covariates_values), as.data.frame(soil_classes), as.data.frame(legacy_param),as.data.frame(legacy_litho))

merged_data <- cbind(data_for_model, ocs_ec_vect)

dup_cols <- names(merged_data)[duplicated(names(merged_data))]
merged_data <- merged_data[!names(merged_data) %in% dup_cols]

# Remove the specific columns you listed
columns_to_remove <- c('PHASE1', 'ID', 'PHASE2', 'MISCLU1', 
                       'MISCLU2', 'PERMAFROST', "fid",
                       "SNUM", "PHASE1", "PHASE2", "FAOSOIL", "SQKM",
                       "monsterverwysing_sample_reference", "site_name",
                       "lab_no", "CNTCODE" ,"COUNTRY", "treatment", "x1_cf",
                       "soc_g_cm2" ,"soc_kg_cm2", "lat", "long", "c", "soil_depth_m",
                       "klei_clay", "slik_silt", "p_h" , "sand", "CNTNAME", "c_g_kg",
                       "soilbd", "longitude", "latitude","TOTC")

merged_data <- merged_data %>% dplyr::select(-all_of(columns_to_remove))
merged_data <- merged_data[-c(73, 74, 75, 76), ]
any(is.na(merged_data))
merged_data<- janitor::clean_names(merged_data)


#####################
set.seed(533)
# Create a Task
task <- TaskRegr$new(id = "spatial_task", backend = merged_data, target = "soc_tha")
po <- po("encode", method = "one-hot")
task <- po$train(list(task))[[1]]

# Create Learners
learner_rf <- lrn("regr.ranger", num.trees=100)
learner_xgboost <- lrn("regr.xgboost", nrounds = 100)
learner_svm <- lrn("regr.svm")

# resampling instance for 3-fold cross-validation
resampling <- rsmp("cv", folds = 3)

# Create the benchmark design
design <- benchmark_grid(
  tasks = task,
  learners = list(learner_rf, learner_xgboost, learner_svm),
  resamplings = resampling
)

bmr <- benchmark(design)# Run the benchmark
print(bmr)

# results
bmr_aggr <- bmr$aggregate(msrs(c("regr.mse", "regr.mae", "regr.rsq")))
print(bmr_aggr)
autoplot(bmr, type = "boxplot")


##################################
#random forest
# Step 1: Create a Task
task <- TaskRegr$new(id = "mzimvubu_sf", backend = merged_data, target = "soc_tha")
learner <- lrn("regr.ranger", num.trees=100, importance = "permutation")#Create a Learner

train_set <- sample(task$nrow, 0.8 * task$nrow) # 80% of the data
test_set <- setdiff(seq_len(task$nrow), train_set) # the rest 20% of the data

learner$train(task, row_ids = train_set)#Train the model
predictions <- learner$predict(task, row_ids = test_set)#Predict on the test set
print(predictions)

predictions$score(msr("regr.mse"))
predictions$score(msr("regr.mae"))
predictions$score(msr("regr.rsq"))

#new random points within the study area
mzimvubu_sf <- st_read("data\\mzimvubu_sf.shp") 
new_locations <- st_sample(mzimvubu_sf, size = 100, type = "random")

# Convert the object to an sf object
new_locations_sf <- st_sf(new_locations)
covariates_values_new <- terra::extract(covariates_stack, new_locations_sf)
soil_classes_new <- terra::extract(soils_vect, terra::vect(new_locations_sf))
legacy_parameters_new <-terra::extract(legacy_parameters_vect, terra::vect(new_locations_sf))
legacy_lithology_new <-terra::extract(legacy_lithology_vect, terra::vect(new_locations_sf))

data_for_model_new <- new_data_for_model <- cbind(
  as.data.frame(covariates_values_new),
  as.data.frame(soil_classes_new),
  as.data.frame(legacy_parameters_new),
  as.data.frame(legacy_lithology_new)
)


new_merged_data <- cbind(data_for_model_new, new_locations_sf)
new_merged_data <- new_merged_data[!names(new_merged_data) %in% dup_cols]
new_merged_data <- janitor::clean_names(new_merged_data)
required_cols <- setdiff(colnames(merged_data), "soc_tha")
new_merged_data <- new_merged_data[, required_cols]

# Replace NAs with the mean of the respective column for each column
new_merged_data <- as.data.frame(lapply(new_merged_data, function(x) {
  if (is.numeric(x) && any(is.na(x))) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
}))

any(is.na(new_merged_data))

predictions <- learner$predict_newdata(newdata = new_merged_data)
print(predictions)

####
# Get variable importance
model <- learner$model
importance <- model$variable.importance
#print(importance)

# Create a bar plot of importance
importance_df <- as.data.frame(importance)
importance_df$variable <- rownames(importance_df)
names(importance_df)[1] <- "importance"

ggplot(importance_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable", y = "Importance", title = "Variable Importance Plot") +
  theme_minimal()


new_locations_sf$response <- predictions$response

#ggplot(data = new_locations_sf) +
  #geom_sf(aes(color = response), size = 3) +
  #scale_color_viridis_c() +
  #theme_minimal() +
  #labs(title = "Soil Carbon Predictions for New Locations", 
      # color = "SOC (t/ha)")

######IDW interporlation

coords <- as.matrix(st_coordinates(new_locations_sf))
data <- as.data.frame(new_locations_sf)
proj <- CRS(as.character(st_crs(new_locations_sf)$proj4string))
new_locations_sp <- SpatialPointsDataFrame(coords, data, proj4string = proj)

bbox <- bbox(new_locations_sp)
cs <- 0.008333333 # Define the cell size (adjust based on your data)
grid <- expand.grid(x = seq(from = bbox[1, 1], to = bbox[1, 2], by = cs), 
                    y = seq(from = bbox[2, 1], to = bbox[2, 2], by = cs))

gridded_data <- SpatialPixelsDataFrame(points = grid, 
                                       data = data.frame(id = 1:nrow(grid)), 
                                       proj4string = CRS(proj4string(new_locations_sp)))

# Specify the target grid (here based on the bounding box of your points)
x.range <- range(new_locations_sp@coords[,1])
y.range <- range(new_locations_sp@coords[,2])
grid <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = cs), 
                    y = seq(from = y.range[1], to = y.range[2], by = cs))
gridded_data <- SpatialPixelsDataFrame(points = grid, 
                                       data = data.frame(id = 1:nrow(grid)), 
                                       proj4string = CRS(proj4string(new_locations_sp)))

idw_output <- idw(formula = response ~ 1, locations = new_locations_sp, newdata = gridded_data, idp = 2.0)
mzimvubu_sp <- as(mzimvubu_sf, "Spatial")
idw_raster <- raster(idw_output)
masked_raster <- mask(idw_raster, mzimvubu_sp)
plot(masked_raster, main = "Predicted soil organic carbon in tons per hactare")


##################################################
#Just for comparison if the model improves on 80-20 
# Train the XGBoost model
graph <- po("encode", method = "one-hot", affect_columns = selector_type("factor"))
task <- graph$train(list(task))[[1]]

learner_xgb$train(task, row_ids = train_set)

predictions_xgb <- learner_xgb$predict(task, row_ids = test_set)

predictions_xgb$score(msr("regr.mse"))
predictions_xgb$score(msr("regr.mae"))
predictions_xgb$score(msr("regr.rsq"))

#################
# Train the SVM model
learner_svm <- lrn("regr.svm")
learner_svm$train(task, row_ids = train_set)
predictions_svm <- learner_svm$predict(task, row_ids = test_set)

# Compute MSE and MAE
 predictions_svm$score(msr("regr.mse"))
 predictions_svm$score(msr("regr.mae"))
predictions_svm$score(msr("regr.rsq"))
