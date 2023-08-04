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

# Read raster files
raster_files <- c(
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_1.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_2.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_3.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_4.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_5.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_6.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_7.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_8.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_9.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_10.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_11.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_12.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_13.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_14.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_15.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_16.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_17.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_18.tif",
  "C:\\workspace\\covariate\\Bioclim\\Bioclim_data_19.tif",
  "C:\\workspace\\covariate\\DEM_covariates\\resampled_Mzimvubu_TWI.tif",
  "C:\\workspace\\covariate\\DEM_covariates\\resampled_mean_annual_EVI.tif",
  "C:\\workspace\\covariate\\DEM_covariates\\resampled_mean_annual_NDVI.tif",
  "C:\\workspace\\covariate\\DEM_covariates\\resampled_Mzimvubu_Aspect.tif",
  "C:\\workspace\\covariate\\DEM_covariates\\resampled_Mzimvubu_Elevation.tif",
  "C:\\workspace\\covariate\\DEM_covariates\\resampled_Mzimvubu_Slope.tif",
  "C:\\workspace\\covariate\\tmax\\tmax_wc2.1_30s_tmax_12.tif",
  "C:\\workspace\\covariate\\tmax\\tmax_wc2.1_30s_tmax_01.tif",
  "C:\\workspace\\covariate\\tmax\\tmax_wc2.1_30s_tmax_02.tif",
  "C:\\workspace\\covariate\\tmax\\tmax_wc2.1_30s_tmax_03.tif",
  "C:\\workspace\\covariate\\tmax\\tmax_wc2.1_30s_tmax_04.tif",
  "C:\\workspace\\covariate\\tmax\\tmax_wc2.1_30s_tmax_05.tif",
  "C:\\workspace\\covariate\\tmax\\tmax_wc2.1_30s_tmax_06.tif",
  "C:\\workspace\\covariate\\tmax\\tmax_wc2.1_30s_tmax_07.tif",
  "C:\\workspace\\covariate\\tmax\\tmax_wc2.1_30s_tmax_08.tif",
  "C:\\workspace\\covariate\\tmax\\tmax_wc2.1_30s_tmax_09.tif",
  "C:\\workspace\\covariate\\tmax\\tmax_wc2.1_30s_tmax_10.tif",
  "C:\\workspace\\covariate\\tmax\\tmax_wc2.1_30s_tmax_11.tif",
  "C:\\workspace\\covariate\\prec\\prec_wc2.1_30s_prec_03.tif",
  "C:\\workspace\\covariate\\prec\\prec_wc2.1_30s_prec_04.tif",
  "C:\\workspace\\covariate\\prec\\prec_wc2.1_30s_prec_05.tif",
  "C:\\workspace\\covariate\\prec\\prec_wc2.1_30s_prec_06.tif",
  "C:\\workspace\\covariate\\prec\\prec_wc2.1_30s_prec_07.tif",
  "C:\\workspace\\covariate\\prec\\prec_wc2.1_30s_prec_08.tif",
  "C:\\workspace\\covariate\\prec\\prec_wc2.1_30s_prec_09.tif",
  "C:\\workspace\\covariate\\prec\\prec_wc2.1_30s_prec_10.tif",
  "C:\\workspace\\covariate\\prec\\prec_wc2.1_30s_prec_11.tif",
  "C:\\workspace\\covariate\\prec\\prec_wc2.1_30s_prec_12.tif",
  "C:\\workspace\\covariate\\prec\\prec_wc2.1_30s_prec_01.tif",
  "C:\\workspace\\covariate\\prec\\prec_wc2.1_30s_prec_02.tif",
  "C:\\workspace\\covariate\\tmin\\tmin_wc2.1_30s_tmin_12.tif",
  "C:\\workspace\\covariate\\tmin\\tmin_wc2.1_30s_tmin_01.tif",
  "C:\\workspace\\covariate\\tmin\\tmin_wc2.1_30s_tmin_02.tif",
  "C:\\workspace\\covariate\\tmin\\tmin_wc2.1_30s_tmin_03.tif",
  "C:\\workspace\\covariate\\tmin\\tmin_wc2.1_30s_tmin_04.tif",
  "C:\\workspace\\covariate\\tmin\\tmin_wc2.1_30s_tmin_05.tif",
  "C:\\workspace\\covariate\\tmin\\tmin_wc2.1_30s_tmin_06.tif",
  "C:\\workspace\\covariate\\tmin\\tmin_wc2.1_30s_tmin_07.tif",
  "C:\\workspace\\covariate\\tmin\\tmin_wc2.1_30s_tmin_08.tif",
  "C:\\workspace\\covariate\\tmin\\tmin_wc2.1_30s_tmin_09.tif",
  "C:\\workspace\\covariate\\tmin\\tmin_wc2.1_30s_tmin_10.tif",
  "C:\\workspace\\covariate\\tmin\\tmin_wc2.1_30s_tmin_11.tif"
)
  #categorical data
soils <- terra::vect("C:\\workspace\\mzimvubusoils.shp")
plot(soils, "DOMSOI")

lulc<-c(
"C:\\workspace\\covariate\\lulc\\20210101-20220101_merged.tif",
"C:\\workspace\\covariate\\lulc\\20220101-20230101_merged.tif",
"C:\\workspace\\covariate\\lulc\\20170101-20180101_merged.tif",
"C:\\workspace\\covariate\\lulc\\20180101-20190101_merged.tif",
"C:\\workspace\\covariate\\lulc\\20190101-20200101_merged.tif",
"C:\\workspace\\covariate\\lulc\\20200101-20210101_merged.tif"
)

covariates_stack <- terra::rast(raster_files)
plot(covariates_stack[[25:36]])

coords <- st_coordinates(ocs_ec_sf$geometry)
ocs_ec_sf$longitude <- coords[, 1]
ocs_ec_sf$latitude <- coords[, 2]


# Extract raster values and soil classes at the locations of the points
covariates_values <- terra::extract(covariates_stack, ocs_ec_sf)
ocs_ec_vect <- terra::vect(ocs_ec_sf) # Convert sf object to SpatVector
soil_classes <- terra::extract(soils, ocs_ec_vect)# Extract soil classes

data_for_model <- cbind(as.data.frame(covariates_values), as.data.frame(soil_classes))

# Ensure soil type is a factor
data_for_model$DOMSOI <- as.factor(data_for_model$DOMSOI)
merged_data <- cbind(data_for_model, ocs_ec_vect)

# Remove unnecessary columns
merged_data <- dplyr::select(merged_data, -c('PHASE1','ID', 'PHASE2', 'MISCLU1', 'MISCLU2', 'PERMAFROST',"id.y","fid", "SNUM","PHASE1", "PHASE2", "FAOSOIL","SQKM", "monsterverwysing_sample_reference", "site_name"  ,"lab_no","CNTCODE" ,"COUNTRY","treatment" ,"x1_cf" ,"soc_g_cm2" ,"soc_kg_cm2","lat","long","c", "soil_depth_m" ,"klei_clay" ,"slik_silt", "p_h" , "sand","CNTNAME","c_g_kg","soilbd","longitude","latitude"))
merged_data <- merged_data[-c(73, 74, 75, 76), ]
any(is.na(merged_data))

merged_data<- janitor::clean_names(merged_data)


#####################
set.seed(5446)
# Create a Task
task <- TaskRegr$new(id = "mzimvubu_sf", backend = merged_data, target = "soc_tha")

# Preprocess the task using one-hot encoding for factor variables
po <- po("encode", method = "one-hot")
task <- po$train(list(task))[[1]]

# Create Learners
learner_rf <- lrn("regr.ranger", num.trees=500)
learner_xgboost <- lrn("regr.xgboost", nrounds = 500)
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
learner <- lrn("regr.ranger", num.trees=500, importance = "permutation")#Create a Learner

train_set <- sample(task$nrow, 0.8 * task$nrow) # 80% of the data
test_set <- setdiff(seq_len(task$nrow), train_set) # the rest 20% of the data

learner$train(task, row_ids = train_set)#Train the model
predictions <- learner$predict(task, row_ids = test_set)#Predict on the test set
print(predictions)
predictions$score(msr("regr.mse"))

mae <- predictions$score(msr("regr.mae"))
print(paste("MAE:", mae))

rsq <- predictions$score(msr("regr.rsq"))
print(paste("R squared:", rsq))


set.seed(123)  
new_locations <- st_sample(mzimvubu_sf, size = 100, type = "random")# Generate 1000random points within the study area

# Convert the sfc object to an sf object
new_locations_sf <- st_sf(new_locations)
covariates_values_new <- terra::extract(covariates_stack, new_locations_sf)
soil_classes_new <- terra::extract(soils, terra::vect(new_locations_sf))
data_for_model_new <- cbind(as.data.frame(covariates_values_new), as.data.frame(soil_classes_new))
data_for_model_new$DOMSOI <- as.factor(data_for_model_new$DOMSOI)

# Remove specific columns
data_for_model_new <- dplyr::select(data_for_model_new, -c('PHASE1','ID', 'PHASE2', 'MISCLU1', 'MISCLU2', 'PERMAFROST',"id.y","fid", "SNUM","PHASE1", "PHASE2", "FAOSOIL","CNTCODE" ,"CNTNAME", "SQKM","COUNTRY" ))
data_for_model_new<- janitor::clean_names(data_for_model_new)
data_for_model_new <- na.omit(data_for_model_new)

####
data_for_model_new$dummy_target <- rep(0, nrow(data_for_model_new))
task_new <- TaskRegr$new(id = "mzimvubu_sf_new", backend = data_for_model_new, target = "dummy_target")
predictions_new <- learner$predict(task_new)
data_for_model_new$soc_tha <- predictions_new$data$truth

data_for_model_new$soc_tha <- predictions_new$response


####
# Get variable importance
model <- learner$model
importance <- model$variable.importance
print(importance)

# Create a bar plot of importance

importance_df <- as.data.frame(importance)
importance_df$variable <- rownames(importance_df)
names(importance_df)[1] <- "importance"

ggplot(importance_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable", y = "Importance", title = "Variable Importance Plot") +
  theme_minimal()


##########################################################################################

# XGBOOST
task <- TaskRegr$new(id = "mzimvubu_sf", backend = merged_data, target = "soc_tha")
learner <- lrn("regr.xgboost", nrounds = 500)
po <- po("encode", method = "one-hot")
task = po$train(list(task))[[1]]

train_set <- sample(task$nrow, 0.8 * task$nrow) # 80% of the data
test_set <- setdiff(seq_len(task$nrow), train_set) # the rest 20% of the data

learner$train(task, row_ids = train_set)

predictions <- learner$predict(task, row_ids = test_set)
print(predictions)

mse <- predictions$score(msr("regr.mse"))
print(paste("MSE:", mse))

mae <- predictions$score(msr("regr.mae"))
print(paste("MAE:", mae))

rsq <- predictions$score(msr("regr.rsq"))
print(paste("R squared:", rsq))


set.seed(123)
new_locations <- st_sample(mzimvubu_sf, size = 100, type = "random")

new_locations_sf <- st_sf(new_locations)

covariates_values_new <- terra::extract(covariates_stack, new_locations_sf)
soil_classes_new <- terra::extract(soils, terra::vect(new_locations_sf))

data_for_model_new <- cbind(as.data.frame(covariates_values_new), as.data.frame(soil_classes_new))
data_for_model_new$DOMSOI <- as.factor(data_for_model_new$DOMSOI)

data_for_model_new <- dplyr::select(data_for_model_new, -c('PHASE1','ID', 'PHASE2', 'MISCLU1', 'MISCLU2', 'PERMAFROST',"id.y","fid", "SNUM","PHASE1", "PHASE2", "FAOSOIL","CNTCODE" ,"CNTNAME", "SQKM","COUNTRY"))
data_for_model_new<- janitor::clean_names(data_for_model_new)
data_for_model_new <- na.omit(data_for_model_new)


data_for_model_new$dummy_target <- rep(0, nrow(data_for_model_new))
task_new <- TaskRegr$new(id = "mzimvubu_sf_new", backend = data_for_model_new, target = "dummy_target")

# Preprocess the new task using one-hot encoding for factor variables
task_new = po$train(list(task_new))[[1]]

# Predict on new data
predictions_new <- learner$predict(task_new)

# Assign the predicted values to the new data
data_for_model_new$soc_tha <- predictions_new$data$truth
data_for_model_new$soc_tha <- predictions_new$response

# Feature importance can be calculated after model is trained
importance_matrix <- xgboost::xgb.importance(feature_names = task$feature_names, model = learner$model)
print(importance_matrix)






######################################

##resample then replace files into above 
raster_files <- c("C:\\workspace\\covariate\\DEM_covariates\\mean_annual_EVI.tif",
                  "C:\\workspace\\covariate\\DEM_covariates\\mean_annual_NDVI.tif",
                  "C:\\workspace\\covariate\\DEM_covariates\\Mzimvubu_Aspect.tif",
                  "C:\\workspace\\covariate\\DEM_covariates\\Mzimvubu_Elevation.tif",
                  "C:\\workspace\\covariate\\DEM_covariates\\Mzimvubu_Slope.tif",
                  "C:\\workspace\\covariate\\DEM_covariates\\Mzimvubu_TWI.tif")

# target extent an dresolution
output_dir <- "C:\\workspace\\covariate\\DEM_covariates\\"
target_extent <- ref_extent
target_res <- c(0.008333333, 0.008333333)  

target_raster <- terra::rast(extent = target_extent, resolution = target_res)

# Loop over each raster file
for (file in raster_files) {
  rast <- terra::rast(file)
  resampled_rast <- terra::resample(rast, target_raster, method = "bilinear")  # adjust method as needed
  
  # Save the resampled raster
  filename <- paste0("resampled_", tools::file_path_sans_ext(basename(file)), ".tif")
  output_filepath <- file.path(output_dir, filename)
  terra::writeRaster(resampled_rast, output_filepath, overwrite = TRUE)
}


########################################
