
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

covariates_stack <- terra::rast(raster_files)

  #legacy soil data
soils <- shapefile("C:\\workspace\\mzimvubusoils.shp")
legacy_parameters <-shapefile("C:\\workspace\\covariate\\soils\\parameters_selected.shp")
legacy_lithology <- shapefile("C:\\workspace\\covariate\\soils\\terrain_selected.shp")

soils@data$DOMSOI <- as.factor(soils@data$DOMSOI)
legacy_lithology@data$LITHOLOGY<-as.factor(legacy_lithology@data$LITHOLOGY)
legacy_lithology@data$LANDFORM<-as.factor(legacy_lithology@data$LANDFORM)
str(soils)

#########

covariates_stack <- terra::rast(raster_files)
plot(covariates_stack[[15:25]])

coords <- st_coordinates(ocs_ec_sf$geometry)
ocs_ec_sf$longitude <- coords[, 1]
ocs_ec_sf$latitude <- coords[, 2]


# Extract raster values and soil classes at the locations of the points
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

# Step 2: Remove the specific columns you listed
columns_to_remove <- c('PHASE1', 'ID', 'PHASE2', 'MISCLU1', 
                       'MISCLU2', 'PERMAFROST', "fid",
                       "SNUM", "PHASE1", "PHASE2", "FAOSOIL", "SQKM",
                       "monsterverwysing_sample_reference", "site_name",
                       "lab_no", "CNTCODE" ,"COUNTRY", "treatment", "x1_cf",
                       "soc_g_cm2" ,"soc_kg_cm2", "lat", "long", "c", "soil_depth_m",
                       "klei_clay", "slik_silt", "p_h" , "sand", "CNTNAME", "c_g_kg",
                       "soilbd", "longitude", "latitude")

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
set.seed(183)
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
mzimvubu_sf <- st_read("C:\\workspace\\Kirinyet-development\\data\\mzimvubu_sf.shp") 
new_locations <- st_sample(mzimvubu_sf, size = 100, type = "random")# Generate 100random points within the study area

# Convert the object to an sf object
new_locations_sf <- st_sf(new_locations)
covariates_values_new <- terra::extract(covariates_stack, new_locations_sf)
soil_classes_new <- terra::extract(soils_vect, terra::vect(new_locations_sf))
legacy_parameters_new <-terra::extract(legacy_parameters_vect, terra::vect(new_locations_sf))
legacy_lithology_new <-terra::extract(legacy_lithology_vect, terra::vect(new_locations_sf))
data_for_model_new <- cbind(as.data.frame(covariates_values_new), as.data.frame(soil_classes_new),as.data.frame(legacy_parameters_new),as.data.frame(legacy_lithology_new))

# Identify and remove duplicate columns
unique_cols <- !duplicated(names(data_for_model_new))
data_for_model_unique <- data_for_model_new[, unique_cols]

# Remove the listed columns using dplyr::select
data_for_model_cleaned <- dplyr::select(data_for_model_unique, -c('PHASE1','ID', 'PHASE2', 'MISCLU1', 
                                                                  'MISCLU2', 'PERMAFROST',"id.y","fid", 
                                                                  "SNUM","PHASE1", "PHASE2", "FAOSOIL",
                                                                  "CNTCODE" ,"CNTNAME", "SQKM","COUNTRY",
                                                                  "id.y", "Name", "area_ha", "area_km2"))

data_for_model_cleaned<- janitor::clean_names(data_for_model_new)
data_for_model_cleaned <- na.omit(data_for_model_new)

# Identify columns with NA values
na_cols <- names(data_for_model_cleaned)[colSums(is.na(data_for_model_cleaned)) > 0]

# Now fill those columns with their respective means
for (col in na_cols) {
  if (is.numeric(data_for_model_cleaned[[col]])) {
    missing_value_indices <- which(is.na(data_for_model_cleaned[[col]]))
    data_for_model_cleaned[missing_value_indices, col] <- mean(data_for_model_cleaned[[col]], na.rm = TRUE)
  }
}
data_for_model_cleaned <- data_for_model_cleaned[!is.na(data_for_model_cleaned$domsoi), ]
data_for_model_cleaned <- data_for_model_cleaned[-77]

# Creating a new task with the cleaned data
data_for_model_cleaned$dummy_target <- rep(0, nrow(data_for_model_cleaned))
task_cleaned <- TaskRegr$new(id = "mzimvubu_sf_cleaned", backend = data_for_model_cleaned, target = "dummy_target")

# Predicting on the new task
predictions_cleaned <- learner$predict(task_cleaned)

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


##########################################################################################
set.seed(183)
# XGBOOST
task <- TaskRegr$new(id = "mzimvubu_sf", backend = merged_data, target = "soc_tha")
learner <- lrn("regr.xgboost", nrounds = 100)
po <- po("encode", method = "one-hot")
task = po$train(list(task))[[1]]

train_set <- sample(task$nrow, 0.8 * task$nrow) # 80% of the data
test_set <- setdiff(seq_len(task$nrow), train_set) # the rest 20% of the data

learner$train(task, row_ids = train_set)

predictions <- learner$predict(task, row_ids = test_set)
print(predictions)

predictions$score(msr("regr.mse"))
predictions$score(msr("regr.mae"))
predictions$score(msr("regr.rsq"))


###
# 
terra::plot(template_raster)
plot(new_locations_sf, add=TRUE, col="red", pch=16, cex=0.5)

predictions_new <- learner$predict(task_new)
data_for_model_new$soc_tha <- predictions_new$data$truth
data_for_model_new$soc_tha <- predictions_new$response
