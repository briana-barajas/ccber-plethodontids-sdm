## ===================================================================
##                            Load Packages                       ----
## ===================================================================

rm(list = ls())

renv::activate()

# wrangling
library(tidyverse)
library(here)
library(janitor)

# spatial
library(sf)
library(tmap)
library(terra)

# modeling
library(SDMtune)
library(rsample)
library(rJava)

# source functions
source(here("functions", "sdmtune-brt-function.R"))
source(here("functions", "sdmtune-maxent-function.R"))

# set output CSV directory
output_dir <- here("results")


## ===================================================================
##               BRT Modeling - All & Reduced Variables           ----
## ===================================================================

# ......................preparation...................

# set data directory
rast_dir <- here("data", "raster-stacks", "all-variables")
point_dir <- here("data", "CampRoberts_spatial_data")

# create empty df
brt_all_predictions <- data.frame()


# ................run all & reduced BRT...............
for (i in seq(1:8)) {
  
  print(paste0(" ========== BRT All & Reduced Variables - Plot ", i, " =========="))
  
  brt_res <- tune_brt(plot_number = i,
                      point_dir = point_dir,
                      rast_dir = rast_dir,
                      include_variables = "both",
                      pres_abs = FALSE)
  
  # ...............store results as vars................
  brt_p_coords <- brt_res[["p_coords"]]
  brt_a_coords <- brt_res[["a_coords"]]
  brt_pred_stack <- brt_res[["brt_pred_stack"]]
  brt_gs_all <- brt_res[["brt_gs_all"]]
  brt_gs_reduced <- brt_res[["brt_gs_reduced"]]
  brt_val <- brt_res[["brt_val"]] #2025-10-22 only one val data set
  brt_test <- brt_res[["brt_test"]]
  
  
  # .................select best models.................
  #index of top performing model by AUC 2025-10-22
  brt_id_all <- which.max(brt_gs_all@results$test_AUC)
  brt_id_reduced <- which.max(brt_gs_reduced@results$test_AUC)
  
  # Create new training data set using variables in best performing model 2025-10-22
  brt_train_all <- brt_gs_all@models[[brt_id_all]]@data 
  brt_train_reduced <- brt_gs_reduced@models[[brt_id_reduced]]@data 
  
  # Merge validation data with filtered train data 2025-10-22
  brt_train_all <- mergeSWD(brt_train_all, brt_val, only_presence = TRUE)
  brt_train_reduced <- mergeSWD(brt_train_reduced, brt_val, only_presence = TRUE)
  
  # .............generate & save predictions............
  #2025-10-22 train model on combined validation and training data
  brt_best_mod_all <- train(method = "BRT", 
                            progress = FALSE,
                            data = brt_train_all,
                            distribution = brt_gs_all@results[brt_id_all, 1],
                            n.trees = brt_gs_all@results[brt_id_all, 2],
                            interaction.depth = brt_gs_all@results[brt_id_all, 3],
                            shrinkage = brt_gs_all@results[brt_id_all, 4],
                            bag.fraction = brt_gs_all@results[brt_id_all, 5]
                            )
  

  brt_best_mod_reduced <- train(method = "BRT", 
                                progress = FALSE,
                                data = brt_train_reduced,
                                distribution = brt_gs_all@results[brt_id_all, 1],
                                n.trees = brt_gs_all@results[brt_id_all, 2],
                                interaction.depth = brt_gs_all@results[brt_id_all, 3],
                                shrinkage = brt_gs_all@results[brt_id_all, 4],
                                bag.fraction = brt_gs_all@results[brt_id_all, 5]
                                )

  
  # pull AUC of model using TEST data
  brt_auc_all <- auc(brt_best_mod_all, test=brt_test)
  brt_auc_reduced <- auc(brt_best_mod_reduced, test=brt_test)
  
  
  # .............generate & save predictions............
  brt_map_all <- predict(brt_best_mod_all, data = brt_pred_stack)
  brt_map_reduced <- predict(brt_best_mod_reduced, data = brt_pred_stack)
  
  writeRaster(brt_map_all, paste0(output_dir, "/maps-all/brt_p_p", i, ".tif"))
  writeRaster(brt_map_reduced, paste0(output_dir, "/maps-reduced/brt_p_p", i, ".tif"))
  
  rm(brt_res, brt_id_all, brt_best_mod_all, brt_id_reduced, brt_best_mod_reduced)
  
  
  # ...............isolate all predictions..............
  pres_prediction_all <- extract(brt_map_all, brt_p_coords, xy = TRUE, ID = FALSE) %>%
    rename(prediction = lyr1) %>%
    mutate(pres_abs = 1,
           variables = "all")
  
  abs_prediction_all <- extract(brt_map_all, brt_a_coords, xy = TRUE, ID = FALSE) %>%
    rename(prediction = lyr1) %>%
    mutate(pres_abs = 0,
           variables = "all")
  
  # .............isolate reduced predictions............
  pres_prediction_reduced <- extract(brt_map_reduced, brt_p_coords, xy = TRUE, ID = FALSE) %>%
    rename(prediction = lyr1) %>%
    mutate(pres_abs = 1,
           variables = "reduced")
  
  abs_prediction_reduced <- extract(brt_map_reduced, brt_a_coords, xy = TRUE, ID = FALSE) %>%
    rename(prediction = lyr1) %>%
    mutate(pres_abs = 0,
           variables = "reduced")
  
  rm(brt_p_coords, brt_a_coords, brt_map_reduced, brt_map_all)
  
  # ....................combine results.................
  combined_predictions <- rbind(pres_prediction_all, abs_prediction_all, pres_prediction_reduced, abs_prediction_reduced) %>%
    mutate(plot = i,
           test_auc = ifelse(variables == "all", brt_auc_all, brt_auc_reduced))
  
  brt_all_predictions <- rbind(brt_all_predictions, combined_predictions)
  
  rm(combined_predictions, pres_prediction_all, abs_prediction_all, pres_prediction_reduced, abs_prediction_reduced)
  
} # END BRT all & reduced loop


## ===================================================================
##                    BRT Modeling - Select Variables             ----
## ===================================================================

# ......................preparation...................

# set data directory
rast_dir <- here("data", "raster-stacks", "select-variables")
point_dir <- here("data", "CampRoberts_spatial_data")


# ................Run model w/ select variables.......
for (i in seq(1:8)) {
  
  print(paste0(" ========== BRT Select Variables - Plot ", i, " =========="))
  
  brt_res <- tune_brt(plot_number = i,
                      point_dir = point_dir,
                      rast_dir = rast_dir,
                      include_variables = "all",
                      pres_abs = FALSE)
  
  # ...............store results as vars................
  brt_p_coords <- brt_res[["p_coords"]]
  brt_a_coords <- brt_res[["a_coords"]]
  brt_pred_stack <- brt_res[["brt_pred_stack"]]
  brt_gs_all <- brt_res[["brt_gs_all"]]
  brt_val <- brt_res[["brt_val"]] #2025-10-22 only one val data set
  brt_test <- brt_res[["brt_test"]]
  
  # .................select best models.................
  # brt_id_all <- which.max(brt_gs_all@results$test_AUC)
  # brt_auc_all <- max(brt_gs_all@results$test_AUC)
  # brt_best_mod_all <- combineCV(brt_gs_all@models[[brt_id_all]])
  
  #index of top performing model by AUC 2025-10-22
  brt_id_all <- which.max(brt_gs_all@results$test_AUC)

  # Create new training data set using variables in best performing model 2025-10-22
  brt_train_all <- brt_gs_all@models[[brt_id_all]]@data 
  
  # Merge validation data with filtered train data 2025-10-22
  brt_train_all <- mergeSWD(brt_train_all, brt_val, only_presence = TRUE)
  
  # .............generate & save predictions............
  
  #2025-10-22 train model on combined validation and training data
  brt_best_mod_all <- train(method = "BRT", 
                            progress = FALSE,
                            data = brt_train_all,
                            distribution = brt_gs_all@results[brt_id_all, 1],
                            bag.fraction = brt_gs_all@results[brt_id_all, 2])
  
  # pull AUC of model using TEST data
  brt_auc_all <- auc(brt_best_mod_all, test=brt_test)
  
  # create and save prediction maps
  brt_map_all <- predict(brt_best_mod_all, data = brt_pred_stack)
  
  writeRaster(brt_map_all, paste0(output_dir, "/maps-select/brt_p_p", i, ".tif"))

  rm(brt_res, brt_id_all, brt_best_mod_all)
  
  # ...............isolate all predictions..............
  pres_prediction_all <- extract(brt_map_all, brt_p_coords, xy = TRUE, ID = FALSE) %>%
    rename(prediction = lyr1) %>%
    mutate(pres_abs = 1,
           variables = "select")
  
  abs_prediction_all <- extract(brt_map_all, brt_a_coords, xy = TRUE, ID = FALSE) %>%
    rename(prediction = lyr1) %>%
    mutate(pres_abs = 0,
           variables = "select")
  
  rm(brt_p_coords, brt_a_coords, brt_map_all)
  
  # ....................combine results.................
  combined_predictions <- rbind(pres_prediction_all, abs_prediction_all) %>%
    mutate(plot = i,
           test_auc = brt_auc_all)
  
  brt_all_predictions <- rbind(brt_all_predictions, combined_predictions)
  
  rm(combined_predictions, pres_prediction_all, abs_prediction_all, brt_gs_all, brt_pred_stack)
  
} # END BRT select variable loop


# ..................write CSV results ................
write_csv(brt_all_predictions, here(output_dir, "brt_p_results.csv"))

## ===================================================================
##               MaxEnt Modeling - All & Reduced Variables        ----
## ===================================================================

# clear environment
rm(list=ls()[! ls() %in% c("output_dir","tune_maxent")])

# ......................preparation...................

# set data directory
rast_dir <- here("data", "raster-stacks", "all-variables")
point_dir <- here("data", "CampRoberts_spatial_data")

# create empty df
maxent_all_predictions <- data.frame()

# ................run all & reduced Maxent............
for (i in seq(1:8)) {
  
  print(paste0(" ========== Maxent All & Reduced Variables - Plot ", i, " =========="))
  
  maxent_res <- tune_maxent(plot_number = i,
                            point_dir = point_dir,
                            rast_dir = rast_dir,
                            include_variables = "both")
  
  # ...............store results as vars................
  maxent_p_coords <- maxent_res[["p_coords"]]
  maxent_bg_coords <- maxent_res[["bg_coords"]]
  maxent_pred_stack <- maxent_res[["maxent_pred_stack"]]
  maxent_gs_all <- maxent_res[["maxent_gs_all"]]
  maxent_gs_reduced <- maxent_res[["maxent_gs_reduced"]]
  maxent_val <- maxent_res[["maxent_val"]]
  maxent_test <- maxent_res[["maxent_test"]]
  
  # .................select best models.................
  #index of top performing model by AUC 2025-10-22
  maxent_id_all <- which.max(maxent_gs_all@results$test_AUC)
  maxent_id_reduced <- which.max(maxent_gs_reduced@results$test_AUC)
  
  # Create new training data set using variables in best performing model 2025-10-22
  maxent_train_all <- maxent_gs_all@models[[maxent_id_all]]@data 
  maxent_train_reduced <- maxent_gs_reduced@models[[maxent_id_reduced]]@data 
  
  # Merge validation data with filtered train data 2025-10-22
  maxent_train_all <- mergeSWD(maxent_train_all, maxent_val, only_presence = TRUE)
  maxent_train_reduced <- mergeSWD(maxent_train_reduced, maxent_val, only_presence = TRUE)
  
  
  
  # .............generate & save predictions............
  
  #2025-10-22 train model on combined validation and training data
  maxent_best_mod_all <- train(method = "Maxent", 
                            progress = FALSE,
                            data = maxent_train_all,
                            reg = maxent_gs_all@results[maxent_id_all, 2],
                            fc = maxent_gs_all@results[maxent_id_all, 1]
  )
  
  maxent_best_mod_reduced <- train(method = "Maxent", 
                                progress = FALSE,
                                data = maxent_train_reduced,
                                reg = maxent_gs_reduced@results[maxent_id_all, 2],
                                fc = maxent_gs_reduced@results[maxent_id_all, 1]
  )
  
  # pull AUC of model using TEST data
  maxent_auc_all <- auc(maxent_best_mod_all, test=maxent_test)
  maxent_auc_reduced <- auc(maxent_best_mod_reduced, test=maxent_test)
  
  
  # create and save prediction maps
  maxent_map_all <- predict(maxent_best_mod_all, data = maxent_pred_stack,
                            type = "cloglog", clamp = TRUE)
  
  maxent_map_reduced <- predict(maxent_best_mod_reduced, data = maxent_pred_stack,
                                type = "cloglog", clamp = TRUE)
  
  writeRaster(maxent_map_all, paste0(output_dir, "/maps-all/maxent_p_p", i, ".tif"))
  writeRaster(maxent_map_reduced, paste0(output_dir, "/maps-reduced/maxent_p_p", i, ".tif"))
  
  rm(maxent_res, maxent_id_all, maxent_id_reduced, maxent_best_mod_reduced, maxent_best_mod_all)
  
  # ...............isolate all predictions..............
  pres_prediction_all <- extract(maxent_map_all, maxent_p_coords, xy = TRUE, ID = FALSE) %>%
    rename(prediction = lyr1) %>%
    mutate(pres_abs = 1,
           variables = "all")
  
  abs_prediction_all <- extract(maxent_map_all, maxent_bg_coords, xy = TRUE, ID = FALSE) %>%
    rename(prediction = lyr1) %>%
    mutate(pres_abs = 0,
           variables = "all")
  
  # .............isolate reduced predictions............
  pres_prediction_reduced <- extract(maxent_map_reduced, maxent_p_coords, xy = TRUE, ID = FALSE) %>%
    rename(prediction = lyr1) %>%
    mutate(pres_abs = 1,
           variables = "reduced")
  
  abs_prediction_reduced <- extract(maxent_map_reduced, maxent_bg_coords, xy = TRUE, ID = FALSE) %>%
    rename(prediction = lyr1) %>%
    mutate(pres_abs = 0,
           variables = "reduced")
  
  rm(maxent_p_coords, maxent_bg_coords, maxent_map_reduced, maxent_map_all)
  
  # ....................combine results.................
  combined_predictions <- rbind(pres_prediction_all, abs_prediction_all,
                                pres_prediction_reduced, abs_prediction_reduced) %>%
    mutate(plot = i,
           test_auc = ifelse(variables == "all", maxent_auc_all, maxent_auc_reduced))
  
  maxent_all_predictions <- rbind(maxent_all_predictions, combined_predictions)
  
  rm(combined_predictions, pres_prediction_all, abs_prediction_all, pres_prediction_reduced, abs_prediction_reduced)
  
  
} # END Maxent all & reduced loop



## ===================================================================
##                    Maxent Modeling - Select Variables          ----
## ===================================================================

# ......................preparation...................

# set data directory
rast_dir <- here("data", "raster-stacks", "select-variables")
point_dir <- here("data", "CampRoberts_spatial_data")

# ................Run model w/ select variables.......
for (i in seq(1:8)) {
  
  print(paste0(" ========== Maxent Select Variables - Plot ", i, " =========="))
  
  maxent_res <- tune_maxent(plot_number = i,
                            point_dir = point_dir,
                            rast_dir = rast_dir,
                            include_variables = "all")
  
  # ...............store results as vars................
  maxent_p_coords <- maxent_res[["p_coords"]]
  maxent_bg_coords <- maxent_res[["bg_coords"]]
  maxent_pred_stack <- maxent_res[["maxent_pred_stack"]]
  maxent_gs_all <- maxent_res[["maxent_gs_all"]]
  maxent_val <- maxent_res[["maxent_val"]]
  maxent_test <- maxent_res[["maxent_test"]]
  
  
  # .................select best models.................
  # maxent_id_all <- which.max(maxent_gs_all@results$test_AUC)
  # maxent_auc_all <- max(maxent_gs_all@results$test_AUC)
  # maxent_best_mod_all <- combineCV(maxent_gs_all@models[[maxent_id_all]])
  
  #index of top performing model by AUC 2025-10-22
  maxent_id_all <- which.max(maxent_gs_all@results$test_AUC)

  # Create new training data set using variables in best performing model 2025-10-22
  maxent_train_all <- maxent_gs_all@models[[maxent_id_all]]@data 
  
  # Merge validation data with filtered train data 2025-10-22
  test <- mergeSWD(maxent_train_all, maxent_val, only_presence = TRUE)

  # .............generate & save predictions............
  #2025-10-22 train model on combined validation and training data
  maxent_best_mod_all <- train(method = "Maxent", 
                               progress = FALSE,
                               data = maxent_train_all,
                               reg = maxent_gs_all@results[maxent_id_all, 2],
                               fc = maxent_gs_all@results[maxent_id_all, 1]
  )
  
  # pull AUC of model using TEST data
  maxent_auc_all <- auc(maxent_best_mod_all, test=maxent_test)
  
  # create and save maps
  maxent_map_all <- predict(maxent_best_mod_all, data = maxent_pred_stack,
                            type = "cloglog", clamp = TRUE)
  
  writeRaster(maxent_map_all, paste0(output_dir, "/maps-select/maxent_p_p", i, ".tif"))
  
  # ...............isolate all predictions..............
  pres_prediction_all <- extract(maxent_map_all, maxent_p_coords, xy = TRUE, ID = FALSE) %>%
    rename(prediction = lyr1) %>%
    mutate(pres_abs = 1,
           variables = "select")
  
  abs_prediction_all <- extract(maxent_map_all, maxent_bg_coords, xy = TRUE, ID = FALSE) %>%
    rename(prediction = lyr1) %>%
    mutate(pres_abs = 0,
           variables = "select")
  
  rm(maxent_p_coords, maxent_bg_coords, maxent_map_all)
  
  # ....................combine results.................
  combined_predictions <- rbind(pres_prediction_all, abs_prediction_all) %>%
    mutate(plot = i,
           test_auc = maxent_auc_all)
  
  maxent_all_predictions <- rbind(maxent_all_predictions, combined_predictions)
  
  rm(combined_predictions, pres_prediction_all, abs_prediction_all, maxent_gs_all, maxent_gs_reduced, maxent_pred_stack)
  
} # END Maxent select variable loop

# ..................write CSV results ................
write_csv(maxent_all_predictions, here(output_dir, "maxent_results.csv"))