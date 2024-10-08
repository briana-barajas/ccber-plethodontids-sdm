## ===================================================================
##                            Load Packages                       ----
## ===================================================================

rm(list = ls())

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
                      pres_abs = TRUE)
  
  # ...............store results as vars................
  brt_p_coords <- brt_res[["p_coords"]]
  brt_a_coords <- brt_res[["a_coords"]]
  brt_pred_stack <- brt_res[["brt_pred_stack"]]
  brt_gs_all <- brt_res[["brt_gs_all"]]
  brt_gs_reduced <- brt_res[["brt_gs_reduced"]]
  
  
  # .................select best models.................
  brt_id_all <- which.max(brt_gs_all@results$test_AUC) #index of top model
  brt_auc_all <- max(brt_gs_all@results$test_AUC) # top model AUC
  brt_best_mod_all <- combineCV(brt_gs_all@models[[brt_id_all]]) # apply model
  
  brt_id_reduced <- which.max(brt_gs_reduced@results$test_AUC)
  brt_auc_reduced <- max(brt_gs_reduced@results$test_AUC)
  brt_best_mod_reduced <- combineCV(brt_gs_reduced@models[[brt_id_reduced]])
  
  # .............generate & save predictions............
  brt_map_all <- predict(brt_best_mod_all, data = brt_pred_stack)
  brt_map_reduced <- predict(brt_best_mod_reduced, data = brt_pred_stack)
  
  writeRaster(brt_map_all, paste0(output_dir, "/maps-all/brt_pa_p", i, ".tif"))
  writeRaster(brt_map_reduced, paste0(output_dir, "/maps-reduced/brt_pa_p", i, ".tif"))
  
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
                      pres_abs = TRUE)
  
  # ...............store results as vars................
  brt_p_coords <- brt_res[["p_coords"]]
  brt_a_coords <- brt_res[["a_coords"]]
  brt_pred_stack <- brt_res[["brt_pred_stack"]]
  brt_gs_all <- brt_res[["brt_gs_all"]]
  
  
  # .................select best models.................
  brt_id_all <- which.max(brt_gs_all@results$test_AUC)
  brt_auc_all <- max(brt_gs_all@results$test_AUC)
  brt_best_mod_all <- combineCV(brt_gs_all@models[[brt_id_all]])
  
  # .............generate & save predictions............
  brt_map_all <- predict(brt_best_mod_all, data = brt_pred_stack)
  
  writeRaster(brt_map_all, paste0(output_dir, paste0("/maps-select/brt_pa_p", i, ".tif")))
  
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
write_csv(brt_all_predictions, here(output_dir, "brt_pa_results.csv"))






