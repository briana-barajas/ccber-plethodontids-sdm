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
source(here("R", "sdmtune-brt-function.R"))
source(here("R", "sdmtune-maxent-function.R"))


## ===================================================================
##               BRT Modeling - All & Reduced Variables           ----
## ===================================================================

# ......................preparation...................

# set data directory
rast_dir <- here("data", "raster-stacks", "full-stack")
point_dir <- here("data", "CampRoberts_spatial_data")

# create empty df
brt_all_predictions <- data.frame()


# ................run all & reduced BRT...............
for (i in seq(1:8)) {
  brt_res <- tune_brt(plot_number = i,
                      point_dir = point_dir,
                      rast_dir = rast_dir,
                      include_variables = "both")
  
  # ...............store results as vars................
  brt_p_coords <- brt_res[["p_coords"]]
  brt_a_coords <- brt_res[["a_coords"]]
  brt_pred_stack <- brt_res[["brt_pred_stack"]]
  brt_gs_all <- brt_res[["brt_gs_all"]]
  brt_gs_reduced <- brt_res[["brt_gs_reduced"]]
  
  
  # .................select best models.................
  brt_id_all <- which.max(brt_gs_all@results$test_AUC)
  brt_best_mod_all <- combineCV(brt_gs_all@models[[brt_id_all]])
  
  brt_id_reduced <- which.max(brt_gs_reduced@results$test_AUC)
  brt_best_mod_reduced <- combineCV(brt_gs_reduced@models[[brt_id_reduced]])
  
  # .................generate predictions...............
  brt_map_all <- predict(brt_best_mod_all, data = brt_pred_stack)
  brt_map_reduced <- predict(brt_best_mod_reduced, data = brt_pred_stack)
  
  rm(brt_res, brt_id_all, brt_best_mod_all, brt_id_reduced, brt_best_mod_reduced)
  
  # .................vectorize coordinates .............
  brt_p_coords <- brt_p_coords %>% vect(geom = c("x", "y"), crs = "WSG84")
  brt_a_coords <- brt_a_coords %>% vect(geom = c("x", "y"), crs = "WSG84")
  
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
           variables = "all")
  
  rm(brt_p_coords, brt_a_coords, brt_map_reduced, brt_map_all)
  
  # ....................combine results.................
  combined_predictions <- rbind(pres_prediction_all, abs_prediction_all, pres_prediction_reduced, abs_prediction_reduced) %>%
    mutate(plot = i)
  
  brt_all_predictions <- rbind(brt_all_predictions, combined_predictions)
  
  rm(combined_predictions, pres_prediction_all, abs_prediction_all, pres_prediction_reduced, abs_prediction_reduced)
  
} # END BRT all & reduced loop


## ===================================================================
##                    BRT Modeling - Select Variables             ----
## ===================================================================

# ......................preparation...................

# set data directory
rast_dir <- here("data", "raster-stacks", "full-stack")
point_dir <- here("data", "CampRoberts_spatial_data")


# ................Run model w/ select variables.......
for (i in seq(1:8)) {
  
  brt_res <- tune_brt(plot_number = i,
                      point_dir = point_dir,
                      rast_dir = rast_dir,
                      include_variables = "all")
  
  # ...............store results as vars................
  brt_p_coords <- brt_res[["p_coords"]]
  brt_a_coords <- brt_res[["a_coords"]]
  brt_pred_stack <- brt_res[["brt_pred_stack"]]
  brt_gs_all <- brt_res[["brt_gs_all"]]
  
  
  # .................select best models.................
  brt_id_all <- which.max(brt_gs_all@results$test_AUC)
  brt_best_mod_all <- combineCV(brt_gs_all@models[[brt_id_all]])
  
  # .................generate predictions...............
  brt_map_all <- predict(brt_best_mod_all, data = brt_pred_stack)
  
  rm(brt_res, brt_id_all, brt_best_mod_all)
  
  # .................vectorize coordinates .............
  brt_p_coords <- brt_p_coords %>% vect(geom = c("x", "y"), crs = "WSG84")
  brt_a_coords <- brt_a_coords %>% vect(geom = c("x", "y"), crs = "WSG84")
  
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
    mutate(plot = i)
  
  brt_all_predictions <- rbind(brt_all_predictions, combined_predictions)
  
  rm(combined_predictions, pres_prediction_all, abs_prediction_all, brt_gs_all, brt_gs_reduced, brt_pred_stack)
  
} # END BRT select variable loop

## ===================================================================
##               MaxEnt Modeling - All & Reduced Variables        ----
## ===================================================================

















