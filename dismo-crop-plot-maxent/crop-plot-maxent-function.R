## ----set-up, include=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)


## ----packages, include=FALSE------------------------------------------------------------
# wrangling
library(tidyverse)
library(here)
library(janitor)

# raster/geospatial
library(sf)
library(tmap)
library(terra)
library(raster)
library(spatialEco) #transform elev to hli

# modeling
library(dismo)
library(rsample)
library(rJava)

# set data directory
data_dir <- here("data", "CampRoberts_spatial_data")


## ----maxent-fun-------------------------------------------------------------------------
slsa_crop_maxent <- function(plot_number, new_crs, output_location){
  
  ## ========================================
  ##            Read in Data            ----
  ## ========================================
  
  # point data ----
  occurence <- st_read(here(data_dir, "Species_pts",
                            "CR_BASP_obs_11Jul22.shp")) %>% 
    clean_names() %>% 
    st_make_valid() %>% 
    filter(plot == plot_number)
  
  # plot outline ----
  outline <- st_read(here(data_dir, "plot_outlines", 
                          paste0("Plot_", plot_number, ".shp"))) %>%
    st_make_valid()
  
  # geomorphology rasters ----
  slope <- rast(here(data_dir, "old", "crob_slope"))
  elev <- rast(here(data_dir, "crob_elev"))
  
  # create hli raster ----
  hli <-  hli(elev)
  
  # vegetation rasters ----
  canopy <- rast(here(data_dir, "Canopy_raster",
                      paste0("can_fx_3r_p", plot_number)))
  
  dnd_dn <- rast(here(data_dir, "brush_IDW3rad",
                      paste0("3rd_dnd_dn_p", plot_number)))
  
  li_dn <- rast(here(data_dir, "brush_IDW3rad",
                     paste0("3rd_li_dn_p", plot_number)))
  
  gs_dn <- rast(here(data_dir, "brush_IDW3rad",
                     paste0("3rd_gs_dn_p", plot_number)))
  
  ## ========================================
  ##            Reproject Data           ----
  ## ========================================
  
  # reproject occurrence points ----
  occurence <- st_transform(occurence, crs = new_crs)
  
  # reproject plot outline ----
  outline <- st_transform(outline, crs = new_crs)
  
  # geomorphology rast reprojection ----
  elev <- project(elev, y = new_crs)
  slope <- project(slope, y = new_crs)
  hli <- project(hli, y = new_crs)
  
  # veg rast reprojection ----
  canopy <- project(x = canopy, y = new_crs)
  dnd_dn <- project(x = dnd_dn, y = new_crs)
  li_dn <- project(x = li_dn, y = new_crs)
  gs_dn <- project(x = gs_dn, y = new_crs)
  
  # update raster names ----
  set.names(elev, "elevation")
  set.names(hli, "heat load index")
  set.names(slope, "slope")
  set.names(canopy, "canopy cover")
  set.names(dnd_dn, "downed wood cover")
  set.names(li_dn, "litter cover")
  set.names(gs_dn, "grass cover")
  
  ## ========================================
  ##      Filter to data within plot     ----
  ## ========================================
  # filter occurrence points ----
  occurence <- st_filter(occurence, outline,
                         .predicate = st_within)
  
  # crop geo rasters using plot outlines
  elev <- crop(elev, outline, mask = TRUE)
  hli <- crop(hli, outline, mask = TRUE)
  slope <- crop(slope, outline, mask = TRUE)
  
  # crop vegetation raster using plot outline 
  canopy <-crop(canopy, outline, mask = TRUE)
  dnd_dn <- crop(dnd_dn, outline, mask = TRUE)
  li_dn <- crop(li_dn, outline, mask = TRUE)
  gs_dn <- crop(gs_dn, outline, mask = TRUE)
  
  ## ========================================
  ##        Prep rasters to stack        ----
  ## ========================================
  
  # save extent ----
  can_extent <- ext(canopy)
  
  # crop geomorphology rasters ----
  elev <- crop(elev, canopy)
  hli <- crop(hli, canopy)
  slope <- crop(slope, canopy)
  
  # update gro raster ext ----
  ext(elev) <- c(can_extent$xmin, can_extent$xmax,
                 can_extent$ymin, can_extent$ymax)
  
  ext(hli) <- c(can_extent$xmin, can_extent$xmax,
                can_extent$ymin, can_extent$ymax)
  
  ext(slope) <- c(can_extent$xmin, can_extent$xmax,
                  can_extent$ymin, can_extent$ymax)
  
  # update resolution in all geomorphic layers ----
  elev <- resample(elev, canopy)
  hli <- resample(hli, canopy)
  slope <- resample(slope, canopy)
  
  ## ========================================
  ##     Convert and stack predictors    ----
  ## ========================================
  # convert predictors to RasterLayer object ----
  elev <- raster(elev)
  hli <- raster(hli)
  slope <- raster(slope)
  
  canopy <- raster(canopy)
  dnd_dn <- raster(dnd_dn)
  li_dn <- raster(li_dn)
  gs_dn <- raster(gs_dn)
  
  # stack all predictors ----
  predictor_stack_rast <- raster::stack(elev, hli, slope, canopy, 
                                        dnd_dn, li_dn, gs_dn)
  
  # store raster for prediction step
  assign(x = paste0("pred_stack_rast_p", plot_number), 
         value = predictor_stack_rast, envir = .GlobalEnv)
  
  # convert occurence points ----
  occurence_sp <- as_Spatial(occurence)
  
  
  ## ========================================
  ##          Run Maxent Model           ----
  ## ========================================
  maxent(predictor_stack_rast, occurence_sp,
         removeDuplicates=TRUE, 
         path = output_location)
  
}



## ---------------------------------------------------------------------------------------
slsa_crop_maxent(plot_number = 2, new_crs = "WGS84")

