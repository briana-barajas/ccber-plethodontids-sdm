#' Create Environmental Raster Prediction Stack
#'
#' @param plot_number Plot number raster stack will be created for 1-8
#' @param new_crs CRS for reprojection of raster layers, default WGS84
#' @param data_dir File path to environmental variable rasters and plot outlines
#' @param end_path File path to store raster stacks
#'
#' @return Stores environmental raster predictor stack to designated path

createPredStack <- function(plot_number, new_crs = "WGS84", data_dir, end_path){
  
  ## ========================================
  ##            Read in Data             ----
  ## ========================================
  
  # geomorphology rasters ----
  slope <- rast(here(data_dir, "old", "crob_slope"))
  elev <- rast(here(data_dir, "crob_elev"))
  
  # create hli raster ----
  hli <-  hli(elev)
  
  # vegetation rasters ----
  canopy <- rast(here(data_dir, "Canopy_raster", paste0("can_fx_3r_p", plot_number))) 
  
  dnd_dn <- rast(here(data_dir, "brush_IDW3rad", paste0("3rd_dnd_dn_p", plot_number))) 
  
  li_dn <- rast(here(data_dir, "brush_IDW3rad", paste0("3rd_li_dn_p", plot_number))) 
  
  gs_dn <- rast(here(data_dir, "brush_IDW3rad", paste0("3rd_gs_dn_p", plot_number))) 
  
  ba_dn <- rast(here(data_dir, "brush_IDW3rad", paste0("3rd_ba_dn_p", plot_number))) 
  
  br_dn <- rast(here(data_dir, "brush_IDW3rad", paste0("3rd_br_dn_p", plot_number))) 
  
  br_ht <- rast(here(data_dir, "brush_IDW3rad", paste0("3rd_br_ht_p", plot_number))) 
  
  dnd_db <- rast(here(data_dir, "brush_IDW3rad", paste0("3rd_dnd_db_p", plot_number))) 
  
  dnd_st <- rast(here(data_dir, "brush_IDW3rad", paste0("3rd_dnd_st_p", plot_number))) 
  
  fb_dn <- rast(here(data_dir, "brush_IDW3rad", paste0("3rd_fb_dn_p", plot_number)))
  
  rk_dn <- rast(here(data_dir, "brush_IDW3rad", paste0("3rd_rk_dn_p", plot_number)))
  
  dnd_stc <- rast(here(data_dir, "brush_IDW3rad", paste0("dnd_stc_3rp", plot_number)))
  
  
  ## ========================================
  ##            Reproject Data           ----
  ## ========================================
  
  # # reproject occurrence points ----
  # occurence <- st_transform(occurence, crs = new_crs)
  
  # geomorphology rast reprojection ----
  elev <- project(elev, y = new_crs)
  slope <- project(slope, y = new_crs)
  hli <- project(hli, y = new_crs)
  
  # veg rast reprojection ----
  canopy <- project(x = canopy, y = new_crs)
  dnd_dn <- project(x = dnd_dn, y = new_crs)
  li_dn <- project(x = li_dn, y = new_crs)
  gs_dn <- project(x = gs_dn, y = new_crs)
  
  ba_dn <- project(x = ba_dn, y = new_crs)
  br_dn <- project(x = br_dn, y = new_crs)
  br_ht <- project(x = br_ht, y = new_crs)
  dnd_db <- project(x = dnd_db, y = new_crs)
  dnd_st <- project(x = dnd_st, y = new_crs)
  fb_dn <- project(x = fb_dn, y = new_crs)
  rk_dn <- project(x = rk_dn, y = new_crs)
  dnd_stc <- project(x = dnd_stc, y = new_crs)
  
  # update raster names ----
  set.names(elev, "elevation")
  set.names(hli, "heat_load_index")
  set.names(slope, "slope")
  set.names(canopy, "canopy_cover")
  set.names(dnd_dn, "density_downed_wood")
  set.names(li_dn, "litter_cover")
  set.names(gs_dn, "grass_cover")
  
  set.names(ba_dn, "density_bare_ground")
  set.names(br_dn, "brush_density")
  set.names(br_ht, "brush_height")
  set.names(dnd_st, "state_downed_wood")
  set.names(dnd_stc, "dnd_stc")
  set.names(fb_dn, "forb_density")
  set.names(rk_dn, "rk_dn")
  set.names(dnd_db, "diameter_downed_wood")
  
  ## ========================================
  ##           Update ext and res        ----
  ## ========================================
  
  # save extent ----
  can_extent <- ext(canopy)
  
  # crop geomorphology rasters ----
  elev <- crop(elev, canopy)
  hli <- crop(hli, canopy)
  slope <- crop(slope, canopy)
  
  # update geo raster ext ----
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
  ##            Export Raster Stacks     ----
  ## ========================================
  # create raster stack ----
  predictor_stack_rast <- c(canopy, dnd_dn, li_dn, slope, elev, hli,
                            # comment out center row for select variables
                            # ba_dn, br_ht, dnd_st, br_dn, dnd_stc, dnd_db, fb_dn,
                            gs_dn)

  
  # export raster stack ----
  writeRaster(x = predictor_stack_rast,
              filename = paste0(end_path, "/predictor_stack_rast_p", plot_number, ".tif"),
              overwrite = TRUE)
}