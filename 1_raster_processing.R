## ===================================================================
##                            Load Packages                       ----
## ===================================================================
# wrangling
library(tidyverse)
library(here)
library(janitor)

# raster/geospatial
library(sf)
library(tmap)
library(terra)
library(spatialEco) #transform elev to hli

# set data directory
data_dir <- here("data", "CampRoberts_spatial_data")
end_path <- here("data", "raster-stacks", "all-variables")

# source in function
source(here("R", "raster-processing-function.R"))

## ===================================================================
##                   Create Environmental Raster Stacks           ----
## ===================================================================

for (i in seq(1:8)) {
  createPredStack(plot_number = i,
                  data_dir = data_dir,
                  end_path = end_path)
}

## ===================================================================
##                       Load and View Rasters                    ----
## ===================================================================

for (i in seq(1:8)) {
  
  file_path <- paste0("predictor_stack_rast_p", i, ".tif")
  
  layer <- rast(here(end_path, file_path))
  
  assign(x = paste0("predictor_stack_rast_p", i),
         value = layer,
         envir = .GlobalEnv)
}


## ===================================================================
##                      Plot Points in Rasters                    ----
## ===================================================================

# # ......... check occurence points fall within raster.............
# plot_number <- 1
# point_dir <- here("data", "CampRoberts_spatial_data")
# 
# # read in occurrence points
# occurrences <- st_read(here(point_dir, "Species_pts", "CR_BASP_obs_11Jul22.shp"), quiet = TRUE) %>% 
#   st_make_valid() %>% 
#   clean_names() %>% 
#   filter(plot == plot_number) %>% 
#   st_transform(crs = "WGS84")
# 
# # plot points within cropped raster 
# tm_shape(predictor_stack_rast_p1$canopy_cover) +
#   tm_raster(palette = "YlGn") +
#   tm_shape(occurrences) +
#   tm_dots(size = 0.2)
# 
# # ............ check absence points fall within raster ...........
# plot_number <- 1
# point_dir <- here("data", "CampRoberts_spatial_data")
# 
# # read in full PA data
# occurrences <- read_csv(here(point_dir, "Species_pts", "BASP_pres_abs.csv"), show_col_types = FALSE) %>% 
#   clean_names() %>% 
#   filter(plot == plot_number)
# 
# # filter to absence points & vectorize
# a_coords <- occurrences %>% 
#   filter(basp_pa == 0) %>% 
#   rename(y = latitude, x = longitude) %>% 
#   dplyr::select(x,y) %>% 
#   vect(geom=c("x","y"), crs = "WGS84") %>% 
#   st_as_sf()
# 
# # plot points within cropped raster
# tm_shape(predictor_stack_rast_p1$canopy_cover) +
#   tm_raster(palette = "YlGn") +
#   tm_shape(a_coords) +
#   tm_dots(size = 0.2)


## ===================================================================
##                       Count Number of Cells                   ----
## ===================================================================

# count number of cells for MaxEnt background points
global(predictor_stack_rast_p1, fun = "notNA")
global(predictor_stack_rast_p2, fun = "notNA") # not identical
global(predictor_stack_rast_p3, fun = "notNA")
global(predictor_stack_rast_p4, fun = "notNA") # not identical
global(predictor_stack_rast_p5, fun = "notNA")
global(predictor_stack_rast_p6, fun = "notNA")
global(predictor_stack_rast_p7, fun = "notNA")
global(predictor_stack_rast_p8, fun = "notNA")
