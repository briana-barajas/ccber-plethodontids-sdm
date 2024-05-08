#' Tune MaxEnt Model Using SDMTune
#'
#' @param plot_number Numeric plot number
#' @param point_dir File path for occurrence data
#' @param rast_dir File path for environmental variables
#'
#' @return Grid search results, initial maxent model, reduced variable maxent model, and test data
#'
tune_maxent <- function(plot_number, point_dir, rast_dir){
  
  ## ========================================
  ##              Load Data              ----
  ## ========================================
  # store plot as character
  plot_name <- paste0("plot", plot_number)
  
  # load occurrence points
  occurrences <- st_read(here(point_dir, "Species_pts", "CR_BASP_obs_11Jul22.shp")) %>% 
    st_make_valid() %>% 
    clean_names() %>% 
    filter(plot == plot_number)
  
  # list all environmental raster names
  env_layer_names <- c("ba_dn", "br_ht","dnd_db", "dnd_st", "elev", "gs_dn", 
                       "li_dn", "slope", "br_dn", "canopy","dnd_dn", 
                       "dnd_stc", "fb_dn", "hli", "rk_dn")
  
  # load environmental rasters
  for (i in env_layer_names) {
    layer <- rast(here(rast_dir, i, plot_name, paste0(i, ".asc")))
    assign(x = paste0(i), layer, envir = .GlobalEnv)
  }
  
  # create full predictor stack
  predictor_stack_rast <- c(ba_dn, br_ht, dnd_st, elev, gs_dn, li_dn,
                            slope, br_dn, canopy, dnd_dn, dnd_stc, dnd_db,
                            fb_dn, hli, rk_dn)
  
  # remove individual rasters
  rm(ba_dn, br_ht, dnd_st, elev, gs_dn, li_dn, slope, br_dn, canopy, dnd_dn, 
     dnd_stc, fb_dn, hli, rk_dn, layer, dnd_db)
  
  ## ========================================
  ##        Occurrence Data Preparation  ----
  ## ========================================
  # update occurence df, isolating occurrence lat and long
  occurrence_coords <- occurrences %>% 
    st_drop_geometry() %>% 
    rename(y = latitude,
           x = longitude) %>% 
    dplyr::select(x,y)
  
  # create background points using raster stack
  bg_points <- spatSample(predictor_stack_rast,
                          size = 1000,
                          replace = TRUE,
                          xy = TRUE)
  
  # isolate coordinates of bg points
  bg_coords <- bg_points %>% dplyr::select(c(x,y))
  
  ## ========================================
  ##           Model Pre-Processing      ----
  ## ========================================
  # create SWD object using data
  swd_obj <- prepareSWD(species = "Black-bellied Slender Salamander",
                        p = occurrence_coords,
                        a = bg_coords,
                        env = predictor_stack_rast)
  
  # update swd_object to add sample to background
  swd_obj <- addSamplesToBg(swd_obj)
  
  # split data into test and train
  split <- trainValTest(swd_obj, 
                        test = 0.2,
                        val = 0, 
                        only_presence = TRUE, 
                        seed = 2) 
  train <- split[[1]]
  test <- split[[2]]
  
  # prepare cross validation folds
  k_max <- round(nrow(distinct(occurrence_coords, x, y)) * 0.8)
  
  cv_folds <- randomFolds(train, k = k_max, only_presence = TRUE)
  
  ## ========================================
  ##          Define Model & Variables   ----
  ## ========================================
  # define model
  maxent_model <- train(method = "Maxnet",
                        progress = FALSE,
                        folds = cv_folds,
                        data = train)
  
  # select hyper parameters for testing
  param_tune <- list(
    reg = seq(0.1, 3, 0.1),
    fc = c("lq", "lh", "lqp", "lqph", "lqpht"))
  
  # remove variables with importance less than 2% IF it doesn't decrease model performance
  maxent_model_red <- reduceVar(maxent_model,
                                interactive = FALSE,
                                th = 2,
                                metric = "auc",
                                test = test,
                                use_jk = TRUE)
  
  ## ========================================
  ##          Tune Hyperparameters       ----
  ## ========================================
  # test possible combinations with gridSearch
  gs <- gridSearch(maxent_model_red,
                   interactive = FALSE,
                   hypers = param_tune, 
                   metric = "auc", 
                   test = test)
  
  ## ========================================
  ##             Return Results          ----
  ## ========================================
  # test data (need for ROC plots)
  assign("maxent_test", test, envir = .GlobalEnv)
  
  # predictor raster stack (needed for mapping)
  assign("maxent_pred_stack", predictor_stack_rast, envir = .GlobalEnv)
  
  # grid search results
  assign(x = "maxent_gs", gs, envir = .GlobalEnv)
  
  # initial model object
  assign("maxent_model", maxent_model, envir = .GlobalEnv)
  
  # reduced model
  assign("maxent_mod_reduced", maxent_model_red, envir = .GlobalEnv)

}













