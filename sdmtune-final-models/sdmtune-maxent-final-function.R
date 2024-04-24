#' Tune MaxEnt Model Using SDMTune
#'
#' @param plot_number Numeric plot number
#'
#' @return Grid search results for 
#'
function(plot_number){
  
  ## ========================================
  ##              Load Data              ----
  ## ========================================
  # store plot as character
  plot_name <- paste0("plot", plot_number)
  
  # load occurrence points
  occurrences <- st_read(here(data_dir, "Species_pts", "CR_BASP_obs_11Jul22.shp")) %>% 
    st_make_valid() %>% 
    clean_names() %>% 
    filter(plot == plot_number)
  
  # list all environmental raster names
  env_layer_names <- c("ba_dn", "br_ht","dnd_db", "dnd_st", "elev", "gs_dn", 
                       "li_dn", "slope", "br_dn", "canopy","dnd_dn", 
                       "dnd_stc", "fb_dn", "hli", "rk_dn")
  
  # load environmental rasters
  for (i in env_layer_names) {
    layer <- rast(here(output_dir, i, plot_name, paste0(i, ".asc")))
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
                        p = occurrence_coords, # occurrence points
                        a = bg_coords, # background point coordinates
                        env = predictor_stack_rast) # background layers
  
  # update swd_object to add sample to background
  swd_obj <- addSamplesToBg(swd_obj)
  
  # split data into test and train
  split <- trainValTest(swd_obj, 
                        test = 0.2, # % of data for testing
                        val = 0, # % of of data for validation
                        only_presence = TRUE, # F=split bg points, T=use all bg points
                        seed = 2) # set seed for random split
  train <- split[[1]]
  test <- split[[2]]
  
  # prepare cross validation folds
  k_max <- nrow(distinct(occurrence_coords, x, y)) - 1
  
  cv_folds <- randomFolds(train, k = k_max, only_presence = TRUE)
  
  ## ========================================
  ##          Define Model & Variables   ----
  ## ========================================
  # define model
  maxnet_model <- train(method = "Maxnet", 
                        folds = cv_folds,
                        data = train)
  
  # select hyper parameters for testing
  param_tune <- list(
    reg = seq(0.1, 3, 0.1), # regularization multiplier
    fc = c("lq", "lh", "lqp", "lqph", "lqpht")) # feature class combination
  
  # remove variables with importance less than 2% IF it doesn't decrease model performance
  maxnet_model_red <- reduceVar(maxnet_model,
                                th = 2,
                                metric = "auc",
                                test = test,
                                use_jk = TRUE)
  
  ## ========================================
  ##          Tune Hyperparameters       ----
  ## ========================================
  # test possible combinations with gridSearch
  gs <- gridSearch(maxnet_model_red, 
                   hypers = param_tune, 
                   metric = "auc", 
                   test = test)
}
















