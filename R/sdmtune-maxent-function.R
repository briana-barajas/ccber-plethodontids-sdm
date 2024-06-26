#' Tune MaxEnt Model Using SDMTune
#'
#' @param plot_number Numeric plot number
#' @param point_dir File path for occurrence data
#' @param rast_dir File path for environmental variables
#' @param include_variables Model with "all", or "reduced" environmental variables, default is "both"
#'
#' @return Grid search results, initial maxent model, reduced variable maxent model, and test data
#'
tune_maxent <- function(plot_number, point_dir, rast_dir, include_variables = "both"){
  
  ## ========================================
  ##              Load Data              ----
  ## ========================================
  print(paste0("Load Data"))
  
  # load occurrence points
  occurrences <- st_read(here(point_dir, "Species_pts", "CR_BASP_obs_11Jul22.shp"), quiet = TRUE) %>% 
    st_make_valid() %>% 
    clean_names() %>% 
    filter(plot == plot_number)
  
  # read in predictor stack
  pred_stack_name <- paste0("predictor_stack_rast_p", plot_number, ".tif")
  maxent_pred_stack <- rast(here(rast_dir, pred_stack_name))
  
  ## ========================================
  ##        Occurrence Data Preparation  ----
  ## ========================================
  print(paste0("Occurrence Data Preparation"))
  
  # update occurence df, isolating occurrence lat and long
  occurrence_coords <- occurrences %>% 
    st_drop_geometry() %>% 
    rename(y = latitude,
           x = longitude) %>% 
    dplyr::select(x,y)
  
  # create background points using raster stack
  bg_points <- spatSample(maxent_pred_stack,
                          size = 1000,
                          replace = TRUE,
                          xy = TRUE)
  
  # isolate coordinates of bg points
  bg_coords <- bg_points %>% dplyr::select(c(x,y))
  
  ## ========================================
  ##           Model Pre-Processing      ----
  ## ========================================
  print(paste0("Model Pre-Processing"))
  
  # create SWD object using data
  swd_obj <- prepareSWD(species = "Black-bellied Slender Salamander",
                        p = occurrence_coords,
                        a = bg_coords,
                        env = maxent_pred_stack)
  
  # update swd_object to add sample to background
  swd_obj <- addSamplesToBg(swd_obj)
  
  # split data into test and train
  split <- trainValTest(swd_obj, 
                        test = 0.2,
                        val = 0, 
                        only_presence = TRUE, 
                        seed = 2) 
  train <- split[[1]]
  maxent_test <- split[[2]]
  
  # prepare cross validation folds
  k_max <- round(nrow(distinct(occurrence_coords, x, y)) * 0.55)
  
  cv_folds <- randomFolds(train, k = k_max, only_presence = TRUE)
  
  ## ========================================
  ##          Define Model & Variables   ----
  ## ========================================
  print(paste0("Define Model & Variables"))
  
  # define model
  maxent_model <- train(method = "Maxnet",
                        progress = FALSE,
                        folds = cv_folds,
                        data = train)
  
  # select hyper parameters for testing
  param_tune <- list(
    reg = seq(0.1, 3, 0.1),
    fc = c("lq", "lh", "lqp", "lqph", "lqpht"))
  
  ## ========================================
  ##    Tune Hyper parameters (BOTH)     ----
  ## ========================================
  
  if(include_variables %in% c("BOTH", "both", "Both")){
    
    # ..............reduce variables.............
    print(paste0("Reduce Variables"))
    
    maxent_mod_reduced <- reduceVar(maxent_model,
                                    interactive = FALSE,
                                    verbose = FALSE,
                                    th = 2,
                                    metric = "auc",
                                    test = maxent_test,
                                    use_jk = TRUE)
    
    # ..............grid search.............
    print(paste0("Grid Search - All Variables"))
    maxent_gs_all <- gridSearch(maxent_model,
                                interactive = FALSE,
                                progress = FALSE,
                                hypers = param_tune, 
                                metric = "auc", 
                                test = maxent_test)
    
    print(paste0("Grid Search - Reduced Variables"))
    maxent_gs_reduced <- gridSearch(maxent_mod_reduced,
                                    interactive = FALSE,
                                    progress = FALSE,
                                    hypers = param_tune, 
                                    metric = "auc", 
                                    test = maxent_test)
    
    # ...............return results...............
    res <- list(occurrence_coords, bg_coords, maxent_test, maxent_pred_stack, maxent_gs_all, maxent_gs_reduced)
    names(res) <- list("p_coords", "bg_coords", "maxent_test", "maxent_pred_stack", "maxent_gs_all", "maxent_gs_reduced")
    
    
    ## ========================================
    ##    Tune Hyperparameters (REDUCED)   ----
    ## ========================================  
  } else if (include_variables %in% c("REDUCED", "reduced", "Reduced")){
    
    # ..............reduce variables.............
    print(paste0("Reduce Variables"))
    
    maxent_mod_reduced <- reduceVar(maxent_model,
                                    interactive = FALSE,
                                    verbose = FALSE,
                                    th = 2,
                                    metric = "auc",
                                    test = maxent_test,
                                    use_jk = TRUE)
    
    # ..............grid search.............
    print(paste0("Grid Search - Reduced Variables"))
    maxent_gs_reduced <- gridSearch(maxent_mod_reduced,
                                    interactive = FALSE,
                                    progress = FALSE,
                                    hypers = param_tune, 
                                    metric = "auc", 
                                    test = maxent_test)
    
    # ...............return results...............
    res <- list(occurrence_coords, bg_coords, maxent_test, maxent_pred_stack, maxent_gs_reduced)
    names(res) <- list("p_coords", "bg_coords", "maxent_test", "maxent_pred_stack", "maxent_gs_reduced")
    
    ## ========================================
    ##    Tune Hyperparameters (ALL VARS)  ----
    ## ========================================
    
  } else if(include_variables %in% c("ALL", "all", "All")){
    
    # ..............grid search.............
    print(paste0("Grid Search - All Variables"))
    maxent_gs_all <- gridSearch(maxent_model,
                                interactive = FALSE,
                                progress = FALSE,
                                hypers = param_tune, 
                                metric = "auc", 
                                test = maxent_test)
    
    # ...............return results...............
    res <- list(bg_coords, occurrence_coords, maxent_test, maxent_pred_stack, maxent_gs_all)
    names(res) <- list("bg_coords", "p_coords", "maxent_test", "maxent_pred_stack", "maxent_gs_all")
    
    
    ## ========================================
    ##    Tune Hyperparameters (ERROR) ----
    ## ======================================== 
    
  } else {
    
    stop(paste0("Must set include_variables to All, Both, or Reduced"))
    
  } # END if-else statement
  
  ## ========================================
  ##             Return Results          ----
  ## ========================================
  
  return(res)
  
}












