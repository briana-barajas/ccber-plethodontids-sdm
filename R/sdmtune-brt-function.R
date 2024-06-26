#' Tune Boosted Regression Tree Model Using SDMTune
#'
#' @param plot_number Numeric plot number
#' @param point_dir File path for occurrence point data
#' @param rast_dir File path for environmental variables
#' @param include_variables Model with "all", or "reduced" environmental variables, default is "both"
#'
#' @return Grid search results, initial BRT model, reduced variable BRT model, and test data
#'
tune_brt <- function(plot_number, point_dir, rast_dir, include_variables = "both"){
  
  ## ========================================
  ##              Load Data              ----
  ## ========================================
  print(paste0("Load Data"))
  
  # load occurrence points
  occurrences <- read_csv(here(point_dir, "Species_pts", "BASP_pres_abs.csv"), show_col_types = FALSE) %>% 
    clean_names() %>% 
    filter(plot == plot_number)
  
  # read in predictor stack
  pred_stack_name <- paste0("predictor_stack_rast_p", plot_number, ".tif")
  brt_pred_stack <- rast(here(rast_dir, pred_stack_name))
  
  ## ========================================
  ##        Occurrence Data Preparation  ----
  ## ========================================
  print(paste0("Occurrence Data Preparation"))
  
  # split presence and absence points
  p_coords <- occurrences %>% 
    filter(basp_pa == 1) %>% 
    rename(y = latitude, x = longitude) %>% 
    dplyr::select(x,y)
  
  a_coords <- occurrences %>% 
    filter(basp_pa == 0) %>% 
    rename(y = latitude, x = longitude) %>% 
    dplyr::select(x,y)
  
  ## ========================================
  ##           Model Pre-Processing      ----
  ## ========================================
  print(paste0("Model Pre-Processing"))
  
  # create SWD object using data
  swd_obj <- prepareSWD(species = "Black-Bellied Slender Salamander",
                        p = p_coords,
                        a = a_coords,
                        env = brt_pred_stack)
  
  # split data into test and train
  split <- trainValTest(swd_obj, 
                        test = 0.2,
                        val = 0, 
                        only_presence = FALSE, 
                        seed = 2) 
  train <- split[[1]]
  brt_test <- split[[2]]
  
  # prepare cross validation folds
  k_max <- round(nrow(distinct(p_coords, x, y)) * 0.8)
  
  cv_folds <- randomFolds(train, k = k_max, only_presence = FALSE)
  
  ## ========================================
  ##          Define Model & Variables   ----
  ## ========================================
  print(paste0("Define Model & Variables"))
  
  # define model
  brt_model <- train(method = "BRT", 
                     progress = FALSE,
                     folds = cv_folds,
                     data = train)
  
  # select hyper parameters for testing
  param_tune <- list(
    distribution = "gaussian",
    n.trees = seq(10, 100, 10),
    interaction.depth = seq(1,6,1),
    shrinkage = seq(0.01, 0.1, 0.01),
    bag.fraction = seq(0.5, 0.75, 0.05)
  )
  
  
  ## ========================================
  ##    Tune Hyper parameters (BOTH)     ----
  ## ========================================
  
  if(include_variables %in% c("BOTH", "both", "Both")){
    
    # ..............reduce variables.............
    print(paste0("Reduce Variables"))
    
    brt_mod_reduced <- reduceVar(brt_model,
                                 interactive = FALSE,
                                 verbose = FALSE,
                                 th = 2,
                                 metric = "auc",
                                 test = brt_test,
                                 use_jk = TRUE)
    
    # ..............grid search.............
    print(paste0("Grid Search - All Variables"))
    brt_gs_all <- gridSearch(brt_model, 
                             interactive = FALSE,
                             progress = FALSE,
                             hypers = param_tune, 
                             metric = "auc", 
                             test = brt_test)
    
    print(paste0("Grid Search - Reduced Variables"))
    brt_gs_reduced <- gridSearch(brt_mod_reduced, 
                                 interactive = FALSE,
                                 progress = FALSE,
                                 hypers = param_tune, 
                                 metric = "auc", 
                                 test = brt_test)
    
    # ...............return results...............
    res <- list(p_coords, a_coords, brt_test, brt_pred_stack, brt_gs_all, brt_gs_reduced)
    names(res) <- list("p_coords", "a_coords", "brt_test", "brt_pred_stack", "brt_gs_all", "brt_gs_reduced")
    
    
    ## ========================================
    ##    Tune Hyperparameters (REDUCED)   ----
    ## ========================================
    
  } else if (include_variables %in% c("REDUCED", "reduced", "Reduced")){
    
    # ..............reduce variables.............
    print(paste0("Reduce Variables"))
    
    brt_mod_reduced <- reduceVar(brt_model,
                                 interactive = FALSE,
                                 verbose = FALSE,
                                 th = 2,
                                 metric = "auc",
                                 test = brt_test,
                                 use_jk = TRUE)
    
    # ..............grid search.............
    print(paste0("Grid Search - Reduced Variables"))
    brt_gs_reduced <- gridSearch(brt_mod_reduced, 
                                 interactive = FALSE,
                                 progress = FALSE,
                                 hypers = param_tune, 
                                 metric = "auc", 
                                 test = brt_test)
    
    # ...............return results...............
    res <- list(p_coords, a_coords, brt_test, brt_pred_stack, brt_gs_reduced)
    names(res) <- list("p_coords", "a_coords", "brt_test", "brt_pred_stack", "brt_gs_reduced")
    
    
    ## ========================================
    ##    Tune Hyperparameters (ALL VARS)  ----
    ## ========================================
    
  } else if (include_variables %in% c("ALL", "all", "All")){
    
    # ..............grid search.............
    print(paste0("Grid Search - All Variables"))
    brt_gs_all <- gridSearch(brt_model, 
                             interactive = FALSE,
                             progress = FALSE,
                             hypers = param_tune, 
                             metric = "auc", 
                             test = brt_test)
    
    # ...............return results...............
    res <- list(p_coords, a_coords, brt_test, brt_pred_stack, brt_gs_all)
    names(res) <- list("p_coords", "a_coords", "brt_test", "brt_pred_stack", "brt_gs_all")
    
    
    
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
  
} # END function





