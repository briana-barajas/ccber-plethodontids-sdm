#' Tune Boosted Regression Tree Model Using SDMTune
#'
#' @param plot_number Numeric plot number
#' @param point_dir File path for occurrence point data
#' @param rast_dir File path for environmental variables
#' @param k_folds The number of cross validation folds
#'
#' @return Grid search results, initial BRT model, reduced variable BRT model, and test data
#'
tune_brt <- function(plot_number, point_dir, rast_dir, k_folds){
  
  ## ========================================
  ##              Load Data              ----
  ## ========================================
  print(paste0("Load Data"))
  
  # load occurrence points
  occurrences <- read_csv(here(point_dir, "Species_pts", "BASP_pres_abs.csv")) %>% 
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
  
  rm(occurrences, envir = .GlobalEnv)
  
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
  #k_max <- round(nrow(distinct(occurrence_coords, x, y)) * 0.8)
  
  cv_folds <- randomFolds(train, k = k_folds, only_presence = FALSE)
  
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
    # interaction.depth = seq(1,6,1),
    # shrinkage = seq(0.01, 0.1, 0.01),
    bag.fraction = seq(0.5, 0.75, 0.05)
  )
  
  print(paste0("Reduce Variables"))
  # remove variables with importance less than 2% IF it doesn't decrease model performance
  brt_mod_reduced <- reduceVar(brt_model,
                               interactive = FALSE,
                               verbose = FALSE,
                               th = 2,
                               metric = "auc",
                               test = brt_test,
                               use_jk = TRUE)
  
  ## ========================================
  ##          Tune Hyperparameters       ----
  ## ========================================
  print(paste0("Tune Hyperparameters"))
  
  # test possible combinations with gridSearch
  brt_gs <- gridSearch(brt_mod_reduced, 
                       interactive = FALSE,
                       progress = FALSE,
                       hypers = param_tune, 
                       metric = "auc", 
                       test = brt_test)
  
  ## ========================================
  ##             Return Results          ----
  ## ========================================
  # return list of results needed for predictions
  res <- list(brt_test, brt_pred_stack, brt_gs, brt_model, brt_mod_reduced)
  names(res) <- list("brt_test", "brt_pred_stack", "brt_gs", "brt_model", "brt_mod_reduced")
  
  return(res)
  
}





