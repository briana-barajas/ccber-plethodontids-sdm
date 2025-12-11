library(tidyverse)
library(terra)
library(here)

global_min <- -0.25
global_max <- 1.3

for (i in seq(1:8)) {
  
  # ================== All variables ==================
  
  # 1) BRT Presence Absence - All variables
  map_path <- paste0(here("results/maps-all/brt_pa_p"), i, ".tif")
  map_data <- rast(map_path)
  
  breaks <- c(global_min, 0, 1, global_max)
  colors <- c("darkblue", "white", "orange", "darkred")
  colors <- colorRampPalette(colors)(100)
  
  
  pic_path <- paste0(here("results/maps-all/maps-all-jpg/"), "brt_pa_p", i, ".jpg")
  jpeg(pic_path, width = 800, height = 600, quality = 100)
  
  plot(map_data,
       col = colors,
       range = c(global_min, global_max),
       main = paste0("BRT Presence Absence, All Variables - Plot", i), 
       axes = TRUE)
  
  # close and save file
  dev.off()
  
  
  
  # 2) Maxent - All variables
  map_path <- paste0(here("results/maps-all/maxent_p_p"), i, ".tif")
  
  breaks <- c(global_min, 0, 1, global_max)
  colors <- c("darkblue", "white", "orange", "darkred")
  colors <- colorRampPalette(colors)(100)
  
  pic_path <- paste0(here("results/maps-all/maps-all-jpg/"), "maxent_p_p", i, ".jpg")
  jpeg(pic_path, width = 800, height = 600, quality = 100)
  
  plot(map_data,
       col = colors,
       range = c(global_min, global_max),
       main = paste0("Maxent, All Variables - Plot", i), 
       axes = TRUE)
  
  # close and save file
  dev.off()
  
  
  # 3) BRT - All variables
  map_path <- paste0(here("results/maps-all/brt_p_p"), i, ".tif")
  map_data <- rast(map_path)

  breaks <- c(global_min, 0, 1, global_max)
  colors <- c("darkblue", "white", "orange", "darkred")
  colors <- colorRampPalette(colors)(100)
  
  pic_path <- paste0(here("results/maps-all/maps-all-jpg/"), "brt_p_p", i, ".jpg")
  jpeg(pic_path, width = 800, height = 600, quality = 100)
  
  plot(map_data,
       col = colors,
       range = c(global_min, global_max),
       main = paste0("BRT, All Variables - Plot", i), 
       axes = TRUE)
  
  # close and save file
  dev.off()
  
  
  
  
  
  # ================== Select variables ==================
  
  # 1) BRT Presence Absence - Select variables
  map_path <- paste0(here("results/maps-select/brt_pa_p"), i, ".tif")
  map_data <- rast(map_path)
  
  breaks <- c(global_min, 0, 1, global_max)
  colors <- c("darkblue", "white", "orange", "darkred")
  colors <- colorRampPalette(colors)(100)
  
  pic_path <- paste0(here("results/maps-select/maps-select-jpg/"), "brt_pa_p", i, ".jpg")
  jpeg(pic_path, width = 800, height = 600, quality = 100)
  
  plot(map_data,
       col = colors,
       range = c(global_min, global_max),
       main = paste0("BRT Presence Absence, Select Variables - Plot", i), 
       axes = TRUE)
  
  # close and save file
  dev.off()
  
  
  
  # 2) Maxent - Select variables
  map_path <- paste0(here("results/maps-select/maxent_p_p"), i, ".tif")
  map_data <- rast(map_path)

  breaks <- c(global_min, 0, 1, global_max)
  colors <- c("darkblue", "white", "orange", "darkred")
  colors <- colorRampPalette(colors)(100)
  
  pic_path <- paste0(here("results/maps-select/maps-select-jpg/"), "maxent_p_p", i, ".jpg")
  jpeg(pic_path, width = 800, height = 600, quality = 100)
  
  plot(map_data,
       col = colors,
       range = c(global_min, global_max),
       main = paste0("Maxent, Select Variables - Plot", i), 
       axes = TRUE)
  
  # close and save file
  dev.off()
  
  
  # 3) BRT - Select variables
  map_path <- paste0(here("results/maps-select/brt_p_p"), i, ".tif")
  map_data <- rast(map_path)

  breaks <- c(global_min, 0, 1, global_max)
  colors <- c("darkblue", "white", "orange", "darkred")
  colors <- colorRampPalette(colors)(100)
  
  pic_path <- paste0(here("results/maps-select/maps-select-jpg/"), "brt_p_p", i, ".jpg")
  jpeg(pic_path, width = 800, height = 600, quality = 100)
  
  plot(map_data,
       col = colors,
       range = c(global_min, global_max),
       main = paste0("BRT, Select Variables - Plot", i), 
       axes = TRUE)
  
  # close and save file
  dev.off()
  
  
  # ================== Reduced variables ==================
  
  # 1) BRT Presence Absence - Select variables
  map_path <- paste0(here("results/maps-reduced/brt_pa_p"), i, ".tif")
  map_data <- rast(map_path)

  breaks <- c(global_min, 0, 1, global_max)
  colors <- c("darkblue", "white", "orange", "darkred")
  colors <- colorRampPalette(colors)(100)
  
  pic_path <- paste0(here("results/maps-reduced/maps-reduced-jpg/"), "brt_pa_p", i, ".jpg")
  jpeg(pic_path, width = 800, height = 600, quality = 100)
  
  plot(map_data,
       col = colors,
       range = c(global_min, global_max),
       main = paste0("BRT Presence Absence, Reduced Variables - Plot", i), 
       axes = TRUE)
  
  # close and save file
  dev.off()
  
  
  
  # 2) Maxent - Reduced variables
  map_path <- paste0(here("results/maps-reduced/maxent_p_p"), i, ".tif")
  map_data <- rast(map_path)

  breaks <- c(global_min, 0, 1, global_max)
  colors <- c("darkblue", "white", "orange", "darkred")
  colors <- colorRampPalette(colors)(100)
  
  pic_path <- paste0(here("results/maps-reduced/maps-reduced-jpg/"), "maxent_p_p", i, ".jpg")
  jpeg(pic_path, width = 800, height = 600, quality = 100)
  
  plot(map_data,
       col = colors,
       range = c(global_min, global_max),
       main = paste0("Maxent, Reduced Variables - Plot", i), 
       axes = TRUE)
  
  # close and save file
  dev.off()
  
  
  # 3) BRT - Reduced variables
  map_path <- paste0(here("results/maps-reduced/brt_p_p"), i, ".tif")
  map_data <- rast(map_path)

  breaks <- c(global_min, 0, 1, global_max)
  colors <- c("darkblue", "white", "orange", "darkred")
  colors <- colorRampPalette(colors)(100)
  
  pic_path <- paste0(here("results/maps-reduced/maps-reduced-jpg/"), "brt_p_p", i, ".jpg")
  jpeg(pic_path, width = 800, height = 600, quality = 100)
  
  plot(map_data,
       col = colors,
       range = c(global_min, global_max),
       main = paste0("BRT, Reduced Variables - Plot", i), 
       axes = TRUE)
  
  # close and save file
  dev.off()
  
}