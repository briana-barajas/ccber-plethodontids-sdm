## ===================================================================
##                            Load Packages                       ----
## ===================================================================

library(tidyverse)
library(here)
library(tmap)

maxent_results <- read_csv(here("results", "maxent_results.csv"))
brt_results <- read_csv(here("results", "brt_results.csv"))


## ===================================================================
##                           Correlation                          ----
## ===================================================================

maxent_results %>% 
  group_by(variables) %>% 
  summarise(cor = cor(prediction, pres_abs))

brt_results %>% 
  group_by(variables) %>% 
  summarise(cor = cor(prediction, pres_abs))
