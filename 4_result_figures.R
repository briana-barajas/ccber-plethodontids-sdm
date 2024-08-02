## ===================================================================
##                            Load Packages                       ----
## ===================================================================

library(tidyverse)
library(here)
library(tmap)

maxent_results <- read_csv(here("results", "maxent_results.csv"))
brt_p_results <- read_csv(here("results", "brt_p_results.csv"))
brt_pa_results <- read_csv(here("results", "brt_pa_results.csv"))



## ===================================================================
##              Calculate Correlation & Combine Results           ----
## ===================================================================

# presence-only methods
maxent_results <- maxent_results %>% 
  group_by(variables) %>% 
  summarise(cor = cor(prediction, pres_abs),
            auc = mean(test_auc),
            method = "maxent")

brt_p_results <- brt_p_results %>% 
  group_by(variables) %>% 
  summarise(cor = cor(prediction, pres_abs),
            auc = mean(test_auc),
            method = "brt_p")

# full results
results <- brt_pa_results %>% 
  group_by(variables) %>% 
  summarise(cor = cor(prediction, pres_abs),
            auc = mean(test_auc),
            method = "brt_pa") %>% 
  rbind(maxent_results, brt_p_results)

rm(brt_pa_results, brt_p_results, maxent_results)


## ===================================================================
##                             Plots                              ----
## ===================================================================
ggplot(results, aes(x = auc,
                    y = cor, 
                    shape = method,
                    col = variables)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("maroon", "dodgerblue", "yellowgreen")) +
  labs(x = "Test AUC", y = "Correlation") +
  theme_minimal()
