## ===================================================================
##                            Load Packages                       ----
## ===================================================================

library(tidyverse)
library(here)
library(viridis)

maxent_results <- read_csv(here("results", "maxent_results.csv"))
brt_p_results <- read_csv(here("results", "brt_p_results.csv"))
brt_pa_results <- read_csv(here("results", "brt_pa_results.csv"))



## ===================================================================
##                  Calculate Aggregate Correlation               ----
## ===================================================================

## ...................calculate Maxent cor....................

# presence-only methods
maxent_cor <- maxent_results %>% 
  group_by(variables) %>% 
  summarise(cor = cor(prediction, pres_abs),
            auc = mean(test_auc),
            Method = "Maxent")

## ................calculate BRT pres-only cor................
brt_p_cor <- brt_p_results %>% 
  group_by(variables) %>% 
  summarise(cor = cor(prediction, pres_abs),
            auc = mean(test_auc),
            Method = "BRT")

## ...................create full corr df.....................
# calculate cor for BRT pres-abs
results_cor <- brt_pa_results %>% 
  group_by(variables) %>% 
  summarise(cor = cor(prediction, pres_abs),
            auc = mean(test_auc),
            Method = "BRT PA") %>% 
  
  # bind to pres-only correlations
  rbind(maxent_cor, brt_p_cor) %>% 
  rename(Variables = variables)

rm(maxent_cor, brt_p_cor)

## ===================================================================
##                 Calculate Plot Correlations                    ----
## ===================================================================

## ...................calculate Maxent cor....................
# presence-only methods
maxent_cor <- maxent_results %>% 
  group_by(variables, plot) %>% 
  summarise(cor = cor(prediction, pres_abs),
            auc = mean(test_auc),
            Method = "Maxent")

## ................calculate BRT pres-only cor................
brt_p_cor <- brt_p_results %>% 
  group_by(variables, plot) %>% 
  summarise(cor = cor(prediction, pres_abs),
            auc = mean(test_auc),
            Method = "BRT")

## ...................create full corr df.....................
# calculate cor for BRT pres-abs
results_plot_cor <- brt_pa_results %>% 
  group_by(variables, plot) %>% 
  summarise(cor = cor(prediction, pres_abs),
            auc = mean(test_auc),
            Method = "BRT PA") %>% 
  
  # bind to pres-only correlations
  rbind(maxent_cor, brt_p_cor) %>% 
  rename(Variables = variables)

rm(maxent_cor, brt_p_cor)


## ===================================================================
##                        Figures - Aggregate Cor                 ----
## ===================================================================
## ..............correlation plot - presence only.............
results_cor %>% 
  filter(Method != "BRT PA") %>% 
  
  ggplot() +
  geom_point(aes(x = auc,
                 y = cor,
                 # col = Method, 
                 shape = Variables),
             size = 3.5) +
  
  # specify color and shape for consistency
  # scale_color_manual(values = c("Maxent" = "#009988",
  #                               "BRT" = "#EE7733")) +
  scale_shape_manual(values = c("all" = 16,
                                "reduced" = 17,
                                "select" = 15)) +
  
  # add model labels
  annotate("text", 
           x = 0.68, 
           y = 0.32, 
           label = "Maxent",
           size = 5) +
  
  annotate("text",
           x = 0.63,
           y = 0.21,
           label = "BRT",
           size = 5) +
  
  # plot labels & theme
  guides(col = FALSE) +
  theme_bw()+ 
  labs(x = "Test AUC", y = "Correlation")


## ................correlation plot - all models..............
ggplot(results_cor, aes(x = auc,
                        y = cor,
                        # col = Method, 
                        shape = Variables)) +
  geom_point(size = 3.5) +
  
  # specify color and shape for consistency
  # scale_color_manual(values = c("Maxent" = "#009988",
  #                               "BRT" = "#EE7733",
  #                               "BRT PA" = "#EE3377")) +
  scale_shape_manual(values = c("all" = 16,
                                "reduced" = 17,
                                "select" = 15)) +
  
  # add model labels
  annotate("text", 
           x = 0.68, 
           y = 0.32, 
           label = "Maxent",
           size = 4) +
  
  annotate("text",
           x = 0.63,
           y = 0.21,
           label = "BRT",
           size = 4) +
  
  annotate("text",
           x = 0.8,
           y = 0.65,
           label = "BRT PA",
           size = 4) +
  
  # plot labels & theme 
  labs(x = "Test AUC", y = "Correlation") +
  guides(col = FALSE) +
  theme_bw()

## ===================================================================
##                         Figures - Sample Size                  ----
## ===================================================================
# .................. add number of occurrence points.................
results_plot_cor <- results_plot_cor %>% 
  mutate(no_p_points = case_when(plot == 1 ~ 51,
                                 plot == 2 ~ 57,
                                 plot == 3 ~ 78,
                                 plot == 4 ~ 19,
                                 plot == 5 ~ 72,
                                 plot == 6 ~ 39,
                                 plot == 7 ~ 10,
                                 plot == 8 ~ 21),
         plot = as.factor(plot),
         Method = as.factor(Method))


# ...................... # occurrence points plot ....................
# all variable combinations
# ggplot(results_plot_cor) +
#   geom_point(aes(x = auc,
#                  y = cor,
#                  col = Variables,
#                  size = no_p_points)) +
#   scale_color_manual(values = c("all" = "maroon",
#                                 "reduced" = "dodgerblue",
#                                 "select" = "yellowgreen")) +
#   scale_size_continuous(range = c(1,8)) +
#   facet_wrap(~Method)

# only reduced variables
results_plot_cor %>% filter(Variables == "reduced") %>% 

ggplot() +
  geom_point(aes(x = auc,
                 y = cor,
                 size = no_p_points),
             shape = 1) +
  scale_size_continuous(range = c(1,9)) +
  theme_bw() +
  labs(x = "Test AUC", y = "Correlation",
       size = "No. Occurrences") +
  facet_wrap(~Method)



