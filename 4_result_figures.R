## ===================================================================
##                            Load Packages                       ----
## ===================================================================

library(tidyverse)
library(here)
library(gghighlight)

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
  labs(x = "Test AUC", y = "Correlation") +
  theme_bw()+ 
  theme() 


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
           size = 6,
           family = "serif") +
  
  annotate("text",
           x = 0.63,
           y = 0.21,
           label = "BRT",
           size = 6,
           family = "serif") +
  
  annotate("text",
           x = 0.8,
           y = 0.65,
           label = "BRT PA",
           size = 6,
           family = "serif") +
  
  # plot labels & theme 
  labs(x = "Test AUC", y = "Correlation") +
  guides(col = FALSE) +
  theme_bw() +
  theme(axis.text = element_text(family = "serif", size = 13),
        axis.title = element_text(family = "serif", size = 15),
        legend.title = element_text(family = "serif", size = 14),
        legend.text = element_text(family = "serif", size = 13))


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

ggplot(results_plot_cor) +
  geom_point(aes(x = auc,
                 y = cor,
                 shape = Variables,
                 size = no_p_points)) +
  
  # keep consistent shapes
  scale_shape_manual(values = c("all" = 1,
                                "reduced" = 2,
                                "select" = 0)) +
  
  # scale for point size
  scale_size_continuous(range = c(1,9)) +
  
  # highlight reduced variables
  gghighlight(Variables == "reduced", calculate_per_facet = TRUE) +
  
  # plot labels & theme 
  theme_bw() +
  theme(strip.text.x = element_text(family = "serif", size = 16),
        axis.text = element_text(family = "serif", size = 12),
        axis.title = element_text(family = "serif", size = 14),
        legend.title = element_text(family = "serif", size = 13),
        legend.text = element_text(family = "serif", size = 12)) +
  labs(x = "Test AUC", 
       y = "Correlation",
       size = "No. Occurrences") +
  facet_wrap(~Method) 


## ===================================================================
##                   Figures - Overfitting Assessment             ----
## ===================================================================
maxent_results %>% 
  filter(plot == 1) %>% 
  mutate(pres_abs = as.factor(pres_abs)) %>% 
  
  ggplot() +
  geom_point(mapping = aes(x, y, 
                           shape = pres_abs,
                           col = prediction),
             size = 2) +
  scale_color_gradient(low = "lightyellow", high = "firebrick") +
  scale_shape_manual(values = c(1,2)) +
  theme_bw()
  

brt_pa_results %>% 
  filter(plot == 1) %>% 
  select(x,y,prediction) %>% 
  st_as_sf(coords = c("x","y")) %>% 
  plot()


## ............... histograms....................
brt_pa_results %>%
  ggplot() +
  geom_histogram(aes(prediction)) +
  theme_minimal() +
  labs(title = "BRT PA Prediction Distribution")

brt_p_results %>%
  ggplot() +
  geom_histogram(aes(prediction)) +
  theme_minimal() +
  labs(title = "BRT P Prediction Distribution")

maxent_results %>%
  ggplot() +
  geom_histogram(aes(prediction)) +
  theme_minimal() +
  labs(title = "Maxent Prediction Distribution")


## .................confusion matrix................

library(tidymodels)

brt_pa_results %>% 
  mutate(class = ifelse(prediction >= 0.5,1 ,0)) %>% 
  mutate(class = as.factor(class),
         pres_abs = as.factor(pres_abs)) %>% 
  
conf_mat(truth = pres_abs, estimate = class) %>% 
  autoplot(type = "heatmap") +
  labs(title = "BRT PA")


brt_p_results %>% 
  mutate(class = ifelse(prediction >= 0.5,1 ,0)) %>% 
  mutate(class = as.factor(class),
         pres_abs = as.factor(pres_abs)) %>% 
  
  conf_mat(truth = pres_abs, estimate = class) %>% 
  autoplot(type = "heatmap") +
  labs(title = "BRT P")


maxent_results %>% 
  mutate(class = ifelse(prediction >= 0.5,1 ,0)) %>% 
  mutate(class = as.factor(class),
         pres_abs = as.factor(pres_abs)) %>% 
  
  conf_mat(truth = pres_abs, estimate = class) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Maxent")
