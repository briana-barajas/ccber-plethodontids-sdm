# Species Distribution Modeling for California Slender Salamander 
Independent research project, creating and assessing species distribution models for the California Slender salamander (*Batrachoseps attenuatus*). The goal is to assess the performance of species distribution models given high-resolution raster data over a small spatial scale (6-hectare plots).


## Progress
**Completed** 
1. Manipulated raster data to create a single raster stack of environmental variables that will be used for prediction.
2. Completed a Maxent model for all 8 plots using the default settings for `dismo` Maxent.
3. Tuned hyperparameters and applied Maxent to a single plot using `SDMtune`

**In Progress**
1.  Expediting the modeling process to tune hyperparameters and apply Maxent to the remaining 7 plots
2.  Selecting hyperparameters to tune for a boosted regression tree model


----
*This project was done in collaboration with Christopher Evelyn, Vertebrate Curatorial Manager, and Assistant Researcher at the Cheadle Center for Biodiversity & Ecological Restoration (CCBER).*
