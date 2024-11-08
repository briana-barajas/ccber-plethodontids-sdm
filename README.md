# Species Distribution Modeling for Black-bellied Slender Salamander

Independent graduate student research project, comparing species distribution model performance for the Black-bellied Slender Salamander (*Batrachoseps nigriventris*). The primary goal is to assess the performance of the Boosted Regression Trees and Maximum Entropy (MaxEnt) species distribution models given high-resolution environmental variable data over a small spatial scale (6-hectare plots).

![](https://californiaherps.com/salamanders/images/bnigriventrisss05.jpg)

## Environmental Variables

Species distribution models require known species occurrences as well as environmental data to predict habitat suitability. To compare the contribution of these environmental variables, modeling will be conducted three times for each survey plot: first using all the possible variables, then with reduced variables filtered using the spatial jackknife method, and finally using expert-selected variables.

The environmental variables used are as follows:

| Acronym | Variable              |
|---------|-----------------------|
| elev    | Elevation\*           |
| hli     | Heat load index\*     |
| slope   | Slope\*               |
| canopy  | Canopy cover\*        |
| dnd_dn  | Density downed wood\* |
| li_dn   | Litter cover\*        |
| gs_dn   | Grass cover\*         |
| ba_dn   | Density bare ground   |
| br_dn   | Brush density         |
| br_ht   | Brush height          |
| dnd_st  | State of downed wood  |
| fb_dn   | Forb density          |
| dnd_db  | Diameter downed wood  |

\*indicates expert-selected variables

## Progress

**Completed**

1.  Determined hyperparameter settings to be included in model tuning
2.  Developed functions & script to apply model parameters to selected survey plot
3.  Ran final scripts to produce habitat suitability maps & extracted values at key points

**In Progress**

1.  Selecting methods to address model overfitting
2.  Creating data visualizations to compare model performance

------------------------------------------------------------------------

*This project was done in collaboration with Christopher Evelyn, Vertebrate Curatorial Manager, and Assistant Researcher at the Cheadle Center for Biodiversity & Ecological Restoration (CCBER).*
