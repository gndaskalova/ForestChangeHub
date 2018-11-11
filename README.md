# ForestChangeHub
Code for population, biodiversity & forest cover change attribution analysis

_This repository contains code necessary to replicate data analysis, figures, and tables for the manuscript "Forest loss does not universally lead to population and biodiversity declines".__

It is archived on Zenodo at : __INSERT__

Disclaimer: The project and related code in this repository represent one version of the code developed for the project, and may undergo future changes and revisions.

Authors: Gergana Daskalova, Isla Myers-Smith, Anne Bjorkman, Sarah Supp, Shane Blowes, Anne Magurran, Maria Dornelas

#### Contact: Gergana Daskalova gndaskalova@gmail.com

# Data

### Living Planet Database
The population time series analysed here came from the Living Planet Database, publicly available from http://www.livingplanetindex.org

### BioTIME
The biodiversity time series analysed here came from 332 unique references found in the BioTIME dataset and in other studies which were used with permission.

Approximately 92% (306 references) of the biodiversity studies analysed here are available as part of the published BioTIME Database28. The data are openly available, and can be accessed on Zenodo (https://doi.org/10.5281/zenodo.1211105) or through the BioTIME website (http://biotime.st-andrews.ac.uk/).

Dornelas, M., L.H. Antao, F. Moyes, A.E. Bates, A.E. Magurran, and BioTIME consortium (200+ authors). 2018. BioTIME: a database of biodiversity time-series for the Anthropocene. Global Ecology and Biogeography. 10.1111/geb.12729

The remaining 8% (26 references) of biodiversity studies analysed were used with permission. Some of these studies are published and publicly available outside of the BioTIME database, and others are available with permission from the corresponding author on reasonable request. For more details regarding the original citations, corresponding authors, and availability of these datasets, please refer to Table S1 in Dornelas et al. (2018).

### Land Use Harmonisation Dataset
Available from http://luh.umd.edu

### Global Forest Change Dataset
https://earthenginepartners.appspot.com/science-2013-global-forest
Extracted through the Google Earth Engine

### MODIS Landcover Dataset
http://glcf.umd.edu/data/lc/
Extracted through the Google Earth Engine

# Scripts

_Please note that the majority of the code was written to run on a HPC cluster and thus requires high computing power to successfully run._

```
+ 01-population-models.R 
# Calculates population change trends using state-space models

+ 02-richness-models.R 
# Calculates richness chaneg over time using a Bayesian hiearchical framework

+ 03-before-after-peak-forest-loss-models.R 
# Tests if population and biodiversity change differ 
# before and after contemporary peak forest loss

+ 04-forest-cover-change-continuous-models.R 
# Tests the relationships between forest loss, forest gain and 
# population and biodiversity change

+ 05-models-lags.R 
# Tests if population and biodiversity change lags following 
# peak forest loss are longer for species with longer generation times
```

# Requirements

### Software
R version 3.5.1 or greater

### Packages
tidyverse, brms
