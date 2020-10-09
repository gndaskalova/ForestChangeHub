# ForestChangeHub
Code for population, biodiversity & forest cover change attribution analyses

_This repository contains the code for the statistical analyses in "Landscape-scale forest loss as a catalyst of population and biodiversity change"._

Now published as:
Daskalova, G. N., Myers-Smith, I. H., Bjorkman, A. D., Blowes, S. A., Supp, S. R., Magurran, A. E., & Dornelas, M. (2020). Landscape-scale forest loss as a catalyst of population and biodiversity change. Science, 368(6497), 1341-1347.
https://science.sciencemag.org/content/368/6497/1341.abstract

Authors: Gergana Daskalova, Isla Myers-Smith, Anne Bjorkman, Sarah Supp, Shane Blowes, Anne Magurran, Maria Dornelas

#### Contact: Gergana Daskalova gndaskalova@gmail.com

# Data

### Living Planet Database
The population time series analysed here came from the Living Planet Database, publicly available from http://www.livingplanetindex.org

### BioTIME
The biodiversity time series analysed here came from the BioTIME Database. Approximately 92% of the biodiversity studies analysed here are available as part of the published BioTIME Database. The data are openly available, and can be accessed on Zenodo (https://doi.org/10.5281/zenodo.1211105) or through the BioTIME website (http://biotime.st-andrews.ac.uk/). The remaining 8% of the studies were used with permission, with details on how to download those data available in the supplementary information.

The public studies that were included in the version of BioTIME we analyzed can be downloaded from http://biotime.st-andrews.ac.uk/BioTIME_download.php

For more information about the BioTIME database, please see:

Dornelas, M., L.H. Antao, F. Moyes, A.E. Bates, A.E. Magurran, and BioTIME consortium (200+ authors). 2018. BioTIME: a database of biodiversity time-series for the Anthropocene. Global Ecology and Biogeography. 10.1111/geb.12729

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
# Calculates richness change over time using a Bayesian hiearchical framework

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
