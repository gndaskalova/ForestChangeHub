# Calculating species richness change over time
# Gergana Daskalova
# Gergana Daskalova 20th Feb 2018

#  Load data ----
load("data/input/rarefied_medians2018.Rdata")  
# Output from Blowes and Supp et al.
# Rarefied version of BioTIME
# Raw data are publicly available from Zenodo or the BioTIME website

# Libraries ----
library(tidyr)
library(dplyr)
library(ggplot2)
library(brms)
library(readr)
library(gridExtra)

options(mc.cores = parallel::detectCores())

# Extract terrestrial records and remove those with less than 5 records
terr <- rarefied_medians %>% filter(REALM == "Terrestrial") %>% 
  dplyr::select(STUDY_ID, TAXA, rarefyID, YEAR, S) %>%
  group_by(rarefyID) %>% filter(length(unique(YEAR)) > 4)

# brms models ----

#	Set priors
hier_prior <- c(set_prior(prior = 'normal(0,6)', class='b', coef='year.scaled'), 	# global slope
                set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 		# global intercept
                set_prior(prior = 'cauchy(0,2)', class='sd'))							# group-level intercepts and slopes

# Species richness over time
summary(terr$YEAR)  # starts in 1858, ends in 2017
terr$year2 <- terr$YEAR - 1857  # make years go 1, 2, 3, 4...
terr$year.scaled <- scale(terr$year2, center = T)
summary(terr$year.scaled)  # centered on zero

# One model for all cells - then I extract the slopes of the random effect
richness_sept2018 <- brm(bf(S ~ year.scaled + (year.scaled|rarefyID), 
                        family = brmsfamily('poisson')), data = terr, 
                     prior = hier_prior, iter = 4000,
                     warmup = 2000,
                     inits = '0',
                     control = list(adapt_delta = 0.85), # use 0.80 for early model runs, and increase
                     cores = 2, chains = 2)

# Check model
summary(richness_sept2018)
plot(richness_sept2018)

# Extract slopes for each cell
slopes_richness <- as.data.frame(coef(richness_sept2018))
save(slopes_richness, file = "data/output/slopes_richness_sept2018.RData")
