# Do temporal lags in population and biodiversity change vary based on taxa?
# Gergana Daskalova
# 18th Oct 2019
# gndaskalova@gmail.com

# Libraries ----
library(brms)
library(tidyverse)

# Species richness ----
load("data/output/lags_sp.RData")
load("data/input/rarefied_medians2018.Rdata")  # for metadata on each cell
rarefied_medians <- rarefied_medians2018
meta_for_the_lags <- rarefied_medians %>%
  dplyr::select(rarefyID, BIOME_MAP)

lags_sp <- inner_join(lags_sp, meta_for_the_lags, by = "rarefyID") %>%
  distinct()

# Set priors
lag_prior <- c(set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	
               set_prior(prior = 'cauchy(0,2)', class='sd'))	

# Models fit with a zero intercept because TAXA is a categorical estimate
# and we want an estimate for each taxa, not e.g. birds relative to amphibians
# which would automatically become the intercept if it's not set to zero
sp_lag_m <- brm(bf(lag ~ TAXA - 1 +
                             (1|BIOME_MAP)), 
                        data = lags_sp, 
                        prior = lag_prior, iter = 6000,
                        warmup = 2000,
                        inits = '0',
                        control = list(adapt_delta = 0.89),
                        cores = 2, chains = 2)

# Check model and save output
summary(sp_lag_m)
plot(sp_lag_m)
save(sp_lag_m, file = "data/output/sp_lag_m.RData")

# Turnover ----
load("data/output/lags_tu.RData")
lags_tu <- inner_join(lags_tu, meta_for_the_lags, by = "rarefyID") %>%
  distinct()

tu_lag_m <- brm(bf(lag ~ TAXA - 1 +
                     (1|BIOME_MAP)), 
                data = lags_tu, 
                prior = lag_prior, iter = 6000,
                warmup = 2000,
                inits = '0',
                control = list(adapt_delta = 0.89),
                cores = 2, chains = 2)

# Check model and save output
summary(tu_lag_m)
plot(tu_lag_m)
save(tu_lag_m, file = "data/output/tu_lag_m.RData")

# Population change ----
load("data/output/lags_pop.RData")
mus <- read.csv("data/input/global_mus_scaled.csv")

biome_lpi <- mus %>%
  dplyr::select(id, biome)

period2_pop2 <- inner_join(period2_pop2, biome_lpi, by = "id") %>%
  distinct()

pop_lag_m <- brm(bf(lag ~ Class - 1 +
                     (1|biome)), 
                data = period2_pop2, 
                prior - lag_prior, iter = 6000,
                warmup = 2000,
                inits = '0',
                control = list(adapt_delta = 0.89),
                cores = 2, chains = 2)

# Check model and save output
summary(pop_lag_m)
plot(pop_lag_m)
save(pop_lag_m, file = "data/output/pop_lag_m.RData")

# Mammal population change lags and generation time
# Generation time data from Pacifici et al.

lag_prior<- c(set_prior(prior = 'normal(0,6)', class='b', coef='mean_gentime2'), 	# global slope
              set_prior(prior = 'normal(0,6)', class='Intercept', coef=''))	# global intercept

# Model
mammal_lag_m <- brm(bf(lag ~ mean_gentime2), 
                    data = lag_gen_times3, 
                    prior = lag_prior,
                    iter = 6000,
                    warmup = 2000,
                    inits = '0',
                    control = list(adapt_delta = 0.89),
                    cores = 2, chains = 2)

summary(mammal_lag_m)[17]
plot(mammal_lag_m)
save(mammal_lag_m, file = "data/output/mammal_lag_m2018.RData")