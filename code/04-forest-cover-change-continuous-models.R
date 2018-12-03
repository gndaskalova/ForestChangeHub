# Continuous forest cover change models
# How do forest loss and gain affect population & biodiversity change?
# Gergana Daskalova
# 24th Oct
# gndaskalova@gmail.com

library(tidyr)
library(dplyr)
library(brms)

options(mc.cores = parallel::detectCores())

# Forest loss extracted from the Land Use Harmonisation dataset
# available from the LUH website

# For overall forest loss, we are modelling time series that have
# experienced at least 5% forest loss
# (based on LUH dataset, calculated across a duration matching the time series)

# For forest cover loss and gain calculated based on the 
# Hansen Global Forest Change dataset, we modelled time series that
# have experienced at least 0.5km2 of cumulative loss or gain
# (calculated between 2000-2016, the duration of the GFC dataset)

# The period over which we calculated population change, biodiversity change
# and forest cover chane always matched
# e.g., when modeling forest gain (based on GFC, 2000-2016), 
# species richness change was also calculated between the years 2000 and 2016

# Pop change ----
load("data/output/lpi_mu_hansen.RData")

# ** Forest loss - population increases ----
# LUH forest loss data
load("data/output/mus_luh.RData")

# Duration data
duration_lpd <- read.csv("data/input/global_mus_scaled.csv")
lpi_min_years <- duration_lpd %>%
  dplyr::select(id, End)

##	set some weakly regularising priors
hier_prior_random <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'),
                       set_prior(prior = 'normal(0,6)', class='b', coef='forest.diff_scaled'), 	# global slope
                       set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 		# global intercept
                       set_prior(prior = 'cauchy(0,2)', class='sd'))								# group-level intercepts

mus_luh_pos <- filter(mus_luh, mu > 0)
mus_luh_pos <- mus_luh_pos %>% filter(forest.diff > 0.05)
mus_luh_pos$forest.diff_scaled <- scale(mus_luh_pos$forest.diff, center = T)

# Scale duration column
mus_luh_pos$min_years.scaled <- scale(mus_luh_pos$End, center = T)
summary(mus_luh_pos$min_years.scaled)  # centered on zero

luh_pop_pos <- brm(bf(mu ~ forest.diff_scaled + min_years.scaled + (1|biome)), 
                   data = mus_luh_pos, 
                   prior = hier_prior_random, iter = 6000,
                   warmup = 2000,
                   inits = '0',
                   control = list(adapt_delta = 0.95),
                   cores = 2, chains = 2)

# Check model and save outputs
summary(luh_pop_pos)
plot(luh_pop_pos)
save(luh_pop_pos, file = "data/output/luh_pop_pos2018_dur.RData")

# ** Forest loss - population declines ----
mus_luh_neg <- filter(mus_luh, mu < 0)
mus_luh_neg <- mus_luh_neg %>% filter(forest.diff > 0.05)
mus_luh_neg$forest.diff_scaled <- scale(mus_luh_neg$forest.diff, center = T)

# Scale duration column
mus_luh_neg$min_years.scaled <- scale(mus_luh_neg$End, center = T)
summary(mus_luh_neg$min_years.scaled)  # centered on zero

luh_pop_neg <- brm(bf(mu ~ forest.diff_scaled + min_years.scaled + (1|biome)), 
                   data = mus_luh_neg, 
                   prior = hier_prior_random, iter = 6000,
                   warmup = 2000,
                   inits = '0',
                   control = list(adapt_delta = 0.98),
                   cores = 2, chains = 2)

# Check model and save outputs
summary(luh_pop_neg)
plot(luh_pop_neg)
save(luh_pop_neg, file = "data/output/luh_pop_neg2018_dur.RData")

# ** Forest gain - population increases ----
# 2000 - 2016
# Add a column for duration
lpi_mu_hansen <- left_join(lpi_mu_hansen, lpi_min_years, by = "id") %>%
  distinct()

lpi_mu_hansen_pos <- lpi_mu_hansen %>% filter(mu > 0)
lpi_mu_hansen_pos2 <- filter(lpi_mu_hansen_pos, sum_gain_km > 0.5)
lpi_mu_hansen_pos2$sum_gain_km_scaled <- scale(lpi_mu_hansen_pos2$sum_gain_km, center = T)
summary(lpi_mu_hansen_pos2$sum_gain_km_scaled)

# Scale duration column
lpi_mu_hansen_pos2$min_years.scaled <- scale(lpi_mu_hansen_pos2$End, center = T)
summary(lpi_mu_hansen_pos2$min_years.scaled)  # centered on zero

# Set priors
hier_prior_random <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'),
                       set_prior(prior = 'normal(0,6)', class='b', coef='sum_gain_km_scaled'), 	# global slope
                       set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                       set_prior(prior = 'cauchy(0,2)', class='sd'))		# group-level intercepts

hansen_pop_gain_pos <- brm(bf(mu ~ sum_gain_km_scaled + min_years.scaled + (1|biome)), 
                           data = lpi_mu_hansen_pos2, 
                           prior = hier_prior_random, iter = 6000,
                           warmup = 2000,
                           inits = '0',
                           control = list(adapt_delta = 0.98),
                           cores = 2, chains = 2)

# Check model and save outputs
summary(hansen_pop_gain_pos)
plot(hansen_pop_gain_pos)
save(hansen_pop_gain_pos, file = "data/output/hansen_pop_gain_pos2018_dur.RData")

# ** Forest gain - population declines ----
# 2000 - 2016
# Poor convergence - model not included
# Small sample size and very little variance
lpi_mu_hansen_neg <- lpi_mu_hansen %>% filter(mu < 0)
lpi_mu_hansen_neg2 <- filter(lpi_mu_hansen_neg, sum_gain_km > 0.5)
lpi_mu_hansen_neg2$sum_gain_km_scaled <- scale(lpi_mu_hansen_neg2$sum_gain_km, center = T)
summary(lpi_mu_hansen_neg2$sum_gain_km_scaled)

# Scale duration column
lpi_mu_hansen_neg2$min_years.scaled <- scale(lpi_mu_hansen_neg2$min_years, center = T)
summary(lpi_mu_hansen_neg2$min_years.scaled)  # centered on zero

# Set priors
hier_prior_random <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'),
                       set_prior(prior = 'normal(0,6)', class='b', coef='sum_gain_km_scaled'), 	# global slope
                       set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                       set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

hansen_pop_gain_neg <- brm(bf(mu ~ sum_gain_km_scaled + min_years.scaled + (1|biome)), 
                           data = lpi_mu_hansen_neg2, 
                           prior = hier_prior_random, iter = 6000,
                           warmup = 2000,
                           inits = '0',
                           control = list(adapt_delta = 0.98),
                           cores = 2, chains = 2)

# Check model and save outputs
summary(hansen_pop_gain_neg)
plot(hansen_pop_gain_neg)
save(hansen_pop_gain_neg, file = "data/output/hansen_pop_gain_neg2018.RData")

# ** Forest loss - population increases ----
# 2000 - 2016

# Set priors
hier_prior_random2 <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'),
                        set_prior(prior = 'normal(0,6)', class='b', coef='sum_loss_km_scaled'), 	# global slope
                        set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                        set_prior(prior = 'cauchy(0,2)', class='sd'))		# group-level intercepts

lpi_mu_hansen_pos3 <- lpi_mu_hansen %>% filter(mu > 0)
lpi_mu_hansen_pos3 <- filter(lpi_mu_hansen_pos3, sum_loss_km > 0.5)
lpi_mu_hansen_pos3$sum_loss_km_scaled <- scale(lpi_mu_hansen_pos3$sum_loss_km, center = T)

# Scale duration column
lpi_mu_hansen_pos3$min_years.scaled <- scale(lpi_mu_hansen_pos3$End, center = T)
summary(lpi_mu_hansen_pos3$min_years.scaled)  # centered on zero

hansen_pop_loss <- brm(bf(mu ~ sum_loss_km_scaled + min_years.scaled + (1|biome)), 
                       data = lpi_mu_hansen_pos3, 
                       prior = hier_prior_random2, iter = 6000,
                       warmup = 2000,
                       inits = '0',
                       control = list(adapt_delta = 0.89),
                       cores = 2, chains = 2)

# Check model and save outputs
summary(hansen_pop_loss)
plot(hansen_pop_loss)
save(hansen_pop_loss, file = "data/output/hansen_pop_loss_pos2018_dur.RData")

# ** Forest loss - population declines ----
# 2000 - 2016

lpi_mu_hansen_neg3 <- lpi_mu_hansen %>% filter(mu < 0)
lpi_mu_hansen_neg3 <- filter(lpi_mu_hansen_neg3, sum_loss_km > 0.5)
lpi_mu_hansen_neg3$sum_loss_km_scaled <- scale(lpi_mu_hansen_neg3$sum_loss_km, center = T)

# Scale duration column
lpi_mu_hansen_neg3$min_years.scaled <- scale(lpi_mu_hansen_neg3$End, center = T)
summary(lpi_mu_hansen_neg3$min_years.scaled)  # centered on zero

hansen_pop_loss_neg <- brm(bf(mu ~ sum_loss_km_scaled + min_years.scaled + (1|biome)), 
                           data = lpi_mu_hansen_neg3, 
                           prior = hier_prior_random2, iter = 6000,
                           warmup = 2000,
                           inits = '0',
                           control = list(adapt_delta = 0.89),
                           cores = 2, chains = 2)

# Check model and save outputs
summary(hansen_pop_loss_neg)
plot(hansen_pop_loss_neg)
save(hansen_pop_loss_neg, file = "data/output/hansen_pop_loss_neg2018_dur.RData")

# Split by species - forest vs non-forest ----
load("data/input/mus_luh_habitat.RData")

#	Set priors
hier_prior_random <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'),
                       set_prior(prior = 'normal(0,6)', class='b', coef='forest.diff_scaled'), 	# global slope
                       set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                       set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

mus_luh_pos_hab <- filter(mus_luh_habitats, mu > 0)
mus_luh_pos_hab <- mus_luh_pos_hab %>% filter(forest.diff > 0.05)
mus_luh_pos_hab$forest.diff_scaled <- scale(mus_luh_pos_hab$forest.diff, center = T)
colnames(mus_luh_pos_hab)[45] <- "habitat"

# Scale duration column
mus_luh_pos_hab$min_years.scaled <- scale(mus_luh_pos_hab$End, center = T)
summary(mus_luh_pos_hab$min_years.scaled)  # centered on zero


# Population increases
luh_pop_pos_hab <- brm(bf(mu ~ forest.diff_scaled + habitat + min_years.scaled + (1|biome)), 
                       data = mus_luh_pos_hab, 
                       prior = hier_prior_random, iter = 6000,
                       warmup = 2000,
                       inits = '0',
                       control = list(adapt_delta = 0.95), 
                       cores = 2, chains = 2)

summary(luh_pop_pos_hab)[14]
plot(luh_pop_pos_hab)
save(luh_pop_pos_hab, file = "data/output/luh_pop_pos_hab2018_dur.RData")

# Population declines
mus_luh_neg_hab <- filter(mus_luh_habitats, mu < 0)
mus_luh_neg_hab <- mus_luh_neg_hab %>% filter(forest.diff > 0.05)
mus_luh_neg_hab$forest.diff_scaled <- scale(mus_luh_neg_hab$forest.diff, center = T)
colnames(mus_luh_neg_hab)[45] <- "habitat"

# Scale duration column
mus_luh_neg_hab$min_years.scaled <- scale(mus_luh_neg_hab$End, center = T)
summary(mus_luh_neg_hab$min_years.scaled)  # centered on zero

luh_pop_neg_hab <- brm(bf(mu ~ forest.diff_scaled + habitat + min_years.scaled + (1|biome)), 
                       data = mus_luh_neg_hab, 
                       prior = hier_prior_random, iter = 6000,
                       warmup = 2000,
                       inits = '0',
                       control = list(adapt_delta = 0.89), 
                       cores = 2, chains = 2)

summary(luh_pop_neg_hab)[14]
plot(luh_pop_neg_hab)
save(luh_pop_neg_hab, file = "data/output/luh_pop_neg_hab2018_dur.RData")

# Sp richness ----

# ** Forest loss - richness gains ----
load("data/output/biotime_luh_polys_changeSept2018.RData")

# Calculate duration
slopes_luh$duration <- slopes_luh$endYear - slopes_luh$startYear

slopes_luh4 <- filter(slopes_luh, forest.diff > 0.05)
slopes_luh_pos <- slopes_luh4 %>% filter(slope > 0)
slopes_luh_pos$forest.diff_scaled <- scale(slopes_luh_pos$forest.diff, center = T)

# Scale duration column
slopes_luh_pos$duration.scaled <- scale(slopes_luh_pos$duration, center = T)
summary(slopes_luh_pos$duration.scaled)  # centered on zero

# Set priors
hier_prior_random_b <- c(set_prior(prior = 'normal(0,6)', class='b', coef='duration.scaled'),
                         set_prior(prior = 'normal(0,6)', class='b', coef='forest.diff_scaled'), 	# global slope
                       set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                       set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

sp_luh_cont_pos <- brm(bf(slope ~ forest.diff_scaled + duration.scaled + (1|Biome)), 
                       data = slopes_luh_pos, 
                       prior = hier_prior_random_b, iter = 6000,
                       warmup = 2000,
                       inits = '0',
                       control = list(adapt_delta = 0.89),
                       cores = 2, chains = 2)

# Check model and save output
summary(sp_luh_cont_pos)
plot(sp_luh_cont_pos)
save(sp_luh_cont_pos, file = "data/output/sp_luh_cont_pos2018_dur.RData")

# ** Forest loss - richness losses ----
slopes_luh_neg <- slopes_luh4 %>% filter(slope < 0)
slopes_luh_neg$forest.diff_scaled <- scale(slopes_luh_neg$forest.diff, center = T)

# Scale duration column
slopes_luh_neg$duration.scaled <- scale(slopes_luh_neg$duration, center = T)
summary(slopes_luh_neg$duration.scaled)  # centered on zero

sp_luh_cont_neg <- brm(bf(slope ~ forest.diff_scaled + duration.scaled + (1|Biome)), 
                       data = slopes_luh_neg, 
                       prior = hier_prior_random_b, iter = 6000,
                       warmup = 2000,
                       inits = '0',
                       control = list(adapt_delta = 0.89),
                       cores = 2, chains = 2)

# Check model and save output
summary(sp_luh_cont_neg)
plot(sp_luh_cont_neg)
save(sp_luh_cont_neg, file = "data/output/sp_luh_cont_neg2018_dur.RData")

# Forest cover gain and loss from Hansen
load("data/output/biotime_forest_change.Rdata") # original BioTIME studies
load("data/output/biotime_forest_change_Sept2018.Rdata") # newest additions
colnames(slopes_sum2)
colnames(slopes_sum_Sept2018)
slopes_sum_Sept2018 <- slopes_sum_Sept2018 %>% 
  dplyr::select(rarefyID, slope, sum_gain, sum_loss,
                sum_gain_km, sum_loss_km)
slopes_forest <- rbind(slopes_sum2[, 1:6], slopes_sum_Sept2018)

# ** Forest gain - richness gains ----
# 2000 - 2016

# Add the biome and duration data
biomes <- slopes_luh %>% dplyr::select(rarefyID, Biome, duration)
slopes_forest <- left_join(slopes_forest, biomes, by = "rarefyID")

slopes_forest_pos <- filter(slopes_forest, slope > 0)
slopes_forest_pos2 <- filter(slopes_forest_pos, sum_gain_km > 0.5)
slopes_forest_pos2$sum_gain_km_scaled <- scale(slopes_forest_pos2$sum_gain_km, center = T)

# Scale duration column
slopes_forest_pos2$duration.scaled <- scale(slopes_forest_pos2$duration, center = T)
summary(slopes_forest_pos2$duration.scaled)  # centered on zero

# Set priors
hier_prior_random4 <- c(set_prior(prior = 'normal(0,6)', class='b', coef='duration.scaled'),
                        set_prior(prior = 'normal(0,6)', class='b', coef='sum_gain_km_scaled'), 	# global slope
                        set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                        set_prior(prior = 'cauchy(0,2)', class='sd'))	 # group-level intercepts

hansen_sp_gain_pos <- brm(bf(slope ~ sum_gain_km_scaled + duration.scaled + (1|Biome)), 
                          data = slopes_forest_pos2, 
                          prior = hier_prior_random4, iter = 6000,
                          warmup = 2000,
                          inits = '0',
                          control = list(adapt_delta = 0.89),
                          cores = 2, chains = 2)

# Check model and save outputs
summary(hansen_sp_gain_pos)
plot(hansen_sp_gain_pos)
save(hansen_sp_gain_pos, file = "data/output/hansen_sp_gain_pos2018_dur.RData")

# ** Forest gain - richness losses ----
# 2000 - 2016

slopes_forest_neg <- filter(slopes_forest, slope < 0)
slopes_forest_neg2 <- filter(slopes_forest_neg, sum_gain_km > 0.5)
slopes_forest_neg2$sum_gain_km_scaled <- scale(slopes_forest_neg2$sum_gain_km, center = T)

# Scale duration column
slopes_forest_neg2$duration.scaled <- scale(slopes_forest_neg2$duration, center = T)
summary(slopes_forest_neg2$duration.scaled)  # centered on zero

hansen_sp_gain_neg <- brm(bf(slope ~ sum_gain_km_scaled + duration.scaled + (1|Biome)), 
                          data = slopes_forest_neg2, 
                          prior = hier_prior_random4, iter = 6000,
                          warmup = 2000,
                          inits = '0',
                          control = list(adapt_delta = 0.89),
                          cores = 2, chains = 2)

# Check model and save outputs
summary(hansen_sp_gain_neg)
plot(hansen_sp_gain_neg)
save(hansen_sp_gain_neg, file = "data/output/hansen_sp_gain_neg2018_dur.RData")

# ** Forest loss - richness gains ----
# 2000 - 2016

# Set prior
hier_prior_random2 <- c(set_prior(prior = 'normal(0,6)', class='b', coef='duration.scaled'),
                        set_prior(prior = 'normal(0,6)', class='b', coef='sum_loss_km_scaled'), 	# global slope
                        set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                        set_prior(prior = 'cauchy(0,2)', class='sd'))		# group-level intercepts

slopes_forest_pos <- filter(slopes_forest, slope > 0)
slopes_forest_pos3 <- filter(slopes_forest_pos, sum_loss_km > 0.5)
slopes_forest_pos3$sum_loss_km_scaled <- scale(slopes_forest_pos3$sum_loss_km, center = T)

# Scale duration column
slopes_forest_pos3$duration.scaled <- scale(slopes_forest_pos3$duration, center = T)
summary(slopes_forest_pos3$duration.scaled)  # centered on zero


hansen_sp_loss_pos <- brm(bf(slope ~ sum_loss_km_scaled + duration.scaled + (1|Biome)), 
                          data = slopes_forest_pos3, 
                          prior = hier_prior_random2, iter = 6000,
                          warmup = 2000,
                          inits = '0',
                          control = list(adapt_delta = 0.89),
                          cores = 2, chains = 2)

# Check model and save outputs
summary(hansen_sp_loss_pos)
plot(hansen_sp_loss_pos)
save(hansen_sp_loss_pos, file = "data/output/hansen_sp_loss_pos2018_dur.RData")

# ** Forest loss - richness losses ----
# 2000 - 2016

slopes_forest_neg <- filter(slopes_forest, slope < 0)
slopes_forest_neg3 <- filter(slopes_forest_neg, sum_loss_km > 0.5)
slopes_forest_neg3$sum_loss_km_scaled <- scale(slopes_forest_neg3$sum_loss_km, center = T)

# Scale duration column
slopes_forest_neg3$duration.scaled <- scale(slopes_forest_neg3$duration, center = T)
summary(slopes_forest_neg3$duration.scaled)  # centered on zero

hansen_sp_loss_neg <- brm(bf(slope ~ sum_loss_km_scaled + duration.scaled + (1|Biome)), 
                          data = slopes_forest_neg3, 
                          prior = hier_prior_random2, iter = 6000,
                          warmup = 2000,
                          inits = '0',
                          control = list(adapt_delta = 0.89),
                          cores = 2, chains = 2)

# Check model and save outputs
summary(hansen_sp_loss_neg)
plot(hansen_sp_loss_neg)
save(hansen_sp_loss_neg, file = "data/output/hansen_sp_loss_neg2018_dur.RData")

# Turnover ----

# Turnover in the periods before and after peak forest loss
# Measured as Jaccard's dissimilarity relative to the first year of the period
# 0 is exactly the same species composition, 1 is completely different

# ** Forest loss - turnover ----
load("data/output/slopes_luh_Jtu2018.RData")

# Calculate duration
slopes_luh_Jtu$duration <- slopes_luh_Jtu$endYear - slopes_luh_Jtu$startYear

slopes_luh_Jtu <- filter(slopes_luh_Jtu, forest.diff > 0.05)
slopes_luh_Jtu$forest.diff_scaled <- scale(slopes_luh_Jtu$forest.diff, center = T)

# Scale duration column
slopes_luh_Jtu$duration.scaled <- scale(slopes_luh_Jtu$duration, center = T)
summary(slopes_luh_Jtu$duration.scaled)  # centered on zero

# Set priors for a zero-one inflated beta model
# This model was chosen because turnover is bound between 0 and 1
# There are zeros and ones in the data but also continuous values in between

prior2b <- c(set_prior(prior = 'normal(0,6)', class='b', coef='duration.scaled'),
             set_prior(prior = 'normal(0,6)', class='b', coef='forest.diff_scaled'), 	# global slope
             set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
             set_prior("normal(0,.5)", class = "Intercept", dpar = "zoi"),
             set_prior("normal(0,.5)", class = "Intercept", dpar = "coi"), 		
             set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

# zoi refers to the probability of being a zero or a one

# coi refers to the conditional probability of being a one 
# (given an observation is a zero or a one)

# phi is the precision parameter of zero-one inflated beta distribution

Jtu_luh_cont <- brm(bf(final_tu ~ forest.diff_scaled + duration.scaled + (1|Biome), 
                       coi ~ 1, zoi ~ 1),
                    family = zero_one_inflated_beta(),
                    data = slopes_luh_Jtu, 
                    prior = prior2b, iter = 6000,
                    warmup = 2000,
                    inits = '0',
                    control = list(adapt_delta = 0.89),
                    cores = 2, chains = 2)

# Check model and save outputs
summary(Jtu_luh_cont)
plot(Jtu_luh_cont)
save(Jtu_luh_cont, file = "data/output/Jtu_luh_cont2018_dur.RData")


# ** Forest gain - turnover ----
# 2000 - 2016
# Load in Hansen GFC data
load("data/input/rarefied_medians2018.RData")

# Extract turnover relative to 2000 
# (to match the temporal scale of the GFC dataset)
Jtu_finals2000 <- rarefied_medians2018 %>% dplyr::select(rarefyID, YEAR, Jtu_base) %>%
  filter(YEAR > 2000)

Jtu_finals2000 <- Jtu_finals2000 %>% group_by(rarefyID) %>%
  mutate(max.year = max(YEAR),
         test = max.year == YEAR) %>%
  filter(test == TRUE) %>% distinct()
colnames(Jtu_finals2000)[3] <- "final_tu"
slopes_forest <- inner_join(slopes_forest, Jtu_finals2000, by = "rarefyID") %>%
  distinct()

# Set priors
prior4b <- c(set_prior(prior = 'normal(0,6)', class='b', coef='duration.scaled'),
             set_prior(prior = 'normal(0,6)', class='b', coef='sum_gain_km_scaled'), 	# global slope
             set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
             set_prior("normal(0,.5)", class = "Intercept", dpar = "zoi"),
             set_prior("normal(0,.5)", class = "Intercept", dpar = "coi"), 		
             set_prior(prior = 'cauchy(0,2)', class='sd'))  	# group-level intercepts

slopes_forest6 <- filter(slopes_forest, sum_gain_km > 0.5)
slopes_forest6$sum_gain_km_scaled <- scale(slopes_forest6$sum_gain_km, center = T)

# Scale duration column
slopes_forest6$duration.scaled <- scale(slopes_forest6$duration, center = T)
summary(slopes_forest6$duration.scaled)  # centered on zero

Jtu_hansen_gain_cont <- brm(bf(final_tu ~ sum_gain_km_scaled + (1|Biome), coi ~ 1, zoi ~ 1), 
                            family = zero_one_inflated_beta(),
                            data = slopes_forest6, 
                            prior = prior4b, iter = 6000,
                            warmup = 2000,
                            inits = '0',
                            control = list(adapt_delta = 0.89),
                            cores = 2, chains = 2)

summary(Jtu_hansen_gain_cont)[17]
plot(Jtu_hansen_gain_cont)
save(Jtu_hansen_gain_cont, file = "data/output/Jtu_hansen_gain_cont2018.RData")

# ** Forest loss -  turnover ----
# 2000 - 2016
slopes_forest5 <- filter(slopes_forest, sum_loss_km > 0.5)
slopes_forest5$sum_loss_km_scaled <- scale(slopes_forest5$sum_loss_km, center = T)

prior3b <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'),
             set_prior(prior = 'normal(0,6)', class='b', coef='sum_loss_km_scaled'), 	# global slope
             set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
             set_prior("normal(0,.5)", class = "Intercept", dpar = "zoi"),
             set_prior("normal(0,.5)", class = "Intercept", dpar = "coi"), 		
             set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

Jtu_hansen_loss_cont <- brm(bf(final_tu ~ sum_loss_km_scaled + (1|Biome), 
                               coi ~1, zoi ~ 1),
                            family = zero_one_inflated_beta(), 
                            data = slopes_forest5, 
                            prior = prior3b, iter = 6000,
                            warmup = 2000,
                            inits = '0',
                            control = list(adapt_delta = 0.89),
                            cores = 2, chains = 2)

# Check model and save outputs
summary(Jtu_hansen_loss_cont)
plot(Jtu_hansen_loss_cont)
save(Jtu_hansen_loss_cont, file = "data/output/Jtu_hansen_loss_cont2018.RData")
