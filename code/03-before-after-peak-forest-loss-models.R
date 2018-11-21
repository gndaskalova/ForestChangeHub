# Do population and biodiversity change differ before and after peak forest loss?
# Gergana Daskalova
# Sept 2018
# gndaskalova@gmail.com

# Data publicly available from the BioTIME website
# the Living Planet website
# and the websites for the Global Forest Change 
# and Land Use Harmonisation datasets

library(brms)
library(dplyr)

# Pop change before/after ----
# Load time series duration
load("data/input/vert_periods.RData")
lpi_min_years <- vert_periods %>%
  dplyr::select(id, min_years)

# Load population change values in the periods before and after peak forest loss
mus_period1 <- read.csv("data/input/global_mus_period1_even.csv")
mus_period2 <- read.csv("data/input/global_mus_period2_even.csv")

# Bind dataframes for analysis
mus_period1$period <- "Before"
mus_period2$period <- "After"
mus_periods <- rbind(mus_period1, mus_period2)

# Add a column for duration
mus_periods <- inner_join(mus_periods, lpi_min_years, by = "id") %>%
  distinct()

# Rearrange data so that we get estimates for population change after forest loss
mus_periods$period <- factor(mus_periods$period, levels = c("Before", "After"),
                             labels = c("Before", "After"))

mus_periods_neg <- mus_periods %>% filter(mu < 0)
# Scale duration column
mus_periods_neg$min_years.scaled <- scale(mus_periods_neg$min_years, center = T)
summary(mus_periods_neg$min_years.scaled)  # centered on zero

# Pop change before/after model

# Set priors
hier_prior_random <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'), 	# global slope
                       set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                       set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

# Biome random effect included to account for the spatial clustering of the data

# ** Model for declines ----
ba_pop_neg <- brm(bf(mu ~ period + min_years.scaled + 
                   (1|biome)), 
              data = mus_periods_neg, 
              prior = hier_prior_random, iter = 6000,
              warmup = 2000,
              inits = '0',
              control = list(adapt_delta = 0.89),
              cores = 2, chains = 2)

# Check model and save output
summary(ba_pop_neg)
plot(ba_pop_neg)
save(ba_pop_neg, file = "data/output/ba_pop_neg2018.RData")

# ** Model for increases ----
mus_periods_pos <- mus_periods %>% filter(mu > 0)
mus_periods_pos$min_years.scaled <- scale(mus_periods_pos$min_years, center = T)
summary(mus_periods_pos$min_years.scaled)  # centered on zero

ba_pop_pos <- brm(bf(mu ~ period + min_years.scaled +
                       (1|biome)), 
                  data = mus_periods_pos, 
                  prior = hier_prior_random, iter = 6000,
                  warmup = 2000,
                  inits = '0',
                  control = list(adapt_delta = 0.89), # use 0.80 for early model runs, and increase
                  cores = 2, chains = 2)

# Check model and save output
summary(ba_pop_pos)
plot(ba_pop_pos)
save(ba_pop_pos, file = "data/output/ba_pop_pos2018.RData")

# ** Magnitude of forest loss peak ----
# Do largers peaks in forest cover change correspond with more pop change?
mus2 <- mus_period2 %>% dplyr::select(id, mu)
colnames(mus2) <- c("id", "mu_after")
mus_period1$min_years.scaled <- as.numeric(mus_period1$min_years.scaled)
mus_period1 <- left_join(mus_period1, mus2, by = "id")

mus_period1$diff <- abs(mus_period1$mu_after) - abs(mus_period1$mu)
mus_period1$abs.diff <- abs(mus_period1$mu_after - mus_period1$mu)

load("data/output/LUH_polys_long_lpi_max.RData") 
# Magnitude of peak forest loss data

luh_polys_lpi_max <- luh_polys_lpi_max %>%
  dplyr:: select(id, max.loss)

mus_period1 <- inner_join(mus_period1, luh_polys_lpi_max, by = "id")
mus_period1 <- inner_join(mus_period1, lpi_min_years, by = "id")
mus_period1 <- distinct(mus_period1)

hier_prior_random3 <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'), 	# global slope
                        set_prior(prior = 'normal(0,6)', class='b', coef='max.loss.scaled'), 	# global slope
                        set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                        set_prior(prior = 'cauchy(0,2)', class='sd')) 	# group-level intercepts

mus_period1$max.loss <- abs(as.numeric(mus_period1$max.loss))
mus_period1$max.loss.scaled <- scale(mus_period1$max.loss, center = T)
summary(mus_period1$max.loss.scaled)  # centered on zero
summary(mus_period1$min_years.scaled)  # centered on zero

ba_pop_magnitude <- brm(bf(diff ~ max.loss.scaled + min_years.scaled + 
                             (1|biome)), 
                        data = mus_period1, 
                        prior = hier_prior_random3, iter = 6000,
                        warmup = 2000,
                        inits = '0',
                        control = list(adapt_delta = 0.89),
                        cores = 2, chains = 2)

summary(ba_pop_magnitude)
plot(ba_pop_magnitude)
save(ba_pop_magnitude, file = "data/output/ba_pop_magnitude2018.RData")

# ** Magnitude of pop change ----
# Are trends becoming more acute?
mus_pos_p <- filter(mus_period1, mu > 0)
mus_neg_p <- filter(mus_period1, mu < 0)

mus_pos_p$years_scaled <- scale(mus_pos_p$End, center = T)

# Models
higher_prior_random5 <- c(set_prior(prior = 'normal(0,6)', class='b', coef='years_scaled'), 	# global slope
                          set_prior(prior = 'normal(0,6)', class='Intercept', coef=''),  # global intercept
                          set_prior(prior = 'cauchy(0,2)', class='sd'))	 # group-level intercepts

# Testing if the difference in population change on the individual time series
# level before and after peak forest loss is different from zero

# For the increasing populations
ind_pop_pos <- brm(bf(diff ~ 1 + years_scaled  +
                        (1|biome)),
                   data = mus_pos_p, iter = 6000,
                   prior = higher_prior_random5,
                   warmup = 2000,
                   inits = '0',
                   control = list(adapt_delta = 0.89),
                   cores = 2, chains = 2)

summary(ind_pop_pos)
plot(ind_pop_pos)
save(ind_pop_pos, file = "data/output/ind_pop_pos2018.RData")

# For the decreasing populations
mus_neg_p$years_scaled <- scale(mus_neg_p$End, center = T)

ind_pop_neg <- brm(bf(diff ~ 1 + years_scaled +
                        (1|biome)),
                   data = mus_neg_p, iter = 6000,
                   prior = higher_prior_random5,
                   warmup = 2000,
                   inits = '0',
                   control = list(adapt_delta = 0.89),
                   cores = 2, chains = 2)

# Check model and save output
summary(ind_pop_neg)
plot(ind_pop_neg)
save(ind_pop_neg, file = "data/output/ind_pop_neg2018.RData")

# Population change before, during & after historic all time peak forest loss
load("data/output/mus_bf_af_during.RData")

# Set prior
prior_bf_af_du <- c(set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

mus_bf_af_during$period <- factor(mus_bf_af_during$period,
                                  levels = c("Before", "During", "After"),
                                  labels = c("Before", "During", "After"))

pop_bf_af_du <- brm(bf(mu ~ period + (1|biome)), 
                    data = mus_bf_af_during, 
                    prior = prior_bf_af_du, iter = 6000,
                    warmup = 2000,
                    inits = '0',
                    control = list(adapt_delta = 0.89), # use 0.80 for early model runs, and increase
                    cores = 2, chains = 2)

# Check model and save output
summary(pop_bf_af_du)
plot(pop_bf_af_du)
save(pop_bf_af_du, file = "data/output/pop_bf_af_du2018.RData")

# Sp richness change before/after ----
load("data/output/slopes_period1.RData")
# Slopes of richness change before peak forest loss
load("data/output/slopes_period2.RData")
# Slopes of richness change after forest loss

# For all species richness models below "rarefyID.Estimate.year.scaled"
# is the estimate for the slope of richness change over time 
# derived following the modelling framework in the file "02-richness-models.R"

# Create a column for time series IDs 
# (currently the rownames are the rarefyID column)
slopes_period1$rarefyID <- rownames(slopes_period1)
slopes_period2$rarefyID <- rownames(slopes_period2)

slopes_period1$period <- "Before"
slopes_period2$period <- "After"

# ** Model for gains ----
slopes_p1_pos <- slopes_period1 %>% filter(rarefyID.Estimate.year.scaled > 0)
slopes_p2_pos <- slopes_period2 %>% filter(rarefyID.Estimate.year.scaled > 0)

# Bind together for analysis
slopes_periods_pos <- rbind(slopes_p1_pos, slopes_p2_pos)

# Rearrange data frame so that we get estimates for the period 
# after peak forest loss
slopes_periods_pos$period <- factor(slopes_periods_pos$period, 
                                levels = c("Before", "After"),
                                labels = c("Before", "After"))

# Set priors
hier_prior_random_sp <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'), 	# global slope
                          set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                          set_prior(prior = 'cauchy(0,2)', class='sd'))		# group-level intercepts

load("data/input/min_years_bt.RData")  
# min_years is the duration of biodiversity monitoring 
# before/after peak forest cover change

slopes_periods_pos <- inner_join(slopes_periods_pos, min_years, by = "rarefyID") %>%
  distinct()

# Scale the duration column (mean centered on zero)
slopes_periods_pos$min_years.scaled <- scale(slopes_periods_pos$min_years, center = T)
summary(slopes_periods_pos$min_years.scaled)  # centered on zero
slopes_periods_pos$min_years.scaled <- as.numeric(slopes_periods_pos$min_years.scaled)

# Add the biome classification to use as a random effect
# to account for the spatial clustering of time series
load("data/output/biotime_luh_polys_changeSept2018.RData")
biomes.bt <- slopes_luh %>% dplyr::select(rarefyID, Biome)

slopes_periods_pos <- inner_join(slopes_periods_pos, biomes.bt, by = "rarefyID") %>%
  distinct()

# Species richness gains before/after contemporary peak forest loss
ba_sp_pos <- brm(bf(rarefyID.Estimate.year.scaled ~ period + min_years.scaled +
                  (1|Biome)), 
             data = slopes_periods_pos, 
             prior = hier_prior_random_sp, 
             iter = 6000,
             warmup = 2000,
             inits = '0',
             control = list(adapt_delta = 0.89),
             cores = 2, chains = 2)

# Check model and save output
summary(ba_sp_pos)
plot(ba_sp_pos)
save(ba_sp_pos, file = "data/output/ba_sp_pos2018.RData")

# ** Model for losses ----
slopes_p1_neg <- slopes_period1 %>% filter(rarefyID.Estimate.year.scaled < 0)
slopes_p2_neg <- slopes_period2 %>% filter(rarefyID.Estimate.year.scaled < 0)

# Bind together for analysis
slopes_periods_neg <- rbind(slopes_p1_neg, slopes_p2_neg)

# Rearrange data frame so that we get estimates for the period 
# after peak forest loss
slopes_periods_neg$period <- factor(slopes_periods_neg$period, 
                                    levels = c("Before", "After"),
                                    labels = c("Before", "After"))

# Add duration data
slopes_periods_neg <- inner_join(slopes_periods_neg, min_years, by = "rarefyID") %>%
  distinct()

# Scale the duration column to be mean centered on zero
slopes_periods_neg$min_years.scaled <- scale(slopes_periods_neg$min_years, center = T)
summary(slopes_periods_neg$min_years.scaled)  # centered on zero
slopes_periods_neg$min_years.scaled <- as.numeric(slopes_periods_neg$min_years.scaled)

# Add the biome classification to use as a random effect
# to account for the spatial clustering of time series
slopes_periods_neg <- inner_join(slopes_periods_neg, biomes.bt, by = "rarefyID") %>%
  distinct()

# Species richness losses before/after contemporary peak forest loss
ba_sp_neg <- brm(bf(rarefyID.Estimate.year.scaled ~ period + min_years.scaled +
                      (1|Biome)), 
                 data = slopes_periods_neg, 
                 prior = hier_prior_random_sp, 
                 iter = 6000,
                 warmup = 2000,
                 inits = '0',
                 control = list(adapt_delta = 0.89), # use 0.80 for early model runs, and increase
                 cores = 2, chains = 2)

# Check model and save output
summary(ba_sp_neg)
plot(ba_sp_neg)
save(ba_sp_neg, file = "data/output/ba_sp_neg2018.RData")

# ** Magnitude of forest loss peaks ----
# Do larger peaks in forest cover change correspond with more sp richness change?
sp2 <- slopes_period2 %>% dplyr::select(rarefyID, rarefyID.Estimate.year.scaled)
colnames(sp2) <- c("rarefyID", "slope_after")
slopes_period1 <- inner_join(slopes_period1, sp2, by = "rarefyID")
slopes_period1$diff <- abs(slopes_period1$rarefyID.Estimate.year.scaled - slopes_period1$slope_after)

# Load magnitude of peak forest loss data
load("data/output/LUH_polys_long_96.RData")

# Calculate the magnitude of maximum forest loss 
# during the duration of the time series
luh_polys_max <- luh_polys_long %>% group_by(rarefyID) %>%
  mutate(max.loss = min(primf_diff, na.rm = T)) 
luh_polys_max$test <- luh_polys_max$primf_diff == luh_polys_max$max.loss
luh_polys_max <- filter(luh_polys_max, test == TRUE)

luh_polys_max <- luh_polys_max  %>%
  dplyr::select(rarefyID, year, max.loss) %>% distinct() %>% drop_na(max.loss) %>%
  dplyr::select(rarefyID, max.loss)

# Add magnitude of forest loss data and duration data (min_years column) to the 
# data frame that has the difference in richness change 
# before and after forest loss
slopes_period1 <- inner_join(slopes_period1, luh_polys_max, by = "rarefyID")
slopes_period1 <- inner_join(slopes_period1, min_years, by = "rarefyID") %>%
  distinct(slopes_period1)

hier_prior_random3 <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'), 	# global slope
                        set_prior(prior = 'normal(0,6)', class='b', coef='max.loss.scaled'), 	# global slope
                        set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                        set_prior(prior = 'cauchy(0,2)', class='sd'))		# group-level intercepts

slopes_period1$max.loss <- abs(slopes_period1$max.loss)
slopes_period1$max.loss.scaled <- scale(slopes_period1$max.loss, center = T)
summary(slopes_period1$max.loss.scaled)  # centered on zero
slopes_period1$min_years.scaled <- scale(slopes_period1$min_years, center = T)
summary(slopes_period1$min_years.scaled)  # centered on zero

# Make sure variables are numeric
slopes_period1$max.loss.scaled <- as.numeric(slopes_period1$max.loss.scaled)
slopes_period1$min_years.scaled <- as.numeric(slopes_period1$min_years.scaled)

# Add the biome classification to use as a random effect
# to account for the spatial clustering of time series
slopes_period1 <- inner_join(slopes_period1, biomes.bt, by = "rarefyID") %>%
  distinct()

ba_sp_magnitude <- brm(bf(diff ~ max.loss.scaled + min_years.scaled + 
                             (1|Biome)), 
                        data = slopes_period1, 
                        prior = hier_prior_random3, iter = 6000,
                        warmup = 2000,
                        inits = '0',
                        control = list(adapt_delta = 0.89),
                        cores = 2, chains = 2)

summary(ba_sp_magnitude)
plot(ba_sp_magnitude)
save(ba_sp_magnitude, file = "data/output/ba_sp_magnitude2018.RData")

# Turnover ----
load("data/input/tu_even1b.RData")
load("data/input/tu_even2b.RData")
# Turnover in the periods before and after peak forest loss
# Measured as Jaccard's dissimilarity relative to the first year of the period
# 0 is exactly the same species composition, 1 is completely different

# Extract turnover in the final year for each time series relative to the first
tu_even1b <- tu_even1b %>% group_by(rarefyID) %>%
  mutate(max.year = max(YEAR),
         test = max.year == YEAR) %>%
  filter(test == TRUE)

tu_even2b <- tu_even2b %>% group_by(rarefyID) %>%
  mutate(max.year = max(YEAR),
         test = max.year == YEAR) %>%
  filter(test == TRUE)

# Bind data frames together for analysis
beta_periods <- rbind(tu_even1b, tu_even2b)

# Add duration for each time series
beta_periods <- inner_join(beta_periods, min_years, by = "rarefyID") %>%
  distinct()

# Scale duration data to be mean centered on zero
beta_periods$min_years.scaled <- scale(beta_periods$min_years, center = T)
summary(beta_periods$min_years.scaled)  # centered on zero

# Make sure duration is numeric
beta_periods$min_years.scaled <- as.numeric(beta_periods$min_years.scaled)

# Add the biome classification to use as a random effect
# to account for the spatial clustering of time series
beta_periods <- inner_join(beta_periods, biomes.bt, by = "rarefyID") %>%
  distinct()

# Rearrange data frame so that we get estimates for the period 
# after peak forest loss
beta_periods$period <- factor(beta_periods$period, levels = c("Before", "After"),
                              labels = c("Before", "After"))

# Set priors for a zero-one inflated beta model
# This model was chosen because turnover is bound between 0 and 1
# There are zeros and ones in the data but also continuous values in between
prior2b <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'), 	# global slope
             set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 		# global intercept
             set_prior("normal(0,.5)", class = "Intercept", dpar = "zoi"),
             set_prior("normal(0,.5)", class = "Intercept", dpar = "coi"), 	
             set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

# zoi refers to the probability of being a zero or a one

# coi refers to the conditional probability of being a one 
# (given an observation is a zero or a one)

# phi is the precision parameter of zero-one inflated beta distribution

# Turnover before/after
ba_tu <- brm(bf(Jtu_base ~ period + min_years.scaled + 
                  (1|Biome), coi ~ 1, zoi ~ 1),
             family = zero_one_inflated_beta(), 
             data = beta_periods,
             prior = prior2b, iter = 6000,
             warmup = 2000,
             inits = '0',
             control = list(adapt_delta = 0.85),
             cores = 2, chains = 2)

# Check model and save output
summary(ba_tu)
plot(ba_tu)
save(ba_tu, file = "data/output/ba_tu2018.RData")

# ** Magnitude of forest loss peak ----

# Note that here we are using a model with a caussian distribution
# because we are modelling the difference in turnover in the period after
# peak forest loss compared to the period before peak forest loss 
# (continuous numbers)

hier_prior_random3 <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'), 	# global slope
                        set_prior(prior = 'normal(0,6)', class='b', coef='max.loss.scaled'), 	# global slope
                        set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 		# global intercept
                        set_prior(prior = 'cauchy(0,2)', class='sd'))  # group-level intercepts

# Join with data on magnitude of peak loss
tu_even1b <- left_join(tu_even1b, luh_polys_max, by = "rarefyID")
tu_even1b$max.loss <- abs(tu_even1b$max.loss)
tu_even1b$max.loss.scaled <- scale(tu_even1b$max.loss, center = T)
tu_even1b$max.loss.scaled <- as.numeric(tu_even1b$max.loss.scaled)

# Extract just turnover in the period after forest loss
tu_even2b_simple <- tu_even2b %>% dplyr::select(rarefyID, Jtu_base)
colnames(tu_even2b_simple)[2] <- "Jtu_base_p2"

# Join with the data frame that has turnover in the period before forest loss
# to calculate the difference
beta_periods_diff <- left_join(tu_even1b, tu_even2b_simple, by = "rarefyID")
beta_periods_diff$diff <- abs(beta_periods_diff$Jtu_base - beta_periods_diff$Jtu_base_p2)

# Remove NAs
beta_periods_diff <- beta_periods_diff %>% drop_na(diff)

# Add biome and duration data
beta_periods_diff <- left_join(beta_periods_diff, biomes.bt, by = "rarefyID")
beta_periods_diff <- left_join(beta_periods_diff, min_years, by = "rarefyID") %>%
  distinct()
beta_periods_diff$min_years.scaled <- scale(beta_periods_diff$min_years.x, center = T)

ba_tu_magnitude <- brm(bf(diff ~ max.loss.scaled + min_years.scaled + 
                            (1|Biome)),
                       data = beta_periods_diff, 
                       prior = hier_prior_random3, iter = 6000,
                       warmup = 2000,
                       inits = '0',
                       control = list(adapt_delta = 0.89), # use 0.80 for early model runs, and increase
                       cores = 2, chains = 2)

# Check model and save output
summary(ba_tu_magnitude)
plot(ba_tu_magnitude)
save(ba_tu_magnitude, file = "data/output/ba_tu_magnitude2018.RData")

# ** Magnitude of turnover ----
# Set prior
prior_diff <- c(set_prior(prior = 'normal(0,6)', class='b', coef='min_years.scaled'), 	# global slope
                        set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                        set_prior(prior = 'cauchy(0,2)', class='sd'))  # group-level intercepts

# Test if the difference in turnover before/after forest loss is different from zero
ba_diff <- brm(bf(diff ~ 1 + min_years.scaled + 
                            (1|Biome)),
                       data = beta_periods_diff, 
                       prior = prior_diff, iter = 6000,
                       warmup = 2000,
                       inits = '0',
                       control = list(adapt_delta = 0.89), # use 0.80 for early model runs, and increase
                       cores = 2, chains = 2)
# Check model
summary(ba_diff)
