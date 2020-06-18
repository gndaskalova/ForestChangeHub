# Combine LUH states and determine dominant type for each LPD cell
# before and after the biggest forest change event

load("data/output/LUH_polys_c3ann_lpi.RData")
load("data/output/LUH_polys_c3nfx_lpi.RData")
load("data/output/LUH_polys_c3per_lpi.RData")
load("data/output/LUH_polys_c4ann_lpi.RData")
load("data/output/LUH_polys_c4per_lpi.RData")
load("data/output/LUH_polys_long_lpi.RData")
load("data/output/LUH_polys_pastr_lpi.RData")
load("data/output/LUH_polys_primn_lpi.RData")
load("data/output/LUH_polys_range_lpi.RData")
load("data/output/LUH_polys_secdf_lpi.RData")
load("data/output/LUH_polys_secdn_lpi.RData")
load("data/output/LUH_polys_urban_lpi.RData")

library(dplyr)
library(tidyr)

luh_states <- cbind(luh_polys_c3ann_lpi, luh_polys_c3nfx_lpi[,3])
luh_states <- cbind(luh_states, luh_polys_c3per_lpi[,3])
luh_states <- cbind(luh_states, luh_polys_c4ann_lpi[,3])
luh_states <- cbind(luh_states, luh_polys_c4per_lpi[,3])
luh_states <- cbind(luh_states, luh_polys_lpi_long[,3])
luh_states <- cbind(luh_states, luh_polys_pastr_lpi[,3])
luh_states <- cbind(luh_states, luh_polys_primn_lpi[,3])
luh_states <- cbind(luh_states, luh_polys_range_lpi[,3])
luh_states <- cbind(luh_states, luh_polys_secdf_lpi[,3])
luh_states <- cbind(luh_states, luh_polys_secdn_lpi[,3])
luh_states <- cbind(luh_states, luh_polys_urban_lpi[,3])

colnames(luh_states) <- c("id", "year", "c3ann",
                          "c3nfx", "c3per", "c4ann",
                          "c4per", "primf", "pastr",
                          "primn", "range", "secdf",
                          "secdn","urban")
luh_states_lpi <- luh_states
save(luh_states_lpi, file = "data/output/luh_states_lpi_wide.RData")

luh_states_lpi_long <- gather(luh_states_lpi, type, cover, select = 3:14)
luh_states_lpi_long$type <- as.factor(luh_states_lpi_long$type)
save(luh_states_lpi_long, file = "data/output/luh_states_lpi_long.RData")

trial <- luh_states_lpi_long %>% group_by(id, year) %>% top_n(1) %>%
  distinct()

LPIdata_Feb2016 <- read.csv("data/input/LPIdata_Feb2016.csv")

# Transform from wide to long format
LPI.long <- gather(data = LPIdata_Feb2016, key = "year", value = "pop", select = 26:70)

library(readr)
library(scales)
# Get rid of the X in front of years
LPI.long$year <- parse_number(LPI.long$year)

# Create new column with genus and species together
LPI.long$species <- paste(LPI.long$Genus, LPI.long$Species)

LPI.long <- LPI.long %>%
  drop_na(pop) %>%
  group_by(id) %>%   # group rows so that each group is one population
  mutate(scalepop = rescale(pop, to = c(-1, 1))) %>%
  filter(length(unique(year)) > 5) %>%
  drop_na(scalepop) %>%
  mutate(meanpop = mean(pop),  # Create column for mean population
         minyear = min(year),
         maxyear = max(year),
         lengthyear = maxyear - minyear) %>%
  ungroup()

metas_lpi <- LPI.long %>% dplyr::select(id, minyear, maxyear)

luh_states_lpi_start <- inner_join(luh_states_lpi_long, metas_lpi, by = "id")
luh_states_lpi_start$test <- luh_states_lpi_start$year == luh_states_lpi_start$minyear
luh_states_lpi_start <- luh_states_lpi_start %>% filter(test == TRUE) %>% distinct()
save(luh_states_lpi_start, file = "data/output/luh_states_lpi_start.RData")

luh_states_lpi_end <- inner_join(luh_states_lpi_long, metas_lpi, by = "id")
luh_states_lpi_end$test <- luh_states_lpi_end$year == luh_states_lpi_end$minyear
luh_states_lpi_end <- luh_states_lpi_end %>% filter(test == TRUE) %>% distinct()
save(luh_states_lpi_end, file = "data/output/luh_states_lpi_end.RData")

luh_states_lpi_start_wide <- inner_join(luh_states_lpi, metas_lpi, by = "id")
luh_states_lpi_start_wide$test <- luh_states_lpi_start_wide$year == luh_states_lpi_start_wide$minyear
luh_states_lpi_start_wide <- luh_states_lpi_start_wide %>% filter(test == TRUE) %>% distinct()
save(luh_states_lpi_start_wide, file = "data/output/luh_states_lpi_start_wide.RData")

luh_states_lpi_end_wide <- inner_join(luh_states_lpi, metas_lpi, by = "id")
luh_states_lpi_end_wide$test <- luh_states_lpi_end_wide$year == luh_states_lpi_end_wide$minyear
luh_states_lpi_end_wide <- luh_states_lpi_end_wide %>% filter(test == TRUE) %>% distinct()
save(luh_states_lpi_end_wide, file = "data/output/luh_states_end_wide.RData")
