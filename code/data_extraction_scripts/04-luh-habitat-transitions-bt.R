# Combine LUH states and determine dominant type for each cell
# before and after the biggest forest change event

load("data/output/LUH_polys_c3ann.RData")
load("data/output/LUH_polys_c3nfx.RData")
load("data/output/LUH_polys_c3per.RData")
load("data/output/LUH_polys_c4ann.RData")
load("data/output/LUH_polys_c4per.RData")
load("data/output/LUH_polys_long.RData")
load("data/output/LUH_polys_pastr.RData")
load("data/output/LUH_polys_primn.RData")
load("data/output/LUH_polys_range.RData")
load("data/output/LUH_polys_secdf.RData")
load("data/output/LUH_polys_secdn.RData")
load("data/output/LUH_polys_urban.RData")

library(dplyr)
library(tidyr)

luh_states <- cbind(luh_polys_c3ann, luh_polys_c3nfx[,3])
luh_states <- cbind(luh_states, luh_polys_c3per[,3])
luh_states <- cbind(luh_states, luh_polys_c4ann[,3])
luh_states <- cbind(luh_states, luh_polys_c4per[,3])
luh_states <- cbind(luh_states, luh_polys_long[,3])
luh_states <- cbind(luh_states, luh_polys_pastr[,3])
luh_states <- cbind(luh_states, luh_polys_primn[,3])
luh_states <- cbind(luh_states, luh_polys_range[,3])
luh_states <- cbind(luh_states, luh_polys_secdf[,3])
luh_states <- cbind(luh_states, luh_polys_secdn[,3])
luh_states <- cbind(luh_states, luh_polys_urban[,3])

colnames(luh_states) <- c("rarefyID", "year", "c3ann",
                          "c3nfx", "c3per", "c4ann",
                          "c4per", "primf", "pastr",
                          "primn", "range", "secdf",
                          "secdn","urban")
save(luh_states, file = "data/output/luh_states_wide.RData")

luh_states_long <- gather(luh_states, type, cover, select = 3:14)
luh_states_long$type <- as.factor(luh_states_long$type)
save(luh_states_long, file = "data/output/luh_states_long.RData")

trial <- luh_states_long %>% group_by(rarefyID, year) %>% top_n(1) %>%
  distinct()

load("data/input/rarefied_mediansOct2017.Rdata")
studies <- filter(rarefied_medians, REALM == "Terrestrial") %>%
  dplyr::select(rarefyID, rarefyID_x, rarefyID_y, startYear, endYear) %>%
  distinct()

luh_states_start <- inner_join(luh_states_long, studies, by = "rarefyID")
luh_states_start$test <- luh_states_start$year == luh_states_start$startYear
luh_states_start <- luh_states_start %>% filter(test == TRUE) %>% distinct()
save(luh_states_start, file = "data/output/luh_states_start.RData")

luh_states_end <- inner_join(luh_states_long, studies, by = "rarefyID")
luh_states_end$test <- luh_states_end$year == luh_states_end$endYear
luh_states_end <- luh_states_end %>% filter(test == TRUE) %>% distinct()
save(luh_states_end, file = "data/output/luh_states_end.RData")

luh_states_start_wide <- inner_join(luh_states, studies, by = "rarefyID")
luh_states_start_wide$test <- luh_states_start_wide$year == luh_states_start_wide$startYear
luh_states_start_wide <- luh_states_start_wide %>% filter(test == TRUE) %>% distinct()
save(luh_states_start_wide, file = "data/output/luh_states_start_wide.RData")

luh_states_end_wide <- inner_join(luh_states, studies, by = "rarefyID")
luh_states_end_wide$test <- luh_states_end_wide$year == luh_states_end_wide$endYear
luh_states_end_wide <- luh_states_end_wide %>% filter(test == TRUE) %>% distinct()
save(luh_states_end_wide, file = "data/output/luh_states_end_wide.RData")
