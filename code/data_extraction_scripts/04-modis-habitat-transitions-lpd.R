# LPD_MODIS transitions
setwd("~/Downloads/modis lpd")
# Files stored locally - extracted with the Google Earth Engine
# see script 02-modis.js for reference

temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

LPD_MODIS_2001 <- MODIS_2001_LPD %>% select(1:4)
LPD_MODIS_2002 <- MODIS_2002_LPD %>% select(1:4)
LPD_MODIS_2003 <- MODIS_2003_LPD %>% select(1:4)
LPD_MODIS_2004 <- MODIS_2004_LPD %>% select(1:4)
LPD_MODIS_2005 <- MODIS_2005_LPD %>% select(1:5)
LPD_MODIS_2006 <- MODIS_2006_LPD %>% select(1:4)
LPD_MODIS_2007 <- MODIS_2007_LPD %>% select(1:5)
LPD_MODIS_2008 <- MODIS_2008_LPD %>% select(1:5)
LPD_MODIS_2009 <- MODIS_2009_LPD %>% select(1:5)
LPD_MODIS_2010 <- MODIS_2010_LPD %>% select(1:5)
LPD_MODIS_2011 <- MODIS_2011_LPD %>% select(1:6)
LPD_MODIS_2012 <- MODIS_2012_LPD %>% select(1:6)
LPD_MODIS_2013 <- MODIS_2013_LPD %>% select(1:6)

LPD_MODIS_2001$year <- "2001"
LPD_MODIS_2002$year <- "2002"
LPD_MODIS_2003$year <- "2003"
LPD_MODIS_2004$year <- "2004"
LPD_MODIS_2005$year <- "2005"
LPD_MODIS_2006$year <- "2006"
LPD_MODIS_2007$year <- "2007"
LPD_MODIS_2008$year <- "2008"
LPD_MODIS_2009$year <- "2009"
LPD_MODIS_2010$year <- "2010"
LPD_MODIS_2011$year <- "2011"
LPD_MODIS_2012$year <- "2012"
LPD_MODIS_2013$year <- "2013"

LPD_MODIS <- full_join(LPD_MODIS_2001, LPD_MODIS_2002)
LPD_MODIS <- full_join(LPD_MODIS, LPD_MODIS_2003)
LPD_MODIS <- full_join(LPD_MODIS, LPD_MODIS_2004)
LPD_MODIS <- full_join(LPD_MODIS, LPD_MODIS_2006)
LPD_MODIS <- full_join(LPD_MODIS, LPD_MODIS_2007)
LPD_MODIS <- full_join(LPD_MODIS, LPD_MODIS_2008)
LPD_MODIS <- full_join(LPD_MODIS, LPD_MODIS_2009)
LPD_MODIS <- full_join(LPD_MODIS, LPD_MODIS_2010)
LPD_MODIS <- full_join(LPD_MODIS, LPD_MODIS_2011)
LPD_MODIS <- full_join(LPD_MODIS, LPD_MODIS_2012)
LPD_MODIS <- full_join(LPD_MODIS, LPD_MODIS_2013)

LPD_MODIS <- LPD_MODIS %>% select(system.index, year, X2, X4, X5, X8, X9, X14)
colnames(LPD_MODIS) <- c("id", "year", "evergr.broadl.f", "decid.broadl.f",
                     "mixed.forest", "woody.savannah", "savannas", "cropland")

# Dominant type in each year
LPD_MODIS2 <- replace(LPD_MODIS, is.na(LPD_MODIS), 0)
lpd_states <- LPD_MODIS2[, 3:7]
dominant_start <- as.data.frame(colnames(lpd_states)[max.col(lpd_states,ties.method="first")])
colnames(dominant_start) <- "dominant_cover"
states_LPD_MODIS <- cbind(LPD_MODIS2, dominant_start)

LPD_MODIS_start <- states_LPD_MODIS %>% filter(year == 2001)
LPD_MODIS_end <- states_LPD_MODIS %>% filter(year == 2013)
LPD_MODIS_end <- LPD_MODIS_end %>% dplyr::select(id, dominant_cover)
colnames(LPD_MODIS_end) <- c("id", "dominant_cover_end")
LPD_MODIS_transitions <- inner_join(LPD_MODIS_start, LPD_MODIS_end)
LPD_MODIS_transitions$test <- LPD_MODIS_transitions$dominant_cover == LPD_MODIS_transitions$dominant_cover_end

transitions_lpd <- filter(LPD_MODIS_transitions, test == FALSE)
transitions_lpd$change_type <- paste0(transitions_lpd$dominant_cover, sept = "_", transitions_lpd$dominant_cover_end)

library(ggstatsplot)
(pies <- ggstatsplot::ggpiestats(data = transitions_lpd,
                                 main = change_type))

ggsave(pies, filename = "figures/LPD_MODIS_transitions.png",
       height = 12, width = 15)

no_transitions <- filter(LPD_MODIS_transitions, test == TRUE)
no_transitions$change_type <- "No transition"

all_cells <- rbind(transitions, no_transitions)

(pies2 <- ggstatsplot::ggpiestats(data = all_cells,
                                  main = change_type))

ggsave(pies2, filename = "figures/LPD_MODIS_transitions_all_cells.png",
       height = 12, width = 15)

save(transitions_lpd, file = "data/output/transitions_modis_lpd.RData")
