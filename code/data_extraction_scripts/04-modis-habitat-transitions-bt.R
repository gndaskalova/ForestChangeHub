# MODIS transitions
setwd("~/Downloads/modis biotime")
# Files stored locally - extracted with the Google Earth Engine
# see script 02-modis.js for reference


temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

MODIS_2001 <- MODIS_2001 %>% select(2:6)
MODIS_2002 <- MODIS_2002 %>% select(2:5)
MODIS_2003 <- MODIS_2003 %>% select(2:5)
MODIS_2004 <- MODIS_2004 %>% select(2:4)
MODIS_2005 <- MODIS_2005 %>% select(2:4)
MODIS_2006 <- MODIS_2006 %>% select(2:4)
MODIS_2007 <- MODIS_2007 %>% select(2:4)
MODIS_2008 <- MODIS_2008 %>% select(2:4)
MODIS_2009 <- MODIS_2009 %>% select(2:5)
MODIS_2010 <- MODIS_2010 %>% select(2:5)
MODIS_2011 <- MODIS_2011 %>% select(2:6)
MODIS_2012 <- MODIS_2012 %>% select(2:6)
MODIS_2013 <- MODIS_2013 %>% select(2:4)

MODIS_2001$year <- "2001"
MODIS_2002$year <- "2002"
MODIS_2003$year <- "2003"
MODIS_2004$year <- "2004"
MODIS_2005$year <- "2005"
MODIS_2006$year <- "2006"
MODIS_2007$year <- "2007"
MODIS_2008$year <- "2008"
MODIS_2009$year <- "2009"
MODIS_2010$year <- "2010"
MODIS_2011$year <- "2011"
MODIS_2012$year <- "2012"
MODIS_2013$year <- "2013"

MODIS <- full_join(MODIS_2001, MODIS_2002)
MODIS <- full_join(MODIS, MODIS_2003)
MODIS <- full_join(MODIS, MODIS_2004)
MODIS <- full_join(MODIS, MODIS_2006)
MODIS <- full_join(MODIS, MODIS_2007)
MODIS <- full_join(MODIS, MODIS_2008)
MODIS <- full_join(MODIS, MODIS_2009)
MODIS <- full_join(MODIS, MODIS_2010)
MODIS <- full_join(MODIS, MODIS_2011)
MODIS <- full_join(MODIS, MODIS_2012)
MODIS <- full_join(MODIS, MODIS_2013)

MODIS <- MODIS %>% select(rarefyID, year, X6, X7, X9, X10, X12)
colnames(MODIS) <- c("rarefyID", "year", "closed.shrubland", "open.shrubland",
                     "savannah.trees", "grassland", "cropland")

# Dominant type in each year
MODIS2 <- replace(MODIS, is.na(MODIS), 0)
biotime_states <- MODIS2[, 3:7]
dominant_start <- as.data.frame(colnames(biotime_states)[max.col(biotime_states,ties.method="first")])
colnames(dominant_start) <- "dominant_cover"
biotime_states_modis <- cbind(MODIS2, dominant_start)

MODIS_start <- biotime_states_modis %>% filter(year == 2001)
MODIS_end <- biotime_states_modis %>% filter(year == 2013)
MODIS_end <- MODIS_end %>% dplyr::select(rarefyID, dominant_cover)
colnames(MODIS_end) <- c("rarefyID", "dominant_cover_end")
MODIS_transitions <- inner_join(MODIS_start, MODIS_end)
MODIS_transitions$test <- MODIS_transitions$dominant_cover == MODIS_transitions$dominant_cover_end

transitions <- filter(MODIS_transitions, test == FALSE)
transitions$change_type <- paste0(transitions$dominant_cover, sept = "_", transitions$dominant_cover_end)

library(ggstatsplot)
(pies <- ggstatsplot::ggpiestats(data = transitions,
                        main = change_type))

ggsave(pies, filename = "figures/MODIS_transitions.png",
       height = 12, width = 15)

no_transitions <- filter(MODIS_transitions, test == TRUE)
no_transitions$change_type <- "No transition"

all_cells <- rbind(transitions, no_transitions)

(pies2 <- ggstatsplot::ggpiestats(data = all_cells,
                                 main = change_type))

ggsave(pies2, filename = "figures/MODIS_transitions_all_cells.png",
       height = 12, width = 15)

# Merge with sp richness change and turnover change from 2001 to 2013
load("data/output/slopes_richness_modis.RData")
slopes_modis$rarefyID <- rownames(slopes_modis)

transitions_sp <- inner_join(transitions, slopes_modis, by = "rarefyID")

theme_LPI <- function(){
  theme_bw() +
    theme(text = element_text(family = "Helvetica Light"),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 18),
          axis.line.x = element_line(color="black"), 
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 18, vjust = 1, hjust = 0),
          legend.text = element_text(size = 12),          
          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9), 
          legend.key = element_blank(),
          legend.background = element_rect(color = "black", 
                                           fill = "transparent", 
                                           size = 2, linetype = "blank"))
}

transitions_sp <- arrange(transitions_sp, desc(change_type))

transitions_sp <- distinct(transitions_sp)

top_transitions_sp <- transitions_sp %>%
  group_by(change_type) %>%
  mutate(count = length(unique(rarefyID))) %>%
  filter(count > 10)

top_transitions_sp$change_type <- factor(top_transitions_sp$change_type,
                                         levels = c("savannah.trees_grassland",
                                                    "savannah.trees_closed.shrubland",
                                                    "grassland_closed.shrubland",
                                                    "cropland_closed.shrubland",
                                                    "cropland_open.shrubland",
                                                    "cropland_grassland"),
                                         labels = c("Savannah to grassland",
                                                    "Savannah to closed shrubland",
                                                    "Grassland to closed shrubland",
                                                    "Cropland to closed shrubland",
                                                    "Cropland to open shrubland",
                                                    "Cropland to grassland"))

(transitions_modis_sp <- ggplot(top_transitions_sp, 
                               aes(x = rarefyID.Estimate.year.scaled,
                                   y = change_type)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    geom_density_ridges2(alpha = 0.6, colour = "#C65559", fill = "#C65559", scale = 0.9) +
    geom_vline(xintercept = 0, colour = "grey20", linetype = "dashed") +
    scale_x_continuous(limits = c(-0.4, 0.4)) +
    theme_LPI() +
    labs(y = NULL, x = "\nRichness change", title = "Richness change\n"))

ggsave(transitions_modis_sp, filename = "figures/sept/modis_richness.png",
       height = 5, width = 7)

# Extract turnover slopes for period overlapping with MODIS
load("data/input/rarefied_mediansOct2017.RData")
Jtu_finals2000 <- rarefied_medians2018 %>% dplyr::select(rarefyID, YEAR, Jtu_base) %>%
  filter(YEAR > 2000)
Jtu_finals2000 <- Jtu_finals2000 %>% group_by(rarefyID) %>%
  mutate(duration = length(unique(YEAR))) %>%
  filter(duration > 4)
Jtu_finals2000 <- Jtu_finals2000 %>% group_by(rarefyID) %>%
  mutate(max.year = max(YEAR),
         test = max.year == YEAR) %>%
  filter(test == TRUE)
Jtu_finals2000 <- distinct(Jtu_finals2000)

transitions_tu <- inner_join(transitions, Jtu_finals2000, by = "rarefyID")

transitions_tu <- distinct(transitions_tu)

top_transitions_tu <- transitions_tu %>%
  group_by(change_type) %>%
  mutate(count = length(unique(rarefyID))) %>%
  filter(count > 10)

unique(top_transitions_tu$change_type)

top_transitions_tu$change_type <- factor(top_transitions_tu$change_type,
                                         levels = c("savannah.trees_grassland",
                                                    "savannah.trees_closed.shrubland",
                                                    "grassland_closed.shrubland",
                                                    "cropland_closed.shrubland",
                                                    "cropland_open.shrubland",
                                                    "cropland_grassland"),
                                         labels = c("Savannah to grassland",
                                                    "Savannah to closed shrubland",
                                                    "Grassland to closed shrubland",
                                                    "Cropland to closed shrubland",
                                                    "Cropland to open shrubland",
                                                    "Cropland to grassland"))

(transitions_modis_tu <- ggplot(top_transitions_tu, 
                                aes(x = Jtu_base,
                                    y = change_type)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    geom_density_ridges2(alpha = 0.6, colour = "#578988", fill = "#578988",
                         scale = 0.9) +
    scale_x_continuous(limits = c(0, 1),
                       breaks = c(0, 0.25, 0.50, 0.75, 1),
                       labels = c("0", "0.25", "0.50", "0.75", "1.00")) +
    theme_LPI() +
    labs(y = NULL, x = "\nTurnover", title = "Turnover\n"))

ggsave(transitions_modis_tu, filename = "figures/sept/modis_turnover.png",
       height = 5, width = 7)

# MODIS and pop change
mus_modis <- read.csv("~/Downloads/mus_modis.csv")
load("data/output/transitions_modis_lpd.RData")

transitions_pop <- inner_join(transitions_lpd, mus_modis, by ="id")
transitions_pop <- distinct(transitions_pop)

top_transitions_pop <- transitions_pop %>%
  group_by(change_type) %>%
  mutate(count = length(unique(id))) %>%
  filter(count > 10)

top_transitions_pop$change_type <- factor(top_transitions_pop$change_type,
                                         levels = c("woody.savannah_mixed.forest",
                                                    "savannas_woody.savannah"),
                                         labels = c("Woody savannah to mixed forest",
                                                    "Savannah to woody savannah"))

(transitions_modis_pop <- ggplot(top_transitions_pop, 
                                aes(x = mu,
                                    y = change_type)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    geom_density_ridges2(alpha = 0.6, colour = "#CC9145", fill = "#CC9145",
                         scale = 0.9) +
    geom_vline(xintercept = 0, colour = "grey20", linetype = "dashed") +
    scale_x_continuous(limits = c(-0.4, 0.4)) +
    theme_LPI() +
    labs(y = NULL, x = bquote(atop(' ', italic(mu))), title = "Population change\n"))

ggsave(transitions_modis_pop, filename = "figures/sept/modis_popchange.png",
       height = 3.5, width = 7)
