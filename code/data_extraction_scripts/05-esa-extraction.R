# ESA forest cover change extraction

library(raster)
library(tidyverse)
library(gridExtra)
library(rgdal)

setwd("~/Downloads")
esa <- stack("scratch/esa.tif")  # Locally stored cause it's a big file
str(esa)
plot(esa)
names(esa)
summary(esa[1])

setwd("~/LandUseHub")

# Seeing what happens if I extract for a few points

load("~/LandUseHub/data/input/rarefied_mediansOct2017.Rdata")

baby_bt <- rarefied_medians[1:6,]

coords_sp <- SpatialPoints(cbind(baby_bt$rarefyID_x, baby_bt$rarefyID_y), 
                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Transforming coordinate system to that of the driver data
coords_sp <- spTransform(coords_sp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
coords_df <- as.data.frame(coords_sp@coords)
colnames(coords_df) <- c("long", "lat")

test <- raster::extract(esa, coords_df)

# Creating corners at the appropriate locations around the locations of the time-series
coords_df$atop <- coords_df$lat + 0.04413495
coords_df$bottom <- coords_df$lat - 0.04413495
coords_df$leftb <- coords_df$long - 0.04413495
coords_df$left <- coords_df$long + 0.04413495

# Creating spatial polygons
coords_df$rarefyID <- baby_bt$rarefyID
coords <- coords_df %>% dplyr::select(left, leftb, atop, bottom, rarefyID)
coords3 <- coords %>% gather(type, lon, select = c(1:2))
coords3 <- coords3 %>% gather(direction, lat, select = c(1:2))
coords3 <- coords3 %>% arrange(rarefyID)

coords4 <- coords3 %>% group_by(rarefyID) %>% filter(row_number() == 1)
coords4$order <- "2"
coords3$order <- "1"
coords5 <- full_join(coords3, coords4)
coords5$sort <- paste0(coords5$type, coords5$direction)
coords5 <- coords5 %>% arrange(rarefyID, order, sort)

df_to_spp <- coords5 %>%
  group_by(rarefyID) %>%
  do(poly = dplyr::select(., lon, lat) %>% Polygon()) %>%
  rowwise() %>%
  do(polys = Polygons(list(.$poly),.$rarefyID)) %>%
  {SpatialPolygons(.$polys)}

df14 <- raster::extract(esa, df_to_spp, df = TRUE, cellnumbers = TRUE)
