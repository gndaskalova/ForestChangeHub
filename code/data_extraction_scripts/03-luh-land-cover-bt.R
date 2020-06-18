# Extract land cover through time for the BioTIME sites
# Gergana Daskalova
# 9th Nov 2018
# gndaskalova@gmail.com

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(splitstackshape)
library(dplyr)
library(tidyr)
library(data.table)

nc_data <- nc_open('states.nc')  # Note - big file stored locally
# Available from the LUH website Hurtt et al.

# Save the print(nc) to a text file
{
  sink('states_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector

# Extract primary non-forest land
forest.array <- ncvar_get(nc_data, "primn") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "primn", "_fillvalue")
fillvalue

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

# Import BioTIME lat and long
load("data/input/rarefied_mediansOct2017.Rdata")

coords <- rarefied_medians %>% filter(REALM == "Terrestrial") %>%
  dplyr::select(rarefyID, rarefyID_x, rarefyID_y) %>%
  distinct()

# Creating corners at the appropriate locations around the locations of the time-series
coords$atop <- coords$rarefyID_y + 0.04413495
coords$bottom <- coords$rarefyID_y - 0.04413495
coords$leftb <- coords$rarefyID_x - 0.04413495
coords$left <- coords$rarefyID_x + 0.04413495

# Creating spatial polygons
coords <- coords %>% dplyr::select(left, leftb, atop, bottom, rarefyID)
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

## plot it
plot(df_to_spp)

luh_polys_primn <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_primn) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys_primn$rarefyID <- coords$rarefyID

luh_polys_primn <- luh_polys_primn %>% gather(year, primn, select = c(2:1167))
luh_polys_primn <- arrange(luh_polys_primn, rarefyID)
save(luh_polys_primn, file = "data/output/LUH_polys_primn.RData")

# Extract secondary forests
forest.array <- ncvar_get(nc_data, "secdf") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "secdf", "_fillvalue")
fillvalue

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_secdf <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_secdf) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys_secdf$rarefyID <- coords$rarefyID

luh_polys_secdf <- luh_polys_secdf %>% gather(year, secdf, select = c(2:1167))
luh_polys_secdf <- arrange(luh_polys_secdf, rarefyID)
save(luh_polys_secdf, file = "data/output/LUH_polys_secdf.RData")

# Extract secondary non-forested land
forest.array <- ncvar_get(nc_data, "secdn") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "secdn", "_fillvalue")
fillvalue

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_secdn <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_secdn) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys_secdn$rarefyID <- coords$rarefyID

luh_polys_secdn <- luh_polys_secdn %>% gather(year, secdn, select = c(2:1167))
luh_polys_secdn <- arrange(luh_polys_secdn, rarefyID)
save(luh_polys_secdn, file = "data/output/LUH_polys_secdn.RData")

# Extract pasture
forest.array <- ncvar_get(nc_data, "pastr") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "pastr", "_fillvalue")
fillvalue

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_pastr <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_pastr) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys_pastr$rarefyID <- coords$rarefyID

luh_polys_pastr <- luh_polys_pastr %>% gather(year, pastr, select = c(2:1167))
luh_polys_pastr <- arrange(luh_polys_pastr, rarefyID)
save(luh_polys_pastr, file = "data/output/LUH_polys_pastr.RData")

# Extract rangeland
forest.array <- ncvar_get(nc_data, "range") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "range", "_fillvalue")
fillvalue

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_range <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_range) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys_range$rarefyID <- coords$rarefyID

luh_polys_range <- luh_polys_range %>% gather(year, range, select = c(2:1167))
luh_polys_range <- arrange(luh_polys_range, rarefyID)
save(luh_polys_range, file = "data/output/LUH_polys_range.RData")

# Extract urban
forest.array <- ncvar_get(nc_data, "urban") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "urban", "_fillvalue")
fillvalue

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_urban <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_urban) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys_urban$rarefyID <- coords$rarefyID

luh_polys_urban <- luh_polys_urban %>% gather(year, urban, select = c(2:1167))
luh_polys_urban <- arrange(luh_polys_urban, rarefyID)
save(luh_polys_urban, file = "data/output/LUH_polys_urban.RData")

# Extract C3 annual crops
forest.array <- ncvar_get(nc_data, "c3ann") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "c3ann", "_fillvalue")
fillvalue

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_c3ann <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_c3ann) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys_c3ann$rarefyID <- coords$rarefyID

luh_polys_c3ann <- luh_polys_c3ann %>% gather(year, c3ann, select = c(2:1167))
luh_polys_c3ann <- arrange(luh_polys_c3ann, rarefyID)
save(luh_polys_c3ann, file = "data/output/LUH_polys_c3ann.RData")

# Extract C3 perennial crops
forest.array <- ncvar_get(nc_data, "c3per") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "c3per", "_fillvalue")
fillvalue

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_c3per <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_c3per) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys_c3per$rarefyID <- coords$rarefyID

luh_polys_c3per <- luh_polys_c3per %>% gather(year, c3per, select = c(2:1167))
luh_polys_c3per <- arrange(luh_polys_c3per, rarefyID)
save(luh_polys_c3per, file = "data/output/LUH_polys_c3per.RData")

# Extract C4 annual crops
forest.array <- ncvar_get(nc_data, "c4ann") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "c4ann", "_fillvalue")
fillvalue

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_c4ann <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_c4ann) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys_c4ann$rarefyID <- coords$rarefyID

luh_polys_c4ann <- luh_polys_c4ann %>% gather(year, c4ann, select = c(2:1167))
luh_polys_c4ann <- arrange(luh_polys_c4ann, rarefyID)
save(luh_polys_c4ann, file = "data/output/LUH_polys_c4ann.RData")

# Extract C4 perennial crops
forest.array <- ncvar_get(nc_data, "c4per") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "c4per", "_fillvalue")
fillvalue

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_c4per <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_c4per) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys_c4per$rarefyID <- coords$rarefyID

luh_polys_c4per <- luh_polys_c4per %>% gather(year, c4per, select = c(2:1167))
luh_polys_c4per <- arrange(luh_polys_c4per, rarefyID)
save(luh_polys_c4per, file = "data/output/LUH_polys_c4per.RData")

# Extract C3 nitrogen fixing crops
forest.array <- ncvar_get(nc_data, "c3nfx") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "c3nfx", "_fillvalue")
fillvalue

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_c3nfx <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_c3nfx) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys_c3nfx$rarefyID <- coords$rarefyID

luh_polys_c3nfx <- luh_polys_c3nfx %>% gather(year, c3nfx, select = c(2:1167))
luh_polys_c3nfx <- arrange(luh_polys_c3nfx, rarefyID)
save(luh_polys_c3nfx, file = "data/output/LUH_polys_c3nfx.RData")