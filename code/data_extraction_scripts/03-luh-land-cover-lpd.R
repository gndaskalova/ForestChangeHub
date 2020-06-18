# Extract land cover through time for the Living Planet Database sites
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

nc_data <- nc_open('states.nc')  # Note - this file is pretty big, not on GitHub
# Can be downloaded from the LUH website

# Save the print(nc) dump to a text file
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

#nc_close(nc_data)  # Don't close it because I will continue extracting!

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

# Import LPI lat and long
global_mus_scaled <- read.csv("data/input/global_mus_scaled.csv")

coords2 <- global_mus_scaled %>% filter(system == "Terrestrial") %>%
  dplyr::select(id, Decimal.Latitude, Decimal.Longitude) %>%
  distinct()

coords2$atop <- coords2$Decimal.Latitude + 0.04413495
coords2$bottom <- coords2$Decimal.Latitude - 0.04413495
coords2$leftb <- coords2$Decimal.Longitude - 0.04413495
coords2$left <- coords2$Decimal.Longitude + 0.04413495

# Create SP

coords2 <- coords2 %>% dplyr::select(left, leftb, atop, bottom, id)
coords3 <- coords2 %>% gather(type, lon, select = c(1:2))
coords3 <- coords3 %>% gather(direction, lat, select = c(1:2))
coords3 <- coords3 %>% arrange(id)

coords4 <- coords3 %>% group_by(id) %>% filter(row_number() == 1)
coords4$order <- "2"
coords3$order <- "1"
coords5 <- full_join(coords3, coords4)
coords5$sort <- paste0(coords5$type, coords5$direction)

coords5 <- coords5 %>% arrange(id, order, sort)

df_to_spp <- coords5 %>%
  group_by(id) %>%
  do(poly = dplyr::select(., lon, lat) %>% Polygon()) %>%
  rowwise() %>%
  do(polys=Polygons(list(.$poly),.$id)) %>%
  {SpatialPolygons(.$polys)}

## plot it
plot(df_to_spp)

luh_polys_primn_lpi <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_primn_lpi) <- c("id", seq(from = 850, to = 2015, by = 1))
luh_polys_primn_lpi$id <- coords2$id

luh_polys_primn_lpi <- luh_polys_primn_lpi %>% gather(year, primn, select = c(2:1167))
luh_polys_primn_lpi <- arrange(luh_polys_primn_lpi, id)
save(luh_polys_primn_lpi, file = "data/output/LUH_polys_primn_lpi.RData")

# Extract secondary forests
forest.array <- ncvar_get(nc_data, "secdf") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "secdf", "_fillvalue")
fillvalue

#nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_secdf_lpi <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_secdf_lpi) <- c("id", seq(from = 850, to = 2015, by = 1))
luh_polys_secdf_lpi$id <- coords2$id

luh_polys_secdf_lpi <- luh_polys_secdf_lpi %>% gather(year, secdf, select = c(2:1167))
luh_polys_secdf_lpi <- arrange(luh_polys_secdf_lpi, id)
save(luh_polys_secdf_lpi, file = "data/output/LUH_polys_secdf_lpi.RData")

# Extract secondary non-forested land
forest.array <- ncvar_get(nc_data, "secdn") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "secdn", "_fillvalue")
fillvalue

#nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_secdn_lpi <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_secdn_lpi) <- c("id", seq(from = 850, to = 2015, by = 1))
luh_polys_secdn_lpi$id <- coords2$id

luh_polys_secdn_lpi <- luh_polys_secdn_lpi %>% gather(year, secdn, select = c(2:1167))
luh_polys_secdn_lpi <- arrange(luh_polys_secdn_lpi, id)
save(luh_polys_secdn_lpi, file = "data/output/LUH_polys_secdn_lpi.RData")

# Extract pasture
forest.array <- ncvar_get(nc_data, "pastr") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "pastr", "_fillvalue")
fillvalue

#nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_pastr_lpi <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_pastr_lpi) <- c("id", seq(from = 850, to = 2015, by = 1))
luh_polys_pastr_lpi$id <- coords2$id

luh_polys_pastr_lpi <- luh_polys_pastr_lpi %>% gather(year, pastr, select = c(2:1167))
luh_polys_pastr_lpi <- arrange(luh_polys_pastr_lpi, id)
save(luh_polys_pastr_lpi, file = "data/output/LUH_polys_pastr_lpi.RData")

# Extract rangeland
forest.array <- ncvar_get(nc_data, "range") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "range", "_fillvalue")
fillvalue

#nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_range_lpi <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_range_lpi) <- c("id", seq(from = 850, to = 2015, by = 1))
luh_polys_range_lpi$id <- coords2$id

luh_polys_range_lpi <- luh_polys_range_lpi %>% gather(year, range, select = c(2:1167))
luh_polys_range_lpi <- arrange(luh_polys_range_lpi, id)
save(luh_polys_range_lpi, file = "data/output/LUH_polys_range_lpi.RData")

# Extract urban
forest.array <- ncvar_get(nc_data, "urban") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "urban", "_fillvalue")
fillvalue

#nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_urban_lpi <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_urban_lpi) <- c("id", seq(from = 850, to = 2015, by = 1))
luh_polys_urban_lpi$id <- coords2$id

luh_polys_urban_lpi <- luh_polys_urban_lpi %>% gather(year, urban, select = c(2:1167))
luh_polys_urban_lpi <- arrange(luh_polys_urban_lpi, id)
save(luh_polys_urban_lpi, file = "data/output/LUH_polys_urban_lpi.RData")

# Extract C3 annual crops
forest.array <- ncvar_get(nc_data, "c3ann") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "c3ann", "_fillvalue")
fillvalue

#nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_c3ann_lpi <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_c3ann_lpi) <- c("id", seq(from = 850, to = 2015, by = 1))
luh_polys_c3ann_lpi$id <- coords2$id

luh_polys_c3ann_lpi <- luh_polys_c3ann_lpi %>% gather(year, c3ann, select = c(2:1167))
luh_polys_c3ann_lpi <- arrange(luh_polys_c3ann_lpi, id)
save(luh_polys_c3ann_lpi, file = "data/output/LUH_polys_c3ann_lpi.RData")

# Extract C3 perennial crops
forest.array <- ncvar_get(nc_data, "c3per") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "c3per", "_fillvalue")
fillvalue

#nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_c3per <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_c3per) <- c("id", seq(from = 850, to = 2015, by = 1))
luh_polys_c3per$id <- coords2$id

luh_polys_c3per <- luh_polys_c3per %>% gather(year, c3per, select = c(2:1167))
luh_polys_c3per_lpi <- arrange(luh_polys_c3per, id)
save(luh_polys_c3per_lpi, file = "data/output/LUH_polys_c3per_lpi.RData")

# Extract C4 annual crops
forest.array <- ncvar_get(nc_data, "c4ann") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "c4ann", "_fillvalue")
fillvalue

#nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_c4ann <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_c4ann) <- c("id", seq(from = 850, to = 2015, by = 1))
luh_polys_c4ann$id <- coords2$id

luh_polys_c4ann <- luh_polys_c4ann %>% gather(year, c4ann, select = c(2:1167))
luh_polys_c4ann_lpi <- arrange(luh_polys_c4ann, id)
save(luh_polys_c4ann_lpi, file = "data/output/LUH_polys_c4ann_lpi.RData")

# Extract C4 perennial crops
forest.array <- ncvar_get(nc_data, "c4per") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "c4per", "_fillvalue")
fillvalue

#nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_c4per <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_c4per) <- c("id", seq(from = 850, to = 2015, by = 1))
luh_polys_c4per$id <- coords2$id

luh_polys_c4per <- luh_polys_c4per %>% gather(year, c4per, select = c(2:1167))
luh_polys_c4per_lpi <- arrange(luh_polys_c4per, id)
save(luh_polys_c4per_lpi, file = "data/output/LUH_polys_c4per_lpi.RData")

# Extract C3 nitrogen fixing crops
forest.array <- ncvar_get(nc_data, "c3nfx") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "c3nfx", "_fillvalue")
fillvalue

#nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

luh_polys_c3nfx <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_c3nfx) <- c("id", seq(from = 850, to = 2015, by = 1))
luh_polys_c3nfx$id <- coords2$id

luh_polys_c3nfx <- luh_polys_c3nfx %>% gather(year, c3nfx, select = c(2:1167))
luh_polys_c3nfx_lpi <- arrange(luh_polys_c3nfx, id)
save(luh_polys_c3nfx_lpi, file = "data/output/LUH_polys_c3nfx_lpi.RData")