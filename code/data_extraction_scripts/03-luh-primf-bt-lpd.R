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

# Extract primary forests
forest.array <- ncvar_get(nc_data, "primf") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "primf", "_fillvalue")
fillvalue

nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

# Import bioTIME lat and long
load("data/input/rarefied_mediansOct2017.Rdata")

coords <- rarefied_medians %>% filter(REALM == "Terrestrial") %>%
  dplyr::select(rarefyID, rarefyID_x, rarefyID_y) %>%
  distinct()

coords2 <- coords

# Makes for cells of approximately 96km2, with variation due to the Earth's curvature
coords2$atop <- coords2$rarefyID_y + 0.04413495
coords2$bottom <- coords2$rarefyID_y - 0.04413495
coords2$leftb <- coords2$rarefyID_x - 0.04413495
coords2$left <- coords2$rarefyID_x + 0.04413495

# Create SP

coords2 <- coords2 %>% select(left, leftb, atop, bottom, rarefyID)
coords3 <- coords2 %>% gather(type, lon, select = c(1:2))
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
  do(poly = select(., lon, lat) %>%Polygon()) %>%
  rowwise() %>%
  do(polys=Polygons(list(.$poly),.$rarefyID)) %>%
  {SpatialPolygons(.$polys)}

## plot it
plot(df_to_spp)

luh_polys <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
#save(luh_polys, file = "data/output/LUH_polys.RData")
colnames(luh_polys) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys$rarefyID <- coords$rarefyID

luh_polys_long <- luh_polys %>% gather(year, primf, select = c(2:1167))
luh_polys_long <- arrange(luh_polys_long, rarefyID)
#save(luh_polys_long, file = "data/output/LUH_polys_long.RData")

luh_polys_long <- luh_polys_long %>% group_by(rarefyID) %>%
  mutate(primf_diff = c(NA, diff(primf)))

luh_polys_max <- luh_polys_long %>% group_by(rarefyID) %>%
  mutate(max.loss = min(primf_diff)) %>%
  select(rarefyID, year, max.loss) %>% distinct()

# save(luh_polys_max, file = "data/output/LUH_polys_max.RData")

# Sept 2018 - new studies added ----
nc_data <- nc_open('states.nc')  # Note - this file is pretty big, not on GitHub
# File is in Gergana's Download folder
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

# Extract primary forests
forest.array <- ncvar_get(nc_data, "primf") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "primf", "_fillvalue")
fillvalue

nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

# Import bioTIME lat and long
# Set working directory back to LandUseHub
coords <- load("data/output/cell_coords_newsept.Rdata")

# The new studies are 516, 521, 522, 523, 524
colnames(rarefyID_cell_centre)
new_coords <- filter(rarefyID_cell_centre, STUDY_ID %in% c("516", "521", "522", "523", "524"))

coords <- new_coords %>%
  dplyr::select(rarefyID, rarefyID_x, rarefyID_y) %>%
  distinct()

coords2 <- coords

coords2$atop <- coords2$rarefyID_y + 0.04413495
coords2$bottom <- coords2$rarefyID_y - 0.04413495
coords2$leftb <- coords2$rarefyID_x - 0.04413495
coords2$left <- coords2$rarefyID_x + 0.04413495

# Create SP

coords2 <- coords2 %>% dplyr::select(left, leftb, atop, bottom, rarefyID)
coords3 <- coords2 %>% gather(type, lon, select = c(1:2))
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
  do(poly = dplyr::select(., lon, lat) %>%Polygon()) %>%
  rowwise() %>%
  do(polys=Polygons(list(.$poly),.$rarefyID)) %>%
  {SpatialPolygons(.$polys)}

## plot it
plot(df_to_spp)

luh_polys <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
#save(luh_polys, file = "data/output/LUH_polys.RData")
colnames(luh_polys) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys$rarefyID <- coords$rarefyID

luh_polys_long <- luh_polys %>% gather(year, primf, select = c(2:1167))
luh_polys_longSept2018 <- arrange(luh_polys_long, rarefyID)
#save(luh_polys_longSept2018, file = "data/output/LUH_polys_long_sept2018.RData")


# Oct 2018 -for pop change locations ----
nc_data <- nc_open('states.nc')  # Note - this file is pretty big, not on GitHub
# File is in Gergana's Download folder
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

# Extract primary forests
forest.array <- ncvar_get(nc_data, "primf") # store the data in a 3-dimensional array
dim(forest.array) 

fillvalue <- ncatt_get(nc_data, "primf", "_fillvalue")
fillvalue

nc_close(nc_data)

forest.array[forest.array == fillvalue$value] <- NA

r_brick <- brick(forest.array, xmn=min(lat), 
                 xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- t(r_brick)

# Load BioTIME data in
BioTIMEQSept18 <- read.csv("D:/Gergana_Daskalova/BioTIMELatest/BioTIMELatest/BioTIMEQSept18.csv")
bioTIMEmetadataSept18 <- read.csv("D:/Gergana_Daskalova/BioTIMELatest/BioTIMELatest/bioTIMEmetadataSept18.csv")

meta_realm <- bioTIMEmetadataSept18 %>% dplyr::select(STUDY_ID, REALM, TAXA)
BioTIMEQSept18 <- inner_join(BioTIMEQSept18, meta_realm, by = "STUDY_ID")
meta_duration <- bioTIMEmetadataSept18 %>% mutate(duration = END_YEAR - START_YEAR) %>%
  dplyr::select(STUDY_ID, duration)
BioTIMEQSept18 <- inner_join(BioTIMEQSept18, meta_duration, by = "STUDY_ID")
BioTIMEQSept18 <- distinct(BioTIMEQSept18)

# Keep only terrestrial data and populations with 5 or more records
terr_pop <- BioTIMEQSept18 %>% filter(REALM == "Terrestrial" & duration > 4 &
                                        TAXA != "Terrestrial plants")

coords2 <-terr_pop %>% dplyr::select(STUDY_ID,LATITUDE, LONGITUDE) %>% distinct()

coords2$atop <- coords2$CENT_LAT + 0.04413495
coords2$bottom <- coords2$CENT_LAT - 0.04413495
coords2$leftb <- coords2$CENT_LONG - 0.04413495
coords2$left <- coords2$CENT_LONG + 0.04413495

# Create SP
coords2 <- coords2 %>% dplyr::select(left, leftb, atop, bottom, STUDY_ID)
coords3 <- coords2 %>% gather(type, lon, select = c(1:2))
coords3 <- coords3 %>% gather(direction, lat, select = c(1:2))
coords3 <- coords3 %>% arrange(STUDY_ID)

coords4 <- coords3 %>% group_by(STUDY_ID) %>% filter(row_number() == 1)
coords4$order <- "2"
coords3$order <- "1"
coords5 <- full_join(coords3, coords4)
coords5$sort <- paste0(coords5$type, coords5$direction)

coords5 <- coords5 %>% arrange(STUDY_ID, order, sort)

df_to_spp <- coords5 %>%
  group_by(STUDY_ID) %>%
  do(poly = dplyr::select(., lon, lat) %>%Polygon()) %>%
  rowwise() %>%
  do(polys=Polygons(list(.$poly),.$STUDY_ID)) %>%
  {SpatialPolygons(.$polys)}

## plot it
plot(df_to_spp)

luh_polys <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
#save(luh_polys, file = "data/output/LUH_polys.RData")
colnames(luh_polys) <- c("rarefyID", seq(from = 850, to = 2015, by = 1))
luh_polys$STUDY_ID <- unique(coords5$STUDY_ID)

luh_polys_long <- luh_polys %>% gather(year, primf, select = c(2:1167))
luh_polys_long_bt_pop <- arrange(luh_polys_long, STUDY_ID)
save(luh_polys_long_bt_pop, file = "data/output/LUH_polys_long_bt_pop.RData")

#  LPI ----
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

luh_polys_lpi <- raster::extract(r_brick, df_to_spp, df = TRUE, fun = mean)
colnames(luh_polys_lpi) <- c("id", seq(from = 850, to = 2015, by = 1))
luh_polys_lpi$id <- coords2$id

luh_polys_lpi_long <- luh_polys_lpi %>% gather(year, primf, select = c(2:1167))
luh_polys_lpi_long <- arrange(luh_polys_lpi_long, id)

luh_polys_lpi_long <- luh_polys_lpi_long %>% group_by(id) %>%
  mutate(primf_diff = c(NA, diff(primf)))
save(luh_polys_lpi_long, file = "data/output/LUH_polys_long_lpi.RData")

luh_polys_lpi_max <- luh_polys_lpi_long %>% group_by(id) %>%
  mutate(max.loss = min(primf_diff, na.rm = T)) 
luh_polys_lpi_max$test <- luh_polys_lpi_max$primf_diff == luh_polys_lpi_max$max.loss
luh_polys_lpi_max <- filter(luh_polys_lpi_max, test == TRUE)

luh_polys_lpi_max <- luh_polys_lpi_max  %>%
  dplyr::select(id, year, max.loss) %>% distinct() %>% drop_na(max.loss)
luh_polys_lpi_max <- luh_polys_lpi_max %>% filter(max.loss != 0)
save(luh_polys_lpi_max, file = "data/output/LUH_polys_long_lpi_max.RData")
