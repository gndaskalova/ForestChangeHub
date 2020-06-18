# Extract IUCN threats for BioTIME and LPD species
# Gergana Daskalova
# 2nd Oct 2018

# Adapted from code to extract habitat types by John Godlee

# Packages ----
library(rredlist)
library(dplyr)

# IUCN Redlist API key ----
IUCN_REDLIST_KEY <- "5095ce9b03e5bae012cde5cc112dd0baf09092fa4a723d042142a8c21ea3ad00"

# Load data ----

# Get species list
species_names <- read.csv("data/input/terr_species.csv")

# Get threat data for each species ----
# Doing it in batches because otherwise the connection times out

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[1:100,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df <- lapply(threats_name, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[100:1000,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name2 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df2 <- lapply(threats_name2, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[1000:2000,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Add column to each `result` data frame of species name
threats_name3 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df3 <- lapply(threats_name3, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[2001:2500,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name4 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df4 <- lapply(threats_name4, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[2501:2800,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name5 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df5 <- lapply(threats_name5, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[2801:2900,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name6 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df6 <- lapply(threats_name6, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[2901:2910,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name7 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df7 <- lapply(threats_name7, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[2920:3000,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name8 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df8 <- lapply(threats_name8, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[3001:3200,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name9 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df9 <- lapply(threats_name9, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[3380:4000,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name11 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df11 <- lapply(threats_name11, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[4001:4500,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name12 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df12 <- lapply(threats_name12, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[5001:5500,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name13 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df13 <- lapply(threats_name13, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[5501:6000,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name14 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df14 <- lapply(threats_name14, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[6001:6500,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name15 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df15 <- lapply(threats_name15, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[6501:7000,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name16 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df16 <- lapply(threats_name16, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[7001:8000,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name17 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df17<- lapply(threats_name17, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[8001:8200,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name18 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df18 <- lapply(threats_name18, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[8201:8500,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name19 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df19 <- lapply(threats_name19, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[8501:8800,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name20 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df20 <- lapply(threats_name20, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[8801:9000,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name21 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df21 <- lapply(threats_name21, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[9301:9500,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name22 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df22 <- lapply(threats_name22, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[9801:10000,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name23 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df23 <- lapply(threats_name23, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[10001:11000,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name24 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df24 <- lapply(threats_name24, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(species_names[11001:11568,]$species))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name25 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df25 <- lapply(threats_name25, "[[", 2) %>%
  bind_rows()

# Bind all the objects together
threats <- rbind(threat_df, threat_df2, threat_df3, 
                 threat_df4, threat_df5, threat_df6, threat_df7,
                 threat_df8, threat_df9, threat_df10, threat_df11,
                 threat_df12, threat_df13, threat_df14, threat_df15,
                 threat_df16, threat_df17, threat_df18, threat_df19,
                 threat_df20, threat_df21, threat_df22, threat_df23,
                 threat_df24, threat_df25)

# Save the big file
save(threats, file = "data/output/threats.RData")

threats_sum <- threats %>% group_by(title) %>%
  tally() %>% arrange(desc(n))

# Checking which species don't have threat data in the IUCN
check <- left_join(species_names, threats, by = "species")

# For the species from the new BioTIME studies
load("new_biotime.RData")
new_names <- new_biotime %>% select(GENUS_SPECIES) %>% distinct()

# Check which ones weren't part of previous BioTIME studies
old_names <- species_names %>% select(species)
colnames(new_names) <- "species"
names_diff <- as.data.frame(anti_join(new_names, old_names, by = "species"))

# Extract threats for those new species
# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(names_diff[1:100,]))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name26 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df26 <- lapply(threats_name26, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(names_diff[101:300,]))


# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name27 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df27 <- lapply(threats_name27, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(names_diff[301:700,]))


# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name28 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df28 <- lapply(threats_name28, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(names_diff[701:840,]))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_name29 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_df29 <- lapply(threats_name29, "[[", 2) %>%
  bind_rows()

# Bind all the objects together
threats_new <- rbind(threat_df26, threat_df27, 
                 threat_df28, threat_df29)

threats_biotime <- rbind(threats, threats_new)

# Save the file of threats for BioTIME species
save(threats_biotime, file = "data/output/threats_biotime.RData")


# LPD species ----
mus <- read.csv("global_mus_scaled.csv")
mus <- mus %>% mutate(species = paste(Genus, Species, sep = ' '))
mus_names <- mus %>% select(species) %>% distinct()
mus_names <- as.data.frame(mus_names)

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(mus_names[1:200,]))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_lpi <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_lpi <- lapply(threats_lpi, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(mus_names[201:500,]))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_lpi2 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_lpi2 <- lapply(threats_lpi2, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(mus_names[501:900,]))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_lpi3 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_lpi3 <- lapply(threats_lpi3, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(mus_names[901:1300,]))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_lpi4 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_lpi4 <- lapply(threats_lpi4, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(mus_names[1301:1600,]))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_lpi5 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_lpi5 <- lapply(threats_lpi5, "[[", 2) %>%
  bind_rows()

# Convert iucn_sp_names to list, 1 name per list entry
iucn_sp_list <- as.list(as.character(mus_names[1601:2074,]))

# Download species habitats to list
threats_sp <- lapply(iucn_sp_list, function(i) {rl_threats(name = i, key = IUCN_REDLIST_KEY)})

# Transform list to tidy data frame with list of habitats per species

# Add column to each `result` data frame of species name
threats_lpi6 <- lapply(threats_sp, function(i) {
  i$result$species = rep(i$name, times = length(i$result$code)); 
  return(i)
})

# Collapse list to data frame
threat_lpi6 <- lapply(threats_lpi6, "[[", 2) %>%
  bind_rows()

# Merge
threats_lpi <- rbind(threat_lpi, threat_lpi2, threat_lpi3,
                     threat_lpi4, threat_lpi5, threat_lpi6)

save(lpi_threats, file = "data/output/threats_lpi.RData")

lpi_threats_sum <- lpi_threats %>% group_by(title) %>% 
  tally() %>% arrange(desc(n))
