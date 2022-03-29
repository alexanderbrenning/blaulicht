library(tidyverse)
library(sf)
library(leaflet)

town <- "Jena" # name of a town in Thuringia

# Load preprocessed ALKIS data:
(load("alkis-addresses.Rdata"))

# Pick ALKIS data from 'town':
ags <- as.character(alkis_towns[ alkis_towns$Ortsname == town, "AGS" ])
alkis_addresses <- alkis_addresses[ alkis_addresses$AGS == ags , ]
alkis_addresses <- alkis_addresses[ , c("UTMX", "UTMY", "STN")]

# Aggregate coordinates at the street level:
alkis_addresses$STN <- factor(alkis_addresses$STN)
streets <- data.frame(
  STN = levels(alkis_addresses$STN),
  UTMX = tapply(alkis_addresses$UTMX, alkis_addresses$STN, median),
  UTMY = tapply(alkis_addresses$UTMY, alkis_addresses$STN, median)
)

# Assign projection and transform to WGS84:
crs <- "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
wgs84 <- "+proj=longlat +datum=WGS84"
pt <- st_sfc(st_multipoint(x=cbind(streets$UTMX,streets$UTMY), dim = "XY"))
pt <- st_set_crs(pt, crs)
pt <- st_transform(pt, crs = wgs84)
lonlat <- st_coordinates(pt)
streets$lat <- lonlat[,"Y"]
streets$lon <- lonlat[,"X"]

# streets <- readRDS(paste0(town, "_streets.rds"))

# Plot street locations to for quality control:
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lat = streets$lat, lng = streets$lon, 
             label = streets$STN, radius = 5,
             clusterOptions = markerClusterOptions())
m 


# Save in binary format:
saveRDS(streets, file = paste0(town, "_streets.rds"))
