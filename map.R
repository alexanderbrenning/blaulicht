library(tidyverse)
library(leaflet)

town <- "Jena"
# town <- "Weimar"
# town <- "Gera"
# town <- "Erfurt"

d <- readRDS(paste0(town, "_geocoded_reports.rds"))

reference <- "Quelle: Polizeidienststellen via www.presseportal.de"

d$popup <- paste0('<a href="https://www.presseportal.de', d$url, '"><h3>', d$title, '</h3></a><p>',
                  d$datetime, "<br>",
                  d$streets, "<p>",
                  d$text, "<p>", 
                  reference)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lat = d$lat, lng = d$lon, popup = d$popup,
             clusterOptions = markerClusterOptions())
m 
