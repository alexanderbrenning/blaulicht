library(tidyverse)
source("policereports_functions.R")

town <- "Jena"
# town <- "Weimar"
# town <- "Gera"
# town <- "Erfurt"

d <- readRDS(paste0(town, "_reports.rds"))

streets <- readRDS(paste0(town, "_streets.rds"))

unsel <- str_starts(d$title, "Pressemitteilung")
d <- d[!unsel,]

# geoparse(d$text[5])
# geocode_text(d$text[9])

d <- geocode_reports(d, streets = streets)

print(summary(is.na(d$lon)))
d <- d[!is.na(d$lon),]

saveRDS(d, file = paste0(town, "_geocoded_reports.rds"))
