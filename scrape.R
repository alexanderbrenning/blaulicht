library(tidyverse) # for processing the data
library(rvest)     # for Web-scraping
source("policereports_functions.R")

### Settings:
# town <- "Jena"
town <- "Weimar"
# town <- "Gera"
# town <- "Erfurt"

# Number of pages to be scraped (each containing 30 police reports):
npages <- 100

# debug(scrape_police_reports)
# d <- scrape_police_reports(urls[6], town = town, delay = 0, verbose = 1)

# Load all previously saved reports:
d0 <- load_reports(town = town)

# Scrape all reports - this will take some time!!!
d <- scrape_police_reports(town, npages = npages, verbose = 1)

# Merge both datasets:
d <- rbind(d0, d)
d <- d[!duplicated(d),]

saveRDS(d, file = paste0(town, "_reports.rds"))
