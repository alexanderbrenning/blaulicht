library(tidyverse)
library(rvest)
html <- read_html(url("https://www.presseportal.de/blaulicht/r/Jena"))

# Extract articles:
x <- html %>% html_nodes("ul")
sel <- str_starts(html_attr(x, "class"), "article-list row")
sel[is.na(sel)] <- FALSE
x <- x[sel] %>% html_nodes("article")

# URLs:
urls <- x %>% map_chr(function(y) html_attr(html_nodes(y, "a")[2], "href"))
sel_urls <- str_detect(urls, "/blaulicht/pm/")
urls <- urls[sel_urls]

x <- x[sel_urls]

# Titles:
titles <- x %>% map_chr(function(y) html_text(html_nodes(y, "a")[2]))

# Reports:
reports <- x %>% map_chr(function(y) html_text(html_nodes(y, "p")[2]))

towns <- reports %>% 
  str_extract("^.* \\(ots\\)") %>%
  str_remove(" \\(ots\\)")

dates <- x %>% html_node("div .date") %>% html_text()

# Data frame with results:
d <- data.frame(
  url = urls,
  title = titles,
  town = towns,
  text = reports,
  date = dates
)

# Only interested in police reports from Jena:
sel <- !is.na(d$town) & str_starts(d$town, "Jena")
d <- d[sel,]


streets <- readRDS("Jena_streets.rds")

geoparse <- function(x, streets) {
  sel <- sapply(streets$STN, function(y) any(grepl(y, x, ignore.case = TRUE)))
  sel <- streets$STN[sel]
  if ((length(sel) > 1) & any(sel == "Markt"))
    sel <- sel[ sel != "Markt" ] # could just be a supermarket...
  sel
}

# try it:
geoparse(d$text[1], streets = streets)

geocode <- function(x, streets) {
  sel <- streets$STN %in% x
  data.frame(
    lat = median(streets$lat[sel]),
    lon = median(streets$lon[sel]),
    streets = paste(x, collapse = ", ")
  )
}

# try it:
geocode("Stadtrodaer Straße", streets = streets)
geocode(c("Löbdergraben", "Sonnenhof"), streets = streets)

# apply this workflow to all police reports:
xy <- d$text %>% map(geoparse, streets = streets) %>% 
  map(geocode, streets = streets) %>% bind_rows()
d <- cbind(d, xy)
d <- d[!is.na(d$lat), ]


library(leaflet)
d$popup <- paste0('<a href="', d$url, '">', 
                  '<h3>', d$title, '</h3></a><p>',
                  d$date, "<br>",
                  d$streets, "<p>", d$text, "<p>", 
                  "Quelle: Polizei via presseportal.de ")

# leaflet() %>% addTiles() %>%
#   addMarkers(lat = d$lat, lng = d$lon, popup = d$popup,
#              clusterOptions = markerClusterOptions())

leaflet() %>% addTiles() %>%
  addCircleMarkers(lat = d$lat, lng = d$lon, popup = d$popup,
             radius = 5,
             clusterOptions = markerClusterOptions())
