### Function that scrapes relevant police reports:
# x: a single URL
# delay: Delay in seconds after each request
scrape_police_reports_url <- function(x, town = "Jena", 
                                  out_path = file.path("reports", town),
                                  skip_existing = TRUE,
                                  delay = 10, verbose = 0) {
  URL <- x
  
  if (!is.null(out_path)) {
    cnt <- URL %>% str_extract("[0-9]*$")
    if (cnt == "") cnt <- "0"
    out_file <- paste0(out_path, "/", strftime(Sys.Date(), format = "%Y-%m-%d"),
                       "-", cnt, ".rds")
    if (skip_existing)
      if (file.exists(out_file)) {
        if (verbose >= 1)
          cat("Read file ", out_file, "...\n", sep  = "")
        d <- readRDS(out_file)
        if (verbose >= 1)
          cat("Found ", nrow(d), " police reports from ", town, ".\n", sep = "")
        return(d)
      }
  }
  
  if (verbose >= 1)
    cat("Read URL ", URL, "...\n", sep  = "")
  
  # Read the Web page:
  html <- URL %>% url() %>% read_html()
  
  # Be gentle: wait a few seconds between requests
  if (delay > 0) Sys.sleep(delay)
  
  # The relevant information is in the links and their attributes
  # --> extract this information:
  x <- html %>% html_nodes("a")
  sel <- x %>% html_attr("class") %>% is.na()
  x <- x[sel]
  sel <- x %>% html_attr("href") %>% str_detect("/pm/")
  x <- x[sel]
  sel <- !(x %>% html_node("i") %>% is.na()) | 
    !(x %>% html_attr("title") %>% duplicated(fromLast = TRUE))
  x <- x[sel]
  
  dt <- html %>% html_elements("h5.date") %>% html_text()
  
  sel <- x %>% html_attr("title") 
  sel <- str_detect(sel, "^LPI-") | str_detect(sel, "^BPOLI") |
    str_detect(sel, "^API-") | str_detect(sel, "^LPD-") | 
    str_detect(sel, "^HZA-")
  x <- x[sel]
  dt <- dt[sel]
  
  url <- x %>% html_attr("href")
  title <- x %>% html_attr("title") %>% 
    str_remove("LPI-[A-Z]*: ") %>%
    str_remove("LPD-[A-Z]*: ") %>%
    str_remove("API-[A-Z]*: ") %>%
    str_remove("HZA-[A-Z]*: ") %>%
    str_remove("BPOLI [A-Z]*: ")
  townname <- x %>% html_text() %>% 
    str_extract("^.* \\(ots\\)") %>%
    str_remove(" \\(ots\\)")
  date <- dt %>% str_sub(end = 10)
  time <- dt %>% str_sub(start = 14, end = 18)
  text <- html_text(x)
  
  # Data frame with results:
  d <- data.frame(
    url = url,
    title = title,
    town = townname,
    date = date,
    time = time,
    datetime = dt,
    text = text
  )
  
  # Only interested in police reports from 'town':
  sel <- !is.na(d$town) & str_starts(d$town, town)
  d <- d[sel,]
  
  if (verbose >= 1) {
    cat("Found ", nrow(d), " police reports from ", town, ".\n", sep = "")
  }
  
  if (!is.null(out_path))
    saveRDS(d, file = out_file)
  
  d
}


scrape_police_reports <- function(town, npages = 1, ...) {
  # Base URL and URLs of subsequent pages:
  urls <- paste0("https://www.presseportal.de/blaulicht/r/", town)
  if (npages > 1) {
    suffix <- c("", paste0("/", 30*1:(npages-1)))
    urls <- paste0("https://www.presseportal.de/blaulicht/r/", town, suffix)
  }
  # Do the scraping:
  x <- urls %>% map(function(x) try(scrape_police_reports_url(x, town = town, ...)))
  unsel <- x %>% map_lgl(inherits, "try-error")
  x <- x[!unsel] %>% bind_rows()
  x
}

load_reports <- function(town, path = "reports") {
  d <- dir(file.path(path, "/", town), "\\.rds$", full.names = TRUE) %>% 
    map(readRDS) %>% bind_rows()
  d <- d[!duplicated(d),]
  d
}


geoparse <- function(x, streetnames, fuzzy = FALSE, maxdist = 0.03) {
  
  internal_geoparse <- function(x, streetnames, strict = TRUE, fuzzy, maxdist = 0.03) {
    if (fuzzy) {
      sel <- sapply(streetnames, function(y) length(agrep(y, x, max.distance = maxdist)) > 0)
    } else {
      patterns <- streetnames
      if (strict)
        patterns <- paste0("[ ,\\./-]", streetnames, "[ ,\\./-]")
      sel <- sapply(patterns, function(y) any(grepl(y, x, ignore.case = TRUE)))
    }
    sel
  }
  
  sel <- internal_geoparse(x = x, streetnames = streetnames, strict = TRUE, fuzzy = fuzzy, maxdist = maxdist)
  
  if (length(sel) >= 1) {
    if (any(streetnames[sel] == "Markt")) {
      if (length(sel) > 1) {
        sel[streetnames[sel] == "Markt"] <- FALSE
      } else if (!fuzzy) {
        # Try harder to find a different place name:
        sel <- internal_geoparse(x = x, streetnames = streetnames, strict = FALSE, fuzzy = FALSE)
        if (any(streetnames[sel] == "Markt") & (length(sel) > 1))
          sel[streetnames[sel] == "Markt"] <- FALSE
      }
    }
  }
  streetnames[sel]
}

geocode <- function(x, streets, max_spread = 500) {
  res <- data.frame(
    lon = NA,
    lat = NA,
    streets = ""
  )
  if (length(x) > 0) {
    sel <- which(streets$STN %in% x)
    if (length(sel) > 1) {
      if (max(dist(streets[sel, c("UTMX","UTMY")])) > max_spread)
        sel <- sample(sel, size = 1)
    }
    if (length(sel) > 0) {
      res <- data.frame(
        lon = median(streets$lon[sel]),
        lat = median(streets$lat[sel]),
        streets = paste(x, collapse = ", ")
      )
    }
  }
  res
}

geocode_text <- function(x, streets) {
  geocode(geoparse(x, streetnames = streets$STN), streets = streets)
}

geocode_reports <- function(x, streets) {
  xy <- x$text %>% map(geocode_text, streets = streets) %>% bind_rows()
  x$lon <- xy$lon
  x$lat <- xy$lat
  x$streets <- xy$streets
  x
}
