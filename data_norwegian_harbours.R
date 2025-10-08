library(readr)
library(dplyr)
library(sf)
library(vmstools)

data(ICESareas)

# Download from DataHub
url <- "https://datahub.io/core/un-locode/r/code-list.csv"
cat("Downloading UN/LOCODE data...\n")
all_locodes <- read_csv(url, show_col_types = FALSE)

# Filter for Norway only
atlantic_ports <- all_locodes %>%
  filter(Country %in% c("NO", "LV", "EE", "LT", "FI", "SE", "BE", "IE", "FR", "DK",
                        "NL", "ES", "DE", "IS", "GB", "NO", "PT", "FO", "RU", "GL", "IS")) %>%
 filter(grepl("1", Function)) %>% 
 filter(!grepl("7", Function))
# this filter picks out (1) ports, and tries to remove (7) fixed oil installations - not entirely successful! :-(


# Parse coordinates and create spatial object

# Function to convert coordinates to decimal degrees
parse_coordinates <- function(coord_str) {
  if (is.na(coord_str) || coord_str == "") {
    return(list(lat = NA, lon = NA))
  }
  
  # Split by space
  parts <- strsplit(coord_str, " ")[[1]]
  if (length(parts) != 2) {
    return(list(lat = NA, lon = NA))
  }
  
  # Parse latitude (e.g., "6753N")
  lat_str <- parts[1]
  lat_deg <- as.numeric(substr(lat_str, 1, 2))
  lat_min <- as.numeric(substr(lat_str, 3, 4))
  lat_dir <- substr(lat_str, 5, 5)
  lat_decimal <- lat_deg + (lat_min / 60)
  if (lat_dir == "S") lat_decimal <- -lat_decimal
  
  # Parse longitude (e.g., "01259E")
  lon_str <- parts[2]
  lon_deg <- as.numeric(substr(lon_str, 1, 3))
  lon_min <- as.numeric(substr(lon_str, 4, 5))
  lon_dir <- substr(lon_str, 6, 6)
  lon_decimal <- lon_deg + (lon_min / 60)
  if (lon_dir == "W") lon_decimal <- -lon_decimal
  
  return(list(lat = lat_decimal, lon = lon_decimal))
}

# Apply coordinate parsing
cat("\n\nParsing coordinates...\n")
coords_parsed <- lapply(atlantic_ports$Coordinates, parse_coordinates)

atlantic_ports <- atlantic_ports %>%
  select(Country, Location, Name, Coordinates) %>%
  mutate(
    latitude = sapply(coords_parsed, function(x) x$lat),
    longitude = sapply(coords_parsed, function(x) x$lon)
  )

# Create spatial object (remove rows without coordinates)
atlantic_ports_sf <- atlantic_ports %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)



ia <- st_as_sf(ICESareas, coords = c("SI_LONG", "SI_LATI"), crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_zm()

# Find intersections
atlantic_ports_sf <- st_filter(atlantic_ports_sf, ia, .predicate = st_intersects)

atlantic_ports_sf <- 
  atlantic_ports_sf |> 
  sf::st_transform(crs = 3857) |> 
  # the range in harbour is always 3 km
  sf::st_buffer(dist = 3000) |> 
  sf::st_transform(crs = 4326) |> 
  dplyr::select(Country, Location, Name)

write_sf(atlantic_ports_sf, "Altantic_ports.shp")
