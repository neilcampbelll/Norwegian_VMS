rm(list=ls())

### DOWNLOAD VMS DATA

# URL of the webpage
url <- "https://www.fiskeridir.no/Tall-og-analyse/AApne-data/posisjonsrapportering-vms"

# Get the content of the page
response <- GET(url)
content <- content(response, "text")

# Extract links using regular expressions, now accounting for both filename formats
links <- str_extract_all(content, "(?<=href=\").*?(?:VMS|posisjonsrapportering-vms).*?\\.zip")[[1]]

# Function to process a file
process_file <- function(url, year) {
  # Create directory if it doesn't exist
  dir_path <- file.path("data", year, "VMS")
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  
  filename <- paste0("Posisjonsrapportering_VMS_", year, ".zip")
  
  # Download file
  tryCatch({
    response <- GET(url, write_disk(file.path(dir_path, filename), overwrite = TRUE))
    if (response$status_code == 200) {
      print(paste("Downloaded:", filename))
      
      # Extract contents
      unzip(file.path(dir_path, filename), exdir = dir_path)
      print(paste("Extracted contents to:", dir_path))
      
      # Delete zip file
      file.remove(file.path(dir_path, filename))
      print(paste("Deleted zip file:", filename))
    } else {
      print(paste("Failed to download:", filename, "Status code:", response$status_code))
    }
  }, error = function(e) {
    print(paste("Error processing file:", filename, "Error:", e$message))
  })
}

# Base URL for the downloads
base_url <- "https://www.fiskeridir.no"

# Process each file
for (link in links) {
  if (!startsWith(link, "http")) {
    full_url <- paste0(base_url, link)
  } else {
    full_url <- link
  }
  year <- str_extract(link, "20[0-9]{2}")
  process_file(full_url, year)
}

### DOWNLOAD CATCH DATA

# Base URL for the downloads
base_url <- "https://register.fiskeridir.no/vms-ers/ERS/"

# Function to process a file
process_file <- function(url, year) {
  # Create directory if it doesn't exist
  dir_path <- file.path("data", year, "ERS")
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  
  filename <- paste0("elektronisk-rapportering-ers-", year, ".zip")
  filepath <- file.path(dir_path, filename)
  
  # Download file
  tryCatch({
    response <- GET(url, write_disk(filepath, overwrite = TRUE))
    if (response$status_code == 200) {
      print(paste("Downloaded:", filename))
      
      # Extract contents
      unzip(filepath, exdir = dir_path)
      print(paste("Extracted contents to:", dir_path))
      
      # Delete zip file
      file.remove(filepath)
      print(paste("Deleted zip file:", filename))
    } else {
      print(paste("Failed to download:", filename, "Status code:", response$status_code))
    }
  }, error = function(e) {
    print(paste("Error processing file:", filename, "Error:", e$message))
  })
}

# Process files for years 2011 to 2024
years <- 2011:2024

for (year in years) {
  url <- paste0(base_url, "elektronisk-rapportering-ers-", year, ".zip")
  process_file(url, as.character(year))
}
