rm(list = ls())

options(timeout = 1800) 

# Current URL for the VMS page (March 2026)
vms_page_url <- "https://www.fiskeridir.no/statistikk-tall-og-analyse/data-og-statistikk-om-yrkesfiske/apne-data-posisjonsrapportering-vms"

# Scrape VMS download links from the page
vms_page <- read_html(vms_page_url)
vms_links <- vms_page %>%
  html_nodes("a") %>%
  html_attr("href")

# Keep only links that point to VMS zip files
vms_links <- vms_links[grepl("VMS.*\\.zip|vms.*\\.zip", vms_links, ignore.case = TRUE)]

# Extract year from each link - use the 4-digit year near "VMS" or "vms"
# This avoids matching UUID hex digits
extract_vms_year <- function(link) {
  # Try patterns like "2025-VMS" or "vms-2025" or "VMS_2025"
  m <- str_extract(link, "(20[0-2][0-9])(?=-VMS|-vms|_VMS|-pos)")
  if (!is.na(m)) return(m)
  # Fallback: look for year in the filename portion
  m <- str_extract(basename(link), "20[0-2][0-9]")
  return(m)
}

base_url <- "https://www.fiskeridir.no"

for (link in vms_links) {
  # Build full URL
  if (!startsWith(link, "http")) {
    full_url <- paste0(base_url, link)
  } else {
    full_url <- link
  }
  
  year <- extract_vms_year(link)
  if (is.na(year)) {
    print(paste("Skipping VMS link, could not extract year:", link))
    next
  }
  
  dir_path <- file.path("data", year, "VMS")
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  
  filename <- paste0("VMS_", year, ".zip")
  filepath <- file.path(dir_path, filename)
  
  tryCatch({
    # Use download.file with mode "wb" for binary files - more robust for large files
    result <- download.file(full_url, filepath, mode = "wb", quiet = TRUE)
    
    # Check file size - empty or tiny files are likely errors
    fsize <- file.info(filepath)$size
    if (is.na(fsize) || fsize < 1000) {
      print(paste("VMS", year, "- downloaded file too small (", fsize, "bytes), likely an error page. Removing."))
      file.remove(filepath)
      next
    }
    
    print(paste("Downloaded VMS", year, "-", round(fsize / 1e6, 1), "MB"))
    
    unzip(filepath, exdir = dir_path)
    print(paste("Extracted VMS", year, "to:", dir_path))
    
    file.remove(filepath)
    
  }, error = function(e) {
    print(paste("Error downloading VMS", year, ":", e$message))
    if (file.exists(filepath)) file.remove(filepath)
  })
  
  Sys.sleep(2) # Be polite to the server
}


### DOWNLOAD ERS DATA ----

# ERS uses a consistent URL pattern
ers_base_url <- "https://register.fiskeridir.no/vms-ers/ERS/"

# Years available: 2011 to 2026 (per the current page)
ers_years <- 2011:2026

for (year in ers_years) {
  dir_path <- file.path("data", year, "ERS")
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  
  filename <- paste0("elektronisk-rapportering-ers-", year, ".zip")
  filepath <- file.path(dir_path, filename)
  url <- paste0(ers_base_url, filename)
  
  tryCatch({
    result <- download.file(url, filepath, mode = "wb", quiet = TRUE)
    
    # Check file size
    fsize <- file.info(filepath)$size
    if (is.na(fsize) || fsize < 1000) {
      print(paste("ERS", year, "- downloaded file too small (", fsize, "bytes), likely an error page. Removing."))
      file.remove(filepath)
      next
    }
    
    print(paste("Downloaded ERS", year, "-", round(fsize / 1e6, 1), "MB"))
    
    unzip(filepath, exdir = dir_path)
    print(paste("Extracted ERS", year, "to:", dir_path))
    
    file.remove(filepath)
    
  }, error = function(e) {
    print(paste("Error downloading ERS", year, ":", e$message))
    if (file.exists(filepath)) file.remove(filepath)
  })
  
  Sys.sleep(2) # Be polite to the server
}
