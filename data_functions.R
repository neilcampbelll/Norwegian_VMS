# Function to find the corresponding L5_MET for a given species code
find_l5_met <- function(species_code) {
  assemblage <- target_species$Target_Assemblage[target_species$Species_Code == species_code]
  if (length(assemblage) == 0) return(NA)
  return(assemblage[1])
}

parse_datetime <- function(x) {
  # Try parsing as "DD.MM.YYYY HH:MM:SS"
  parsed <- dmy_hms(x)
  
  # If that fails, try parsing as "DD.MM.YYYY"
  if (all(is.na(parsed))) {
    parsed <- dmy(x)
  }
  
  # For entries parsed as just date, add midnight time
  parsed[!is.na(parsed) & hour(parsed) == 0 & minute(parsed) == 0 & second(parsed) == 0] <- 
    parsed[!is.na(parsed) & hour(parsed) == 0 & minute(parsed) == 0 & second(parsed) == 0] + seconds(1)
  
  parsed
}


# Function to generate all dates between two dates
date_sequence <- function(start_date, end_date) {
  seq(start_date, end_date, by = "day")
}

# function to match species codes and 
find_l5_met <- function(species_code) {
  assemblage <- target_species$Target_Assemblage[target_species$Species_Code == species_code]
  if (length(assemblage) == 0) return(NA)
  return(assemblage[1])
}


# Function to extract invalid gear codes from a year's data
extract_invalid_gears <- function(eflalo, year) {
  m4_ices <- getCodeList("GearType")
  
  invalid_gears <- eflalo %>%
    filter(!LE_GEAR %in% m4_ices$Key) %>%
    group_by(LE_GEAR) %>%
    summarise(
      count = n(),
      year = year
    ) %>%
    arrange(desc(count))
  
  return(invalid_gears)
}

# function to create a level 6 metier based on eflalo parameters
construct_metier_l6 <- function(le_gear, le_met, le_msz, lookup_table) {
  if (is.na(le_gear) || is.na(le_met)) return(NA_character_)
  
  mesh_range <- find_best_mesh_range(le_gear, le_met, as.numeric(le_msz), lookup_table)
  
  if (is.na(mesh_range)) return(NA_character_)
  
  paste(le_gear, le_met, mesh_range, "0", "0", sep = "_")
}

#function to work out which mesh size range should apply
find_best_mesh_range <- function(gear, target, mesh_size, lookup_table) {
  if (is.na(gear) || is.na(target)) return(NA_character_)
  
  available <- lookup_table %>%
    filter(L4_gear == gear & L5_target == target) %>%
    pull(available_ranges)
  
  if (length(available) == 0) return(NA_character_)
  
  ranges <- available[[1]]
  
  if (is.na(mesh_size) || mesh_size <= 0) {
    if (">0" %in% ranges) return(">0")
    if ("0" %in% ranges) return("0")
    return(ranges[1])
  }
  
  for (range in ranges) {
    if (grepl("^>=", range)) {
      threshold <- as.numeric(gsub(">=", "", range))
      if (!is.na(threshold) && mesh_size >= threshold) return(range)
    }
    else if (grepl("^<", range)) {
      threshold <- as.numeric(gsub("<", "", range))
      if (!is.na(threshold) && mesh_size < threshold) return(range)
    }
    else if (grepl("-", range)) {
      parts <- strsplit(range, "-")[[1]]
      if (length(parts) == 2) {
        lower <- as.numeric(parts[1])
        upper <- as.numeric(parts[2])
        if (!is.na(lower) && !is.na(upper) && 
            mesh_size >= lower && mesh_size <= upper) {
          return(range)
        }
      }
    }
  }
  
  if (">0" %in% ranges) return(">0")
  return(ranges[1])
}


# Helper function for robust reading and date parsing
read_ers_file <- function(path) {
  read_delim(
    path, 
    delim = ";", 
    escape_double = TRUE, 
    trim_ws = TRUE, 
    guess_max = 100000, 
    locale = locale(decimal_mark = ",", grouping_mark = ".")
  )
}

parse_no_datetime <- function(date_col, time_col = NULL) {
  # Combines date and time if both provided, handles dots, dashes, and Norwegian months
  dt_string <- if(is.null(time_col)) date_col else paste(date_col, time_col)
  parse_date_time(dt_string, orders = c("d.m.Y H:M:S", "d.m.Y H.M.S", "d-b-y H.M.S", "d/m/Y H:M:S", "d.m.Y H:M"))
}


