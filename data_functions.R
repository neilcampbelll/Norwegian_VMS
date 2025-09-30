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
