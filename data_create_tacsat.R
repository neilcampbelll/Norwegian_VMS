# years <- 2022

for (years in (2011:2024)){
  vms_file <- list.files(
    path = paste0("data/", years, "/VMS/"),
    pattern = paste0("^(VMS[_-]", years, "|", years, "-VMS)\\.csv$"),
    full.names = TRUE
  )
  
  no.vms <- read.table(vms_file, header = T, sep = ";", dec = ",")
  no.vms <- as.data.frame(no.vms)
  
  # Determine which datetime column exists
  datetime_col <- if("Tidspunkt..UTC." %in% colnames(no.vms)) {
    "Tidspunkt..UTC."
  } else {
    "Tidspunkt.UTC."
  }
  
  if(years == 2022){
    no.vms$Tidspunkt..UTC. <- substr(no.vms$Tidspunkt..UTC., 1, 19)
  }
  # Auto-detect datetime format by trying to parse first row
  sample_datetime <- no.vms[[datetime_col]][1]
  
  # Auto-detect datetime format
  datetime_format <- if(grepl("-[A-Z]{3}-", sample_datetime)) {
    # Format like "01-JAN-24 15.40.00"
    "%d-%b-%y %H.%M.%S"
  } else if(grepl("\\.", sample_datetime) && grepl(" [0-9]{2}\\.[0-9]{2}\\.", sample_datetime)) {
    # Format like "01.01.2022 00.00.00" 
    "%d.%m.%Y %H.%M.%S"
  } else {
    # Format like "07.04.2011 09:30:00" (standard with colons in time)
    "%d.%m.%Y %H:%M:%S"
  }
  
  # Parse the datetime
  no.vms$SI_DATIM <- as.POSIXct(no.vms[[datetime_col]], format = datetime_format, tz = "UTC")
  
  
  tacsat2 <- no.vms %>%
    select(
      VE_REF = Radiokallesignal,
      SI_LATI = Breddegrad,
      SI_LONG = Lengdegrad,
      datetime = all_of(datetime_col),
      SI_SP = Fart,
      SI_HE = Kurs
    ) 
  
  tacsat2<- tacsat2 %>%
    mutate(
      datetime = as.POSIXct(datetime, format = datetime_format),
      SI_DATE = format(datetime, "%d-%m-%Y"),
      SI_TIME = format(datetime, "%H:%M")
    ) %>%
    select(VE_REF, SI_LATI, SI_LONG, SI_DATE, SI_TIME, SI_SP, SI_HE)
  
  print(years)
  save(tacsat2, file = paste0("Results/Tacsat", years, ".RData"))
}
