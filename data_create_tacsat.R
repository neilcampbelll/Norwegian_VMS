for (years in (2011:2025)){

  ## note - the naming protocol changes in 2022, so this is a bit of a fix - not ideal
  vms_file <- list.files(
    path = paste0("data/", years, "/VMS/"),
    pattern = paste0("^(VMS[_-]", years, "|", years, "-VMS)\\.csv$"),
    full.names = TRUE
  )
  
  # Read the file
  no.vms <- read.table(vms_file, header = T, sep = ";", dec = ",")

no.vms <- as.data.frame(no.vms)

tacsat2 <- no.vms %>%
  # Select and rename columns
  select(
    VE_REF = Radiokallesignal,
    SI_LATI = Breddegrad,
    SI_LONG = Lengdegrad,
    datetime = matches("Tidspunkt.*UTC"),
    SI_SP = Fart,
    SI_HE = Kurs
  ) %>%
  # Convert datetime and split into date and time
  mutate(
    datetime = {
      dt <- as.POSIXct(datetime, format = "%d.%m.%Y  %H:%M:%S")
      if (all(is.na(dt))) dt <- as.POSIXct(datetime, format = "%d-%b-%y %H.%M.%OS")
      dt
    },
    SI_DATE = format(datetime, "%d/%m/%Y"),
    SI_TIME = format(datetime, "%H:%M")
  )%>%
  # Select final columns in the correct order
  select(VE_REF, SI_LATI, SI_LONG, SI_DATE, SI_TIME, SI_SP, SI_HE)

print(years)

save(tacsat2, file = paste0("results/Tacsat", years, ".RData"))


}
