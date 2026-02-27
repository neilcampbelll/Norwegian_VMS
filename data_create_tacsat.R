for (years in (2011:2025)){

  ## note - the file naming protocol changes in 2022 - this is a bit of a fix - not ideal
  vms_file <- list.files(
    path = paste0("data/", years, "/VMS/"),
    pattern = paste0("^(VMS[_-]", years, "|", years, "-VMS)\\.csv$"),
    full.names = TRUE
  )
  
  # reads in the file
  no.vms <- read.table(vms_file, header = T, sep = ";", dec = ",")

no.vms <- as.data.frame(no.vms)

tacsat2 <- no.vms %>%
  select(
    VE_REF = Radiokallesignal,
    SI_LATI = Breddegrad,
    SI_LONG = Lengdegrad,
    datetime = matches("Tidspunkt.*UTC"),
    SI_SP = Fart,
    SI_HE = Kurs
  ) %>%
  mutate(
    #  remove trailing zeroes
    datetime_clean = sub("[,.].0{5,}.*$", "", datetime), 
    
    # handling the multiple date formats (check each year!)
    datetime = parse_date_time(datetime_clean, orders = c(
      "d.m.Y H:M:S",      # Original (2011 - 2021)
      "d.m.Y H.M.S",      # 2022 & 2023 format
      "d-b-y H.M.S"       # 2024 format (e.g., 01-JAN-24)
    )),
    
    SI_DATE = format(datetime, "%d/%m/%Y"),
    SI_TIME = format(datetime, "%H:%M")
  ) %>%
  select(VE_REF, SI_LATI, SI_LONG, SI_DATE, SI_TIME, SI_SP, SI_HE)

print(years)

save(tacsat2, file = paste0("results/tacsat_", years, ".RData"))


}
