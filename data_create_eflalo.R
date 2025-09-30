
# Read in the look-up table
target_species <- read.csv("results/target_species.csv")


for(years in 2011:2024){

f1 <- read.table(paste0("data/", years, "/ERS/elektronisk-rapportering-ers-", years, "-ankomstmelding-por.csv"), header=T, sep = ";")
# the original data frame is named 'f1'

# Convert vessel length to use period as decimal separator
f1$Fartøylengde <- as.numeric(gsub(",", ".", f1$Fartøylengde))

aggregated_trips <- f1 %>%
  group_by(Radiokallesignal..ERS., Havn..kode., Ankomsttidspunkt) %>%
  summarise(
    VE_REF = first(Radiokallesignal..ERS.),
    VE_FLT = NA,
    VE_COU = "NO",
    VE_LEN = first(Fartøylengde),
    VE_KW = first(Motorkraft),
    VE_TON = first(Bruttotonnasje.1969),
    FT_LHAR = first(Havn..kode.),
    FT_LDAT = first(Ankomstdato),
    FT_LTIME = first(Ankomstklokkeslett),
    FT_LCOU = substr(first(Havn..kode.), 1, 2),
    .groups = "drop"
  ) %>%
  select(-Radiokallesignal..ERS., -Havn..kode., -Ankomsttidspunkt) %>%
  mutate(
    FT_LDAT = format(dmy(FT_LDAT), "%d/%m/%Y")
  ) %>%
  arrange(VE_REF, FT_LDAT) %>%
  as.data.frame()

# Display the first few rows of the new data frame
head(aggregated_trips)

f2 <- read.table(paste0("data/", years, "/ERS/elektronisk-rapportering-ers-", years, "-avgangsmelding-dep.csv"), header=T, sep = ";")


# Create the new aggregated data frame for departures
aggregated_departures <- f2 %>%
  group_by(Radiokallesignal..ERS., Havn..kode., Avgangstidspunkt) %>%
  summarise(
    VE_REF = first(Radiokallesignal..ERS.),
    FT_DHAR = first(Havn..kode.),
    FT_DDAT = first(Avgangsdato),
    FT_DTIME = first(Avgangsklokkeslett),
    FT_DCOU = substr(first(Havn..kode.), 1, 2),
    TA_TGT = first(Målart.FAO..kode.),
    .groups = "drop"
  ) %>%
  select(-Radiokallesignal..ERS., -Havn..kode., -Avgangstidspunkt) %>%
  mutate(
    FT_DDAT = as.Date(FT_DDAT, format = "%d.%m.%Y")
  ) %>%
  arrange(VE_REF, FT_DDAT) %>%
  as.data.frame()

# Display the first few rows of the new data frame
head(aggregated_departures)


# Convert date and time columns to datetime objects
aggregated_trips <- aggregated_trips %>%
  mutate(
    FT_LDAT = dmy(FT_LDAT), 
    FT_LTIME = hm(FT_LTIME),
    FT_LDATETIME = format(FT_LDAT + FT_LTIME, "%d/%m/%Y %H:%M:%S")
  ) %>%
  mutate(
    FT_LDAT = substr(FT_LDATETIME, 1, 10),
    FT_LTIME = substr(FT_LDATETIME, 12, 19)
  )

aggregated_departures <- aggregated_departures %>%
  mutate(
    FT_DTIME = hm(FT_DTIME),
    FT_DDATETIME = format(FT_DDAT + FT_DTIME, "%d/%m/%Y %H:%M:%S")
  ) %>%
  mutate(
    FT_DDAT = substr(FT_DDATETIME, 1, 10),
    FT_DTIME = substr(FT_DDATETIME, 12, 19)
  )


# Join departures to trips, specifying many-to-many relationship
joined_data <- aggregated_trips %>%
  left_join(
    aggregated_departures %>%
      select(VE_REF, FT_DDATETIME, FT_DHAR, FT_DDAT, FT_DTIME, TA_TGT),
    by = "VE_REF",
    relationship = "many-to-many"
  ) %>%
  mutate(
    FT_DDATETIME_parsed = dmy_hms(FT_DDATETIME),
    FT_LDATETIME_parsed = dmy_hms(FT_LDATETIME)
  ) %>%
  filter(FT_DDATETIME_parsed < FT_LDATETIME_parsed) %>%
  group_by(VE_REF, FT_LDATETIME) %>%
  slice_max(FT_DDATETIME_parsed, n = 1) %>%
  ungroup() %>%
  select(-FT_DDATETIME_parsed, -FT_LDATETIME_parsed) %>%
  mutate(
    FT_LDAT = format(as.Date(FT_LDATETIME, format = "%d/%m/%Y"), "%d/%m/%Y"),
    FT_DDAT = format(as.Date(FT_DDATETIME, format = "%d/%m/%Y"), "%d/%m/%Y"),
    FT_LTIME = format(as.POSIXct(FT_LDATETIME, format = "%d/%m/%Y %H:%M:%S"), "%H:%M:%S"),
    FT_DTIME = format(as.POSIXct(FT_DDATETIME, format = "%d/%m/%Y %H:%M:%S"), "%H:%M:%S")
  ) %>%
  as.data.frame()
# Display the first few rows of the joined data
head(joined_data)


f3 <- read.table(paste0("data/", years, "/ERS/elektronisk-rapportering-ers-", years, "-fangstmelding-dca.csv"), header=T, sep = ";")

f3 <- f3 %>%
  filter(Aktivitet %in% c("I fiske", "Setting av redskap", "Hive / trekke")) %>%
  mutate(
    SI_LATI = as.numeric(gsub(",", ".", Startposisjon.bredde)),
    SI_LONG = as.numeric(gsub(",", ".", Startposisjon.lengde)))

temp <- f3 %>% select(SI_LATI, SI_LONG)

f3$LE_RECT <- vmstools::ICESrectangle(temp)

f3 <- f3[f3$LE_RECT != "NANANA",]



# First, create the LE_ID column in f3

f3 <- f3 %>%
  mutate(
    combined_datetime = paste(Startdato, Startklokkeslett),
    parsed_datetime = dmy_hm(combined_datetime),
    LE_ID = paste0(Radiokallesignal..ERS., 
                   format(parsed_datetime, "%d%m%y"), 
                   LE_RECT, Redskap.FAO..kode.)
  )


aggregated_data <- f3 %>%
  mutate(
    parsed_datetime = dmy_hm(paste(Startdato, Startklokkeslett)),
    LE_CDAT = format(parsed_datetime, "%d/%m/%Y")
  ) %>%
  group_by(LE_ID) %>%
  summarise(
    VE_ID = first(Radiokallesignal..ERS.),
    LE_CDAT = first(LE_CDAT),
    LE_GEAR = first(Redskap.FAO..kode.),
    LE_MSZ = first(Redskap.maskevidde),
    LE_RECT = first(LE_RECT),
    .groups = "drop"
  )
# Now let's handle the catch data
catch_data <- f3 %>%
  group_by(LE_ID, Art.FAO..kode.) %>%
  summarise(
    LE_KG = sum(Rundvekt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Art.FAO..kode.,
    values_from = LE_KG,
    names_prefix = "LE_KG_",
    values_fill = 0
  ) %>%
  as.data.frame()

#aggregated_data$LE_ID <- paste0(aggregated_data$LE_ID, aggregated_data$LE_GEAR)

aggregated_data$joining_id <- substr(aggregated_data$LE_ID, 1, 10)
catch_data$joining_id <- substr(catch_data$LE_ID, 1, 10)

# Join the aggregated data with catch data
final_data <- left_join(
  aggregated_data,
  catch_data,
  by = "LE_ID"
)

# Reorder columns to match the specified format
#final_data <- final_data %>%
#  select(LE_ID, LE_CDAT, LE_GEAR, LE_MSZ, LE_RECT, starts_with("LE_KG_"))

# Display the first few rows of the final data
head(final_data)





# Function to generate all dates between two dates
date_sequence <- function(start_date, end_date) {
  seq(start_date, end_date, by = "day")
}

trip_days <- joined_data %>%
  mutate(
    FT_DDAT = dmy(FT_DDAT),  # Use dmy() from lubridate
    FT_LDAT = dmy(FT_LDAT)   # Use dmy() from lubridate
  ) %>%
  rowwise() %>%
  mutate(
    date = list(date_sequence(FT_DDAT, FT_LDAT))
  ) %>%
  unnest(date) %>%
  mutate(
    date_string = format(date, "%d%m%y"),
    trip_day_id = paste0(VE_REF, date_string)
  )

trip_days <- joined_data %>%
  mutate(
    FT_DDAT = dmy(FT_DDAT),  # Use dmy() from lubridate
    FT_LDAT = dmy(FT_LDAT)   # Use dmy() from lubridate
  ) %>%
  rowwise() %>%
  mutate(
    date = list(date_sequence(FT_DDAT, FT_LDAT))
  ) %>%
  unnest(date) %>%
  mutate(
    date_string = format(date, "%d%m%y"),
    trip_day_id = paste0(VE_REF, date_string)
  )

# First, let's aggregate the trip_days data
aggregated_trips <- trip_days %>%
  group_by(VE_REF, date_string) %>%
  summarise(
    VE_FLT = first(VE_FLT),
    VE_COU = first(VE_COU),
    VE_LEN = first(VE_LEN),
    VE_KW = first(VE_KW),
    VE_TON = first(VE_TON),
    FT_LHAR = last(FT_LHAR),
    FT_LDAT = last(FT_LDAT),
    FT_LCOU = last(FT_LCOU),
    FT_LDATETIME = max(FT_LDATETIME),
    FT_DDATETIME = min(FT_DDATETIME),
    FT_DHAR = first(FT_DHAR),
    FT_DDAT = first(FT_DDAT),
    TA_TGT = first(unique(TA_TGT)),
    date = first(date),
    .groups = "drop"
  ) %>%
  mutate(
    FT_LTIME = substr(FT_LDATETIME, 12, 19),
    FT_DTIME = substr(FT_DDATETIME, 12, 19),
    FT_LDAT = substr(FT_LDATETIME, 1, 10),
    FT_DDAT = substr(FT_DDATETIME, 1, 10)
      ) %>%
  as.data.frame()


aggregated_trips$joining_id <- paste0(aggregated_trips$VE_REF, aggregated_trips$date_string)

final_data <- final_data %>%
  select(-joining_id.y) %>%
  rename(joining_id = joining_id.x)


# Now, let's join this with the catch data
combined_data <- aggregated_trips %>%
  left_join(final_data, by = c("joining_id"))


eflalo <- combined_data %>%
  mutate(
    FT_REF = LE_ID,  
    LE_CDAT = date,
    FT_DTIME = substr(FT_DDATETIME, 12, 19),
    FT_DCOU = substr(FT_DHAR, 1, 2),
    FT_LCOU = substr(FT_LHAR, 1, 2),
    LE_MET = sapply(TA_TGT, find_l5_met)  # Replace species codes with L5_MET
  ) %>%
  select(
    VE_REF, VE_FLT, VE_COU, VE_LEN, VE_KW, VE_TON, FT_REF, FT_DCOU,
    FT_DHAR, FT_DDAT, FT_DTIME, FT_LCOU, FT_LHAR, FT_LDAT, FT_LTIME,
    LE_ID, LE_CDAT, LE_GEAR, LE_MSZ, LE_RECT, LE_MET,
    # Catch information
    sort(names(.)[grep("^LE_KG_", names(.))])
  ) %>%
  arrange(VE_REF, FT_DDAT, FT_DTIME) %>%
  filter(!is.na(FT_REF))


save(eflalo, file = paste0("results/Eflalo", years, ".RData"))

}
