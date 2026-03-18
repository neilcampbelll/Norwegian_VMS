# Read in the look-up table
target_species <- read.csv("results/target_species.csv")

# load the metier list
m6_ices <- icesVocab::getCodeList("Metier6_FishingActivity")

## Clean up the gear codes with a look-up table developed in data_invalid_gears.R

# define a gear mapping - this is my best guess, being precautionary e.g. al LL are LLS
gear_mapping <- tribble(
  ~LE_GEAR, ~correct_gear, ~action,    ~notes,
  "TBS",    "TBB",         "replace",  "Trawl, beam, shrimp -> Beam trawl",
  "PS1",    "PS",          "replace",  "Purse seine type 1 -> Purse seine",
  "PS2",    "PS",          "replace",  "Purse seine type 2 -> Purse seine",
  "GEN",    "GTR",         "replace",  "Trammel net (sole) -> Trammel net",
  "LL",     "LLS",         "replace",  "Longline -> Set longline (precautionary, seabed impact)",
  "TB",     "TBB",         "replace",  "Trawl, beam -> Beam trawl",
  "SV",     "OTB",         "replace",  "Unknown code -> Bottom otter trawl",
  "TM",     "OTM",         "replace",  "Midwater trawl -> Midwater otter trawl",
  "TBN",    "OTB",         "replace",  "Bottom trawl, nephrops -> Bottom otter trawl",
  "TMS",    "OTM",         "replace",  "Midwater trawl variant -> Midwater otter trawl",
  "HAR",    NA_character_, "exclude",  "Harpoons - excluded from analysis",
  "HMP",    "LHM",         "replace",  "Hand and pole lines -> Hand and pole lines (mechanised)",
  "OT",     "OTB",         "replace",  "Otter trawl -> Bottom otter trawl",
  "SND",    "SDN",         "replace",  "Mistyping of Danish seine",
  "NK",     "MIS",         "replace",  "Not known -> Miscellaneous gear"
)

# --- Build metier lookup and gear-target ranges ONCE outside the loop ---
metier_lookup <- m6_ices %>%
  mutate(components = strsplit(Key, "_")) %>%
  rowwise() %>%
  mutate(
    L4_gear = components[1],
    L5_target = components[2],
    mesh_range = components[3],
    sel_dev1 = components[4],
    sel_dev2 = components[5]
  ) %>%
  ungroup() %>%
  select(L4_gear, L5_target, mesh_range, sel_dev1, sel_dev2) %>%
  filter(sel_dev1 == "0" & sel_dev2 == "0")

gear_target_ranges <- metier_lookup %>%
  group_by(L4_gear, L5_target) %>%
  summarise(available_ranges = list(mesh_range), .groups = "drop")


for (years in (2011:2025)) {
  
  # load the different ERS files
  f1 <- read_ers_file(paste0("data/", years, "/ERS/elektronisk-rapportering-ers-", years, "-ankomstmelding-por.csv")) %>% 
    rename_with(~make.names(.))
  
  f2 <- read_ers_file(paste0("data/", years, "/ERS/elektronisk-rapportering-ers-", years, "-avgangsmelding-dep.csv")) %>% 
    rename_with(~make.names(.))
  
  f3 <- read_ers_file(paste0("data/", years, "/ERS/elektronisk-rapportering-ers-", years, "-fangstmelding-dca.csv")) %>% 
    rename_with(~make.names(.))
  
  # process arrivals
  trip_arrivals <- f1 %>%
    mutate(
      Fartøylengde = as.numeric(gsub(",", ".", Fartøylengde)),
      ArrivalDT = parse_no_datetime(Ankomstdato, Ankomstklokkeslett)
    ) %>%
    group_by(Radiokallesignal..ERS., ArrivalDT) %>%
    summarise(
      VE_REF = first(Radiokallesignal..ERS.),
      VE_LEN = first(Fartøylengde),
      VE_KW = first(Motorkraft),
      VE_TON = first(Bruttotonnasje.1969),
      FT_LHAR = first(Havn..kode.),
      FT_LCOU = substr(first(Havn..kode.), 1, 2),
      .groups = "drop"
    )
  
  # process departures
  trip_departures <- f2 %>%
    mutate(DepartureDT = parse_no_datetime(Avgangsdato, Avgangsklokkeslett)) %>%
    group_by(Radiokallesignal..ERS., DepartureDT) %>%
    summarise(
      VE_REF = first(Radiokallesignal..ERS.),
      FT_DHAR = first(Havn..kode.),
      FT_DCOU = substr(first(Havn..kode.), 1, 2),
      TA_TGT = first(Målart.FAO..kode.),
      .groups = "drop"
    )
  
  # join arrivals to departures
  joined_data <- trip_arrivals %>%
    inner_join(trip_departures, by = "VE_REF") %>%
    filter(DepartureDT < ArrivalDT) %>%
    group_by(VE_REF, ArrivalDT) %>%
    slice_max(DepartureDT, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    group_by(VE_REF, DepartureDT) %>%
    slice_min(ArrivalDT, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      FT_LDAT = format(ArrivalDT, "%d/%m/%Y"),
      FT_LTIME = format(ArrivalDT, "%H:%M:%S"),
      FT_DDAT = format(DepartureDT, "%d/%m/%Y"),
      FT_DTIME = format(DepartureDT, "%H:%M:%S"),
      FT_LDATETIME = format(ArrivalDT, "%d/%m/%Y %H:%M:%S"),
      FT_DDATETIME = format(DepartureDT, "%d/%m/%Y %H:%M:%S")
    )
  
  # process catch
  f3 <- f3 %>%
    filter(Aktivitet %in% c("I fiske", "Setting av redskap", "Hive / trekke")) %>%
    mutate(
      SI_LATI = as.numeric(gsub(",", ".", Startposisjon.bredde)),
      SI_LONG = as.numeric(gsub(",", ".", Startposisjon.lengde)),
      Redskap.maskevidde = as.numeric(gsub(",", ".", Redskap.maskevidde)),
      LE_CDAT_DT = parse_no_datetime(Startdato, as.character(Startklokkeslett))
    ) %>%
    filter(!is.na(LE_CDAT_DT)) 
  temp_coords <- as.data.frame(f3 %>% select(SI_LATI, SI_LONG))
  f3$LE_RECT <- vmstools::ICESrectangle(temp_coords)
  
  # Clean records that failed rectangle assignment
  f3 <- f3[f3$LE_RECT != "NANANA", ]
  
  # generate LE_IDs
  f3 <- f3 %>%
    mutate(
      LE_ID = paste0(Radiokallesignal..ERS., format(LE_CDAT_DT, "%d%m%y"), 
                     LE_RECT, Redskap.FAO..kode.)
    )
  
  # Aggregate Logbook and Catch Data
  aggregated_catch <- f3 %>%
    group_by(LE_ID) %>%
    summarise(
      VE_ID = first(Radiokallesignal..ERS.),
      LE_CDAT = first(LE_CDAT_DT),
      LE_GEAR = first(Redskap.FAO..kode.),
      LE_MSZ = first(Redskap.maskevidde),
      LE_RECT = first(LE_RECT),
      .groups = "drop"
    )
  
  species_weights <- f3 %>%
    group_by(LE_ID, Art.FAO..kode.) %>%
    summarise(LE_KG = sum(Rundvekt, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Art.FAO..kode., values_from = LE_KG, 
                names_prefix = "LE_KG_", values_fill = 0)
  
  final_catch_data <- left_join(aggregated_catch, species_weights, by = "LE_ID")
  
  # expand trips to days
  trip_days <- joined_data %>%
    mutate(
      start_date = as.Date(DepartureDT),
      end_date = as.Date(ArrivalDT)
    ) %>%
    rowwise() %>%
    mutate(date = list(seq(start_date, end_date, by = "day"))) %>%
    unnest(date) %>%
    mutate(
      joining_id = paste0(VE_REF, format(date, "%d%m%y"))
    )
  
  # merge data
  eflalo <- trip_days %>%
    left_join(
      final_catch_data %>% mutate(joining_id = substr(LE_ID, 1, 10)), 
      by = "joining_id",
      relationship = "many-to-many" 
    ) %>%
    mutate(
      FT_REF = paste0(VE_REF, format(DepartureDT, "%d%m%y%H%M"), format(ArrivalDT, "%d%m%y%H%M")),
      LE_CDAT = date,
      FT_DCOU = substr(FT_DHAR, 1, 2),
      FT_LCOU = substr(FT_LHAR, 1, 2)
    )
  
  # Derive ICES division from rectangle
  unique_rects <- unique(eflalo$LE_RECT[!is.na(eflalo$LE_RECT)])
  rect_coords <- ICESrectangle2LonLat(unique_rects)
  names(rect_coords) <- c("SI_LATI", "SI_LONG")
  
  valid <- !is.na(rect_coords$SI_LATI)
  rect_sf <- st_as_sf(rect_coords[valid, ], coords = c("SI_LONG", "SI_LATI"), crs = 4326)
  nearest_idx <- st_nearest_feature(rect_sf, ICESareas)
  
  rect_lookup <- data.frame(
    LE_RECT = unique_rects[valid],
    LE_DIV = ICESareas$Area_Full[nearest_idx]
  )
  eflalo$LE_DIV <- rect_lookup$LE_DIV[match(eflalo$LE_RECT, rect_lookup$LE_RECT)]
  
  eflalo <- eflalo %>%
    mutate(across(starts_with("LE_KG_"), ~replace_na(., 0))) %>%
    arrange(VE_REF, FT_DDAT) %>%
    group_by(FT_REF) %>%
    fill(LE_GEAR, LE_MSZ, LE_RECT, .direction = "updown") %>%
    filter(!is.na(LE_ID)) %>%
    ungroup() %>%
    as.data.frame()
  
  eflalo$VE_FLT <- NA
  eflalo$VE_COU <- "NO"
  
  # --- Assign initial metier (L5) from target species ---
  eflalo$LE_MET <- sapply(eflalo$TA_TGT, find_l5_met)
  
  # --- Fix invalid gear codes ---
  eflalo <- eflalo %>%
    left_join(gear_mapping, by = "LE_GEAR") %>%
    mutate(
      LE_GEAR = case_when(
        action == "exclude" ~ NA_character_,
        action == "replace" ~ correct_gear,
        TRUE ~ LE_GEAR
      )
    ) %>%
    filter(!is.na(LE_GEAR)) %>%
    select(-correct_gear, -action, -notes)
  
  # --- Construct L6 metier ---
  eflalo <- eflalo %>%
    rowwise() %>%
    mutate(
      LE_MET = construct_metier_l6(LE_GEAR, LE_MET, LE_MSZ, gear_target_ranges)
    ) %>%
    ungroup()
  
  # --- Select standard columns and format ---
  eflalo_standard_names <- c(
    "VE_REF", "VE_FLT", "VE_COU", "VE_LEN", "VE_KW", "VE_TON",
    "FT_REF", "FT_DCOU", "FT_DHAR", "FT_DDAT", "FT_DTIME",
    "FT_LCOU", "FT_LHAR", "FT_LDAT", "FT_LTIME",
    "LE_ID", "LE_CDAT", "LE_GEAR", "LE_MSZ", "LE_RECT", "LE_DIV", "LE_MET"
  )
  catch_cols <- sort(grep("^LE_KG_", names(eflalo), value = TRUE))
  
  eflalo <- eflalo %>%
    select(
      all_of(eflalo_standard_names),
      all_of(catch_cols)
    ) %>%
    as.data.frame()  
  
  eflalo <- vmstools::formatEflalo(eflalo)
  
  save(eflalo, file = paste0("results/eflalo_", years, ".RData"))
  
  print(paste("Completed year:", years))
}
