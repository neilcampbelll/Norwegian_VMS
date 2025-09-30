# Create the species to target assemblage mapping

# Demersal fish (DEF)
demersal <- c("COD", "HAD", "POK", "WHG", "LIN", "USK", "HAL", "GHL", "REB", "LEM",
"PLE", "DAB", "TUR", "SOL", "WIT", "BLI", "MON", "CAT", "RED", "REG", "GAD",
"GUG", "HKE", "POL", "NOP", "ARY", "CUS", "GDG", "MZZ", "GFB", "RHG",
"POR", "PLA", "PLZ", "RAJ", "RJB", "GAD", "ETX", "FBM", "HKN", "BOG",
"SLX", "KCD", "LUM", "BIB", "CAB", "ACH", "HKX", "BOW", "KON", "TOA", "ANF", "SAI", "POC", "ASK","DGS")

  crustaceans <-  c("NEP", "PRA", "PAN", "CRQ", "KRI", "CRN", "KCS", "CRU", "KCX", "PAL", "JCM", "LBA", "NKR", "AAS","PEN")

  small.pelagics <- c("HER", "SPR", "MAC", "CAP", "ANE", "PIL", "HOM", "WHB", "ARU", "SAN", "JAX", "ANI",
                    "USP", "MAV", "BHG", "LXX", "MUL", "ARG")

  # Large pelagic fish (LPF)
  large.pelagics <- c("BFT", "ALB", "SWR", "SKJ")

  cephalopods <- c("SQE")

   # Molluscs (MOL)
    molluscs <- c("KAM", "MAX", "ECH", "CLU", "CLS", "ISC", "CLB", "WHE", "COC")
    
    # Anadromous (ANA)
    anadromous <- c("SAL", "PIN","CHR")
    
    # Catadromous (CAT - note: same code as catfish, context dependent)
    catadromous <- c()
    # Deepwater species (DWS)
    deepwaters <- c("ORY", "RNG", "SBR", "ALF", "CMO", "SRX", "CAS", "SRR", "KEF","COE","CFB")
    
    # Mixed/Other (MIS)
    misc.species <- c(  "MIW", "TVA", "ANI", "LAH", "LWS",
                   "TVK", "URC", "SIW", "SEH")


species_mapping <- data.frame(
  Species_Code = c( demersal, crustaceans, small.pelagics, large.pelagics, cephalopods,
                    molluscs, anadromous, catadromous, deepwaters, misc.species),
  Target_Assemblage = c(
    # Demersal fish
    rep("DEF", length(demersal)),
    
    # Crustaceans
    rep("CRU", length(crustaceans)),
    
    # Small pelagic
    rep("SPF", length(small.pelagics)),
    
    # Large pelagic
    rep("LPF", length(large.pelagics)),
    
    # Cephalopods
    rep("CEP", length(cephalopods)),
    
    # Molluscs
    rep("MOL", length(molluscs)),
    
    # Anadromous
    rep("ANA", length(anadromous)),
    
    # Catadromous
    rep("CAT", length(catadromous)),
    
    # Deepwater
    rep("DWS", length(deepwaters)),
    
    # Mixed/Other
    rep("MIS", length(misc.species))
  )
)

# Save the mapping
write.csv(species_mapping, "results/target_species.csv", row.names = FALSE)

