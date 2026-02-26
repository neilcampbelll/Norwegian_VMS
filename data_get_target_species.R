target.sp <- NULL

for(years in 2011:2025){
print(years)
f2 <- read.table(paste0("data/", years, "/ERS/elektronisk-rapportering-ers-", years, "-avgangsmelding-dep.csv"), header=T, sep = ";")

target.sp <- c(target.sp, unique(f2$Målart.FAO..kode.))

}

target.sp <- as.data.frame(unique(target.sp))
colnames(target.sp) <- "Target_Species"



write.csv(target.sp, "results/target_species.csv", row.names = F)
