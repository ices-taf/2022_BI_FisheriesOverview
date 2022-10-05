# Initial formatting of the data

library(icesTAF)
library(icesFO)
library(dplyr)

mkdir("data")

# load species list
species_list <- read.taf("bootstrap/data/FAO_ASFIS_species/species_list.csv")
sid <- read.taf("bootstrap/data/ICES_StockInformation/sid.csv")


# 1: ICES official catch statistics

hist <- read.taf("bootstrap/data/ICES_nominal_catches/ICES_historical_catches.csv")
official <- read.taf("bootstrap/data/ICES_nominal_catches/ICES_2006_2019_catches.csv")
prelim <- read.taf("bootstrap/data/ICES_nominal_catches/ICES_preliminary_catches.csv")

catch_dat <-
  format_catches(2021, "Bay of Biscay and the Iberian Coast",
    hist, official, prelim, species_list, sid)

write.taf(catch_dat, dir = "data", quote = TRUE)


# 2: SAG
sag_sum <- read.taf("bootstrap/data/SAG_data/SAG_summary.csv")
sag_refpts <- read.taf("bootstrap/data/SAG_data/SAG_refpts.csv")
sag_status <- read.taf("bootstrap/data/SAG_data/SAG_status.csv")

<<<<<<< HEAD
clean_sag <- format_sag(SAG_summary, SAG_refpts, 2021, "Biscay")
=======
clean_sag <- format_sag(sag_sum, sag_refpts, 2021, "Biscay")
>>>>>>> 126b60a802391bcf894b3d1a86a1ca3e3a43ffdf
clean_sag <- unique(clean_sag)
clean_status <- format_sag_status(sag_status, 2021, "Biscay")

write.taf(clean_sag, dir = "data")
write.taf(clean_status, dir = "data", quote = TRUE)

# 3: STECF effort and landings

effort <- read.taf("bootstrap/initial/data/Effort-by-country.csv", check.names = TRUE)
names(effort)
effort$sub.region <- tolower(effort$sub.region)
unique(effort$sub.region)
effort_BI <- dplyr::filter(effort, grepl("27.8.a|27.8.b|27.8.c|27.8.d|27.8.e|
                                          27.9.a|27.9.b", sub.region))





landings1 <- read.taf("bootstrap/initial/data/Catches-by-country-2018.csv", check.names = TRUE)
landings2 <- read.taf("bootstrap/initial/data/Catches-by-country-2017.csv", check.names = TRUE)
landings3 <- read.taf("bootstrap/initial/data/Catches-by-country-2016.csv", check.names = TRUE)
landings4 <- read.taf("bootstrap/initial/data/Catches-by-country-2015.csv", check.names = TRUE)
landings <- rbind(landings1, landings2, landings3, landings4)
names(landings)
landings$sub.region <- tolower(landings$sub.region)
landings_BI <- dplyr::filter(landings, grepl("27.8.a|27.8.b|27.8.c|27.8.d|27.8.e|
                                          27.9.a|27.9.b", sub.region))

# need to group gears, Sarah help.
unique(landings_BI$gear.type)
unique(effort_BI$gear.type)

landings_BI <- dplyr::mutate(landings_BI, gear_class = case_when(
  grepl("TBB", gear.type) ~ "Beam trawl",
  grepl("DRB|DRH|HMD", gear.type) ~ "Dredge",
  grepl("GNS|GND|GTN|LHP|LLS|FPN|GTR|FYK|LLD|SDN|LTL|LNB", gear.type) ~ "Static/Gill net/LL",
  grepl("OTT|OTB|PTB|SSC|SB|SPR|SV", gear.type) ~ "Otter trawl/seine",
  grepl("PTM|OTM|PS", gear.type) ~ "Pelagic trawl/seine",
  grepl("FPO", gear.type) ~ "Pots",
  grepl("NK|NO|LHM", gear.type) ~ "other",
  is.na(gear.type) ~ "other",
        TRUE ~ "other"
)
)

effort_BI <- dplyr::mutate(effort_BI, gear_class = case_when(
  grepl("TBB", gear.type) ~ "Beam trawl",
  grepl("DRB|DRH|HMD", gear.type) ~ "Dredge",
  grepl("GNS|GND|GTN|LHP|LLS|FPN|GTR|FYK|LLD|SDN|LTL|LNB", gear.type) ~ "Static/Gill net/LL",
  grepl("OTT|OTB|PTB|SSC|SB|SPR|SV", gear.type) ~ "Otter trawl/seine",
  grepl("PTM|OTM|PS", gear.type) ~ "Pelagic trawl/seine",
  grepl("FPO", gear.type) ~ "Pots",
  grepl("NK|NO|LHM", gear.type) ~ "other",
  is.na(gear.type) ~ "other",
        TRUE ~ "other"
)
)

unique(landings_BI[c("gear.type", "gear_class")])
unique(effort_BI[c("gear.type", "gear_class")])



