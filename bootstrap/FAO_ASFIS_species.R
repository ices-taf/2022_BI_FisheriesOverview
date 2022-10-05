
library(icesTAF)
library(icesFO)

species_list <- load_asfis_species()

write.taf(species_list, quote=TRUE)
