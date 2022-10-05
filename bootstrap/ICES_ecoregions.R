library(icesTAF)
library(icesFO)

ecoregion <- icesFO::load_ecoregion("Bay of Biscay and the Iberian Coast")

sf::st_write(ecoregion, "ecoregion.csv", layer_options = "GEOMETRY=AS_WKT")
