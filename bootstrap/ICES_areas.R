library(icesTAF)
taf.library(icesFO)

areas <- load_areas("Bay of Biscay and the Iberian Coast")

sf::st_write(areas, "areas.csv", layer_options = "GEOMETRY=AS_WKT")
