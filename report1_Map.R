
library(icesTAF)
library(icesFO)
library(sf)
library(ggplot2)
library(dplyr)


## Run utilies
source("bootstrap/utilities.r")

# set values for automatic naming of files:
cap_year <- 2021
cap_month <- "November"
ecoreg_code <- "BI"
ecoreg <- "BI"


mkdir("report")

# set values for automatic naming of files:
<<<<<<< HEAD
year_cap = "2021"
ecoreg = "BI"
=======
# year_cap = "2020"
# ecoreg = "BI"
>>>>>>> 126b60a802391bcf894b3d1a86a1ca3e3a43ffdf

##########
#Load data
##########

ices_areas <-
  sf::st_read("areas.csv",
              options = "GEOM_POSSIBLE_NAMES=WKT", crs = 4326)
ices_areas <- dplyr::select(ices_areas, -WKT)

ecoregion <-
  sf::st_read("ecoregion.csv",
              options = "GEOM_POSSIBLE_NAMES=WKT", crs = 4326)
ecoregion <- dplyr::select(ecoregion, -WKT)

###############
##Ecoregion map
###############

plot_ecoregion_map(ecoregion, ices_areas)
ggplot2::ggsave(file_name(cap_year,ecoreg_code,"Figure1", ext = "png"), path = "report", width = 170, height = 200, units = "mm", dpi = 300)


