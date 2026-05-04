library(sf)
library(tidyverse)

# Load shapefiles
load("../pepfar-census_poly-match/outputs/objects/shp_lsts.rda")
pepfar <- cou_shp_lst$malawi$adm3 %>% st_make_valid()
census <- census_shp_lst$malawi$adm2 %>% st_make_valid()

ONE_TO_ONE_THRESHOLD <- 0.85
MANY_TO_ONE_THRESHOLD <- 0.95
CENSUS_CONTRIBUTION_THRESHOLD <- 0.05
BUFFER_DIST_IN_METERS <- 400
