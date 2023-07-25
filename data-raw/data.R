### FERG2 / generate internal data
### .. country definitions

## required packages
library(bd)
library(sf)

## import data
countries <- readxl("WHO_countries_subregions.xlsx")
map1 <- st_read("shp/general_2013.shp")
map2 <- st_read("shp/maskline_general_2013.shp")
map3 <- st_read("shp/maskpoly_general_2013.shp")

###
### SAVE DATA
###

save(
  countries,
  map1, map2, map3,
  file = "../R/sysdata.rda")