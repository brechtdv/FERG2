### FERG2 / generate internal data
### .. country definitions

## required packages
library(bd)

## import data
countries <- readxl("WHO_countries_subregions.xlsx")


###
### SAVE DATA
###

save(
  countries,
  file = "../R/sysdata.rda")