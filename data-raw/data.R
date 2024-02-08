### FERG2 / generate internal data
### .. country definitions
### .. UN WPP population estimates
### .. WHO worldmap shapefiles

## required packages
library(bd)
library(sf)
library(tidyr)
library(dplyr)
## country definitions
countries <- readxl("WHO_countries_subregions.xlsx")

## UN WPP population data
pop <-
  read.csv("WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv")
pop <- subset(pop, ISO3_code %in% countries$ISO3)
pop <- subset(pop, Time >= 1990)
pop <- subset(pop,
  select = c(ISO3_code, Time, AgeGrpStart, PopMale, PopFemale))
pop <-
pivot_longer(
  pop,
  cols = starts_with("Pop"),
  names_to = "SEX",
  names_prefix = "Pop",
  values_to = "POP")
names(pop) <- c("ISO3", "YEAR", "AGE", "SEX", "POP")
pop$POP <- pop$POP*1000
class(pop) <- "data.frame"
str(pop)

## WHO shapefiles
map1 <- st_read("shp/general_2013.shp")
map2 <- st_read("shp/maskline_general_2013.shp")
map3 <- st_read("shp/maskpoly_general_2013.shp")

###
### SAVE DATA
###

save(
  countries,
  pop,
  map1, map2, map3,
  file = "../R/sysdata.rda")