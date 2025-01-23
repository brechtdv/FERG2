### FERG2 / generate internal data
### .. country definitions
### .. UN WPP population estimates
### .. WHO worldmap shapefiles
### .. IHME population estimates for <1y
### .. UN life expectancy
### .. UN life birth

## required packages
library(bd)
library(sf)
library(tidyr)
library(dplyr)
library(data.table)
library(readxl)

## country definitions
countries <- readxl("WHO_countries_subregions.xlsx")

## UN WPP population data
## 1950-2021 only available on Brecht laptop
## 1950-2023 downloaded from https://population.un.org/wpp/Download/Standard/CSV/ on 22/07/24
## 1950-2023 downloaded from https://population.un.org/wpp/Download/Standard/Mortality/ on 03/12/24 to add LE
if (FALSE) {
# pop <-
  # read.csv("WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv")

pop <-
  fread("WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv.gz")


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

write.csv(pop,
  row.names = FALSE,
  file = "WPP2022_Population1JanuaryBySingleAgeSex_Medium_1990-2023.csv")
}

pop <-
  read.csv("WPP2022_Population1JanuaryBySingleAgeSex_Medium_1990-2023.csv")
str(pop)

LE <-
  read_excel("WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx", skip=16, col_types = "text")
LE <- LE %>%
  rename(ISO3 = `ISO3 Alpha-code`)
LE <- subset(LE, ISO3 %in% countries$ISO3)
LE <- subset(LE, Year >= 1990)
LE <- subset(LE,
              select = -c(Index, Variant, `Region, subregion, country or area *`, Notes, `Location code`,
                          `ISO2 Alpha-code`, `SDMX code**`, `Type`, `Parent code`))
colnames(LE) <- paste("A", colnames(LE), sep = "_")
LE <- LE %>%
  rename(ISO3 = A_ISO3, 
         YEAR = A_Year)
LE <- 
  pivot_longer(
    LE,
    cols = starts_with("A_"),
    names_to = "AGE",
    values_to = "LE")

LE$AGE <- sub(".*_", "", LE$AGE)

life_exp <- LE

write.csv(life_exp,
          row.names = FALSE,
          file = "WHO_life_expectancy_AGE_1990_2023.csv")
## UN WPP life birth data
LB <-
  read_excel("WPP2024_FERT_F03_BIRTHS_BY_SINGLE_AGE_OF_MOTHER.xlsx", skip=16, col_types = "text")
LB <- LB %>%
  rename(ISO3 = `ISO3 Alpha-code`)
LB <- subset(LB, ISO3 %in% countries$ISO3)
LB <- subset(LB, Year >= 1990)
LB <- subset(LB,
             select = -c(Index, Variant, `Region, subregion, country or area *`, Notes, `Location code`,
                         `ISO2 Alpha-code`, `SDMX code**`, `Type`, `Parent code`))
colnames(LB) <- paste("A", colnames(LB), sep = "_")
LB <- LB %>%
  rename(ISO3 = A_ISO3, 
         YEAR = A_Year)
LB <- 
  pivot_longer(
    LB,
    cols = starts_with("A_"),
    names_to = "AGE",
    values_to = "LB")

LB$AGE <- sub(".*_", "", LB$AGE)
LB$LB <- as.numeric(LB$LB)*1e3
life_birth <- LB

write.csv(life_birth,
          row.names = FALSE,
          file = "WHO_birth_by_age_mother_1990_2023.csv")

## IHME population data <1
pop_IHME_1 <-
  read.csv("IHME-GBD_2021_DATA_1990_2001_M.csv")
pop_IHME_2 <-
  read.csv("IHME-GBD_2021_DATA-2002_2013_M.csv")
pop_IHME_3 <-
  read.csv("IHME-GBD_2021_DATA-2014_2021_F.csv")
pop_IHME_4 <-
  read.csv("IHME-GBD_2021_DATA_1990_2001_F.csv")
pop_IHME_5 <-
  read.csv("IHME-GBD_2021_DATA-2002_2013_F.csv")
pop_IHME_6 <-
  read.csv("IHME-GBD_2021_DATA-2014_2021_M.csv")
pop_IHME <- rbind(pop_IHME_1, pop_IHME_2, pop_IHME_3,
                  pop_IHME_4, pop_IHME_5, pop_IHME_6)
pop_IHME <- pop_IHME[,c("location_name", "sex_name", "age_name","year", "val")]

pop_IHME <- pop_IHME %>%
  mutate(COUNTRY = case_when(
    location_name == "Türkiye" ~ "Turkiye",
    location_name == "Congo" ~ "Congo (the)",
    location_name == "Democratic Republic of the Congo" ~ "Congo (the Democratic Republic of the)",
    location_name == "Dominican Republic" ~ "Dominican Republic (the)",
    location_name == "Gambia" ~ "Gambia (the)",
    location_name == "Lao People's Democratic Republic" ~ "Lao People's Dem. Republic",
    location_name == "Micronesia (Federated States of)" ~ "Micronesia (Fed. States of)",
    location_name == "Democratic People's Republic of Korea" ~ "Korea (the Democratic People's Republic of)",
    location_name == "Republic of Korea" ~ "Korea (the republic of)",
    .default = location_name
  ))

pop_IHME <- subset(pop_IHME, COUNTRY %in% countries$COUNTRY)
pop_IHME <- merge(pop_IHME, countries[,c("COUNTRY","ISO3")])
pop_IHME <- pop_IHME[,c("ISO3","year","age_name","sex_name","val")]
colnames(pop_IHME) <- c("ISO3", "YEAR", "AGE", "SEX", "POP")

pop_1year <- pop_IHME
pop_1year <- pop_1year[order(pop_1year$ISO3, pop_1year$YEAR, pop_1year$AGE, pop_1year$SEX), ] 

## WHO shapefiles
map1 <- st_read("shp/general_2013.shp")
map2 <- st_read("shp/maskline_general_2013.shp")
map3 <- st_read("shp/maskpoly_general_2013.shp")

###
### SAVE DATA
###

# countries <- FERG2:::countries
# pop <- FERG2:::pop
# life_exp <- FERG2:::life_exp
# life_birth <- FERG2:::life_birth

save(
  countries, pop, pop_1year,
  life_exp,life_birth,
  map1, map2, map3,
  file = "../R/sysdata.rda")
