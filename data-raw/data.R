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
library(stringr)

## country definitions
countries <- readxl("WHO_countries_subregions.xlsx")
# Add official WHO names
countries_WHO <- readxl("data_export_REFMART_REF_COUNTRY (2).xlsx")
str(countries_WHO)
countries_WHO <- countries_WHO[,c("CODE_ISO_3", "NAME_SHORT_EN")]
names(countries_WHO) <- c("ISO3", "COUNTRY_WHO")
countries <- left_join(countries, countries_WHO, by = "ISO3")
# Add official WHO order
countries_WHO <- readxl("data_export_REFMART_REF_COUNTRY (2).xlsx")
str(countries_WHO)
countries_WHO <- countries_WHO[,c("CODE_ISO_3", "CODE_ISO_NUMERIC")]
names(countries_WHO) <- c("ISO3", "COUNTRY_WHO_ORDER")
countries <- left_join(countries, countries_WHO, by = "ISO3")

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
    location_name == "Turkey" ~ "Turkiye",
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
class(pop_1year) <- "data.frame"
rownames(pop_1year) <- NULL
str(pop_1year)

write.csv(pop_1year,
          row.names = FALSE,
          file = "IHME_pop_1y_1990_2021.csv")

## IHME population data all
IHME_pop_1 <-
  read.csv("IHME-GBD_2021_DATA-95f2786f-1.csv")
IHME_pop_2 <-
  read.csv("IHME-GBD_2021_DATA-95f2786f-2.csv")

pop_IHME <- rbind(IHME_pop_1, IHME_pop_2)
pop_IHME <- pop_IHME[, c("location_name", "sex_name", "age_name", "year", "val")]

pop_IHME <- pop_IHME %>%
  mutate(COUNTRY = case_when(
    location_name == "Türkiye" ~ "Turkiye",
    location_name == "Turkey" ~ "Turkiye",
    location_name == "Congo" ~ "Congo (the)",
    location_name == "Democratic Republic of the Congo" ~ "Congo (the Democratic Republic of the)",
    location_name == "Dominican Republic" ~ "Dominican Republic (the)",
    location_name == "Gambia" ~ "Gambia (the)",
    location_name == "Lao People's Democratic Republic" ~ "Lao People's Dem. Republic",
    location_name == "Micronesia (Federated States of)" ~ "Micronesia (Fed. States of)",
    location_name == "Democratic People's Republic of Korea" ~ "Korea (the Democratic People's Republic of)",
    location_name == "Republic of Korea" ~ "Korea (the republic of)",
    location_name == "Arab Republic of Egypt" ~ "Egypt",
    location_name == "Argentine Republic" ~ "Argentina",
    location_name == "Bolivarian Republic of Venezuela" ~ "Venezuela (Bolivarian Republic of)",
    location_name == "Czech Republic" ~ "Czechia",
    location_name == "Democratic Republic of Sao Tome and Principe" ~ "Sao Tome and Principe",
    location_name == "Democratic Republic of Timor-Leste" ~ "Timor-Leste",
    location_name == "Democratic Socialist Republic of Sri Lanka" ~ "Sri Lanka",
    location_name == "Eastern Republic of Uruguay" ~ "Uruguay",
    location_name == "Federal Democratic Republic of Ethiopia" ~ "Ethiopia",
    location_name == "Federal Democratic Republic of Nepal" ~ "Nepal",
    location_name == "Federal Republic of Germany" ~ "Germany",
    location_name == "Federal Republic of Nigeria" ~ "Nigeria",
    location_name == "Federal Republic of Somalia" ~ "Somalia",
    location_name == "Federated States of Micronesia" ~ "Micronesia (Fed. States of)",
    location_name == "Federative Republic of Brazil" ~ "Brazil",
    location_name == "French Republic" ~ "France",
    location_name == "Gabonese Republic" ~ "Gabon",
    location_name == "Grand Duchy of Luxembourg" ~ "Luxembourg",
    location_name == "Hashemite Kingdom of Jordan" ~ "Jordan",
    location_name == "Independent State of Papua New Guinea" ~ "Papua New Guinea",
    location_name == "Independent State of Samoa" ~ "Samoa",
    location_name == "Islamic Republic of Afghanistan" ~ "Afghanistan",
    location_name == "Islamic Republic of Iran" ~ "Iran (Islamic Republic of)",
    location_name == "Islamic Republic of Mauritania" ~ "Mauritania",
    location_name == "Islamic Republic of Pakistan" ~ "Pakistan",
    location_name == "Kingdom of Bahrain" ~ "Bahrain",
    location_name == "Kingdom of Belgium" ~ "Belgium",
    location_name == "Kingdom of Bhutan" ~ "Bhutan",
    location_name == "Kingdom of Cambodia" ~ "Cambodia",
    location_name == "Kingdom of Denmark" ~ "Denmark",
    location_name == "Kingdom of Eswatini" ~ "Eswatini",
    location_name == "Kingdom of Lesotho" ~ "Lesotho",
    location_name == "Kingdom of Morocco" ~ "Morocco",
    location_name == "Kingdom of Norway" ~ "Norway",
    location_name == "Kingdom of Saudi Arabia" ~ "Saudi Arabia",
    location_name == "Kingdom of Spain" ~ "Spain",
    location_name == "Kingdom of Sweden" ~ "Sweden",
    location_name == "Kingdom of Thailand" ~ "Thailand",
    location_name == "Kingdom of the Netherlands" ~ "Netherlands",
    location_name == "Kingdom of Tonga" ~ "Tonga",
    location_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
    location_name == "Lebanese Republic" ~ "Lebanon",
    location_name == "People's Democratic Republic of Algeria" ~ "Algeria",
    location_name == "People's Republic of Bangladesh" ~ "Bangladesh",
    location_name == "People's Republic of China" ~ "China",
    location_name == "Plurinational State of Bolivia" ~ "Bolivia (Plurinational State of)",
    location_name == "Portuguese Republic" ~ "Portugal",
    location_name == "Republic of the Congo" ~ "Congo (the)",
    location_name == "Republic of the Gambia" ~ "Gambia (the)",
    location_name == "Republic of the Marshall Islands" ~ "Marshall Islands",
    location_name == "Republic of the Niger" ~ "Niger",
    location_name == "Republic of the Philippines" ~ "Philippines",
    location_name == "Republic of the Union of Myanmar" ~ "Myanmar",
    location_name == "Republic of Turkey" ~ "Turkiye",
    location_name == "Republic of Moldova" ~ "Republic of Moldova",
    location_name == "Commonwealth of Dominica" ~ "Dominica",
    substr(location_name, 1, 12) == "Republic of " ~ substr(location_name,13, str_count(location_name)),
    location_name == "Principality of Andorra" ~ "Andorra",
    location_name == "Principality of Monaco" ~ "Monaco",
    location_name == "Slovak Republic" ~ "Slovakia",
    location_name == "Socialist Republic of Viet Nam" ~ "Viet Nam",
    substr(location_name, 1, 9) == "State of " ~ substr(location_name, 10, str_count(location_name)),
    location_name == "Sultanate of Oman" ~ "Oman",
    location_name == "Swiss Confederation" ~ "Switzerland",
    location_name == "Togolese Republic" ~ "Togo",
    location_name == "Union of the Comoros" ~ "Comoros",
    location_name == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
    location_name == "United Mexican States" ~ "Mexico",
    location_name == "Commonwealth of the Bahamas" ~ "Bahamas",
    location_name == "Hellenic Republic" ~ "Greece",
    .default = location_name
  ))

pop_IHME <- subset(pop_IHME, COUNTRY %in% countries$COUNTRY)
pop_IHME <- merge(pop_IHME, countries[, c("COUNTRY", "ISO3")])
pop_IHME <- pop_IHME[, c("ISO3", "year", "age_name", "sex_name", "val")]
colnames(pop_IHME) <- c("ISO3", "YEAR", "AGE", "SEX", "POP")
table(pop_IHME$AGE)
pop_IHME <-
  subset(pop_IHME,
         AGE %in%
           c("<1 year", "12-23 months", "2-4 years", "5-9 years",
             "10-14 years", "15-19 years", "20-24 years", "25-29 years",
             "30-34 years", "35-39 years", "40-44 years", "45-49 years",
             "50-54 years", "55-59 years", "60-64 years", "65-69 years",
             "70-74 years", "75-79 years", "80-84 years", "85-89 years",
             "90-94 years", "95+ years"))

# IHME_pop <- IHME_pop %>% 
#   mutate(AGE = case_when(
#     age_name == "<1 year" ~ "0-1", 
#     age_name == "2-4 years"~ "2-4", 
#     age_name == "5-9 years" ~ "5-9",
#     age_name == "10-14 years" ~ "10-14",
#     age_name == "15-19 years" ~ "15-19",
#     age_name == "20-24 years" ~ "20-24",
#     age_name == "25-29 years" ~ "25-29",
#     age_name == "30-34 years" ~ "30-34",
#     age_name == "35-39 years" ~ "35-39",
#     age_name == "40-44 years" ~ "40-44",
#     age_name == "45-49 years" ~ "45-49",
#     age_name == "50-54 years" ~ "50-54",
#     age_name == "55-59 years" ~ "55-59",
#     age_name == "60-64 years" ~ "60-64",
#     age_name == "65-69 years" ~ "65-69",
#     age_name == "70-74 years" ~ "70-74",
#     age_name == "75-79 years" ~ "75-79",
#     age_name == "80-84 years" ~ "80-84",
#     age_name == "85-89 years" ~ "85-125",
#     age_name == "90-94 years" ~ "85-125",
#     age_name == "95+ years" ~ "85-125"))
# 
# IHME_pop$age_name <- NULL
# IHME_pop <- IHME_pop %>% 
#   group_by(ISO3, YEAR, SEX, AGE) %>% 
#   summarise(POP = sum(POP))
# 
# IHME_pop <- arrange(IHME_pop, ISO3, YEAR, SEX, readr::parse_number(AGE))
# pop_IHME <- IHME_pop

## WHO shapefiles
map1 <- st_read("shp/general_2013.shp")
map2 <- st_read("shp/maskline_general_2013.shp")
map3 <- st_read("shp/maskpoly_general_2013.shp")

## scale POP in pop_1year

if (FALSE) {
  countries <- FERG2:::countries
  pop <- FERG2:::pop
  life_exp <- FERG2:::life_exp
  life_birth <- FERG2:::life_birth
  pop_1year <- FERG2:::pop_1year
  pop_IHME <- FERG2:::pop_IHME
}

head(pop_1year); head(pop)

pop_1year_agg <- aggregate(POP ~ ISO3 + YEAR + SEX, pop_1year, sum)
names(pop_1year_agg)[names(pop_1year_agg) == "POP"] <- "POP_IHME"
head(pop_1year_agg)

pop_sub1 <- subset(pop, AGE == 0)
pop_sub1$AGE <- NULL
names(pop_sub1)[names(pop_sub1) == "POP"] <- "POP_WPP"
head(pop_sub1)

pop_1year <- merge(pop_1year, pop_1year_agg)
head(pop_1year)

pop_1year <- merge(pop_1year, pop_sub1)
head(pop_1year)

pop_1year$POP_SCALED <-
  pop_1year$POP * (pop_1year$POP_WPP / pop_1year$POP_IHME)
head(pop_1year)

pop_1year$POP_IHME <- NULL
pop_1year$POP_WPP <- NULL


###
### SAVE DATA
###

# countries <- FERG2:::countries
# pop <- FERG2:::pop
# life_exp <- FERG2:::life_exp
# life_birth <- FERG2:::life_birth
# pop_1year <- FERG2:::pop_1year
# pop_IHME <- FERG2:::pop_IHME

save(
  countries, 
  pop, pop_1year, pop_IHME,
  life_exp, life_birth,
  map1, map2, map3,
  file = "../R/sysdata.rda")
