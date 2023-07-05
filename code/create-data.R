# Create data ----

## Setup ----
# Load/install pkgs
# ------------------------------------------------------------------------------------------------ #
if(!require("xfun")) install.packages("xfun")
xfun::pkg_attach2("tidyverse", "rio", "sjlabelled", "countrycode",
                  "conflicted", "lubridate", "here", "vdemdata")

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# Load data
ib22.df <- tibble(import("/Users/fguelzau/Documents/project/svr/data/Integrationsbarometer/IB2022/IB_22_Final_220823.dta", 
                         encoding = "latin1"))

## Data wrangling ----
# Select variables
ib22_pol.df <- ib22.df %>%
  mutate(
    age = alter,
    age_cat = case_when(
      between(age, 15, 24) ~ "15-24 Jahre",
      between(age, 25, 34) ~ "25-34 Jahre",
      between(age, 35, 44) ~ "35-44 Jahre",
      between(age, 45, 54) ~ "45-54 Jahre",
      between(age, 55, 64) ~ "55-64 Jahre",
      age >= 65 ~ "über 65 Jahre",
      TRUE ~ NA_character_),
    gender = geschlecht - 1,
    educ = factor(bildung,
                  levels = c(1:4), 
                  labels = c("niedrig", "mittel", "hoch", "Schüler")),
    income = if_else(d20 %in% c(97, 98), NA_real_, d20),
    stay_length = year(ymd(a_datum)) - wandjahr,
    ger_citizen = case_when(
      d1_1 == 1 | d1_2 == 1 | d1_3 == 1 ~ 1,
      TRUE ~ 0),
    bundesland = as_character(bundesland),
    bundesland = if_else(bundesland %in% c("Don t know", 
                                           "Refused",
                                           "Weiß nicht", 
                                           "Verweigert"),
                         NA_character_, bundesland),
    state = case_when(
      bundesland %in% c("Baden-Württemberg", "B-Wü", "BW") ~ "Baden-Württemberg",
      bundesland %in% c("Bayern", "By", "BY") ~ "Bayern",
      bundesland %in% c("Berlin", "B", "BE") ~ "Berlin",
      bundesland %in% c("Brandenburg", "BB") ~ "Brandenburg",
      bundesland %in% c("Bremen", "HB") ~ "Bremen",
      bundesland %in% c("Hamburg", "HH") ~ "Hamburg",
      bundesland %in% c("Hessen", "H", "HE") ~ "Hessen",
      bundesland %in% c("Mecklenburg-Vorpommern", "M-V", "MV") ~ "Mecklenburg-Vorpommern",
      bundesland %in% c("Niedersachsen", "N", "NI") ~ "Niedersachsen",
      bundesland %in% c("Nordrhein-Westfalen", "NRW", "NW") ~ "Nordrhein-Westfalen",
      bundesland %in% c("Rheinland-Pfalz", "R-P", "RP") ~ "Rheinland-Pfalz",
      bundesland %in% c("Saarland", "Sa", "SL") ~ "Saarland",
      bundesland %in% c("Sachsen", "Ss", "SN") ~ "Sachsen",
      bundesland %in% c("Sachsen-Anhalt", "S-A", "ST") ~ "Sachsen-Anhalt",
      bundesland %in% c("Schleswig-Holstein", "S-H", "SH") ~ "Schleswig-Holstein",
      bundesland %in% c("Thüringen", "Thü", "TH") ~ "Thüringen",
      TRUE ~ NA_character_),
    region = case_when(
      state %in% c("Schleswig-Holstein", "Niedersachsen", "Nordrhein-Westfalen",
                   "Hessen", "Rheinland-Pfalz", "Baden-Württemberg", "Saarland",
                   "Bayern") ~ "Westdeutschland",
      state %in% c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen-Anhalt",
                   "Sachsen", "Thüringen") ~ "Ostdeutschland",
      state %in% c("Hamburg", "Berlin", "Bremen") ~ "Stadtstaaten",
      TRUE ~ NA_character_)) %>%
  select(a_recno, migra, 
         weight = weight_dg2,
         # Democracy items
         dgg, dwf, dgb, drm, dpu, dme, dra, dmp, dwb, dop, dmk, drp, dlp, dmf,
         age, age_cat, gender, educ, income, lang_skill = pms2,
         religion = d16,
         religion_str = d17,
         state, region,
         mig_gen = geb,
         stay_length, wandjahr,
         ger_citizen,
         d1_1, d1_2, d1_3, d1_other, gebland, s1_sonst) %>%
  mutate(
    gebland_chr = as_character(gebland),
    gebland_chr = if_else(gebland_chr == "Anderes Land eingeben", s1_sonst, gebland_chr),
    gebland_chr = str_squish(gebland_chr),
    across(c(dgg, dwf, dgb, drm, dpu, dme, dra, dmp, dwb, dop, dmk, drp, 
             dlp, dmf, lang_skill, stay_length, wandjahr, religion, religion_str), 
           ~replace(., . %in% c(97, 98, 99997, 99998), NA_real_)),
    # Dichotomize democ items
    across(c(dgg, dwf, dgb, drm, dpu, dme, dra, dmp, dwb, dop, dmk, drp, 
             dlp, dmf), ~case_when(
               .x %in% c(0, 1) ~ 0,
               .x %in% c(2, 3) ~ 1,
               TRUE ~ NA_real_),
           .names = "{.col}_bin"))

## Add V-Dem ----
# Download V-Dem
vdem.df <- vdem %>%
  tibble() %>%
  select(country_text_id, year, v2x_polyarchy) # Example

# Add democracy index in country-of-origin at year of immigration to respondents (only first generation)
# (1) country-of-origin string to iso3c
ib22_pol.df <- ib22_pol.df %>%
  mutate(iso3c = countrycode(gebland_chr, "country.name.de", "iso3c",
                             custom_match = c("Aegypten" = "EGY", "Kosovo" = "XKX", 
                                              "Moldawien" = "MDA", "Oesterreich" = "AUT", 
                                              "Rumaenien" = "ROU", "Simbawe" = "ZWE", 
                                              "Suedafrika" = "ZAF", "Tschetschenien" = "RUS",
                                              "Tuerkei" = "TUR")))

# IB uses current sovereignty as country-of-birth (i.e. Georgia not USSR-Russia)
# Replace with former empire to join vdem-score
ib22_pol.df <- ib22_pol.df %>%
  mutate(iso3c = case_when(
    # former USSR 
    iso3c == "ARM" & between(wandjahr, 1922, 1989) ~ "RUS", # pre 1918 Ottoman Empire but no cases in IB
    iso3c == "AZE" & between(wandjahr, 1900, 1989) ~ "RUS",
    iso3c == "GEO" & between(wandjahr, 1900, 1989) ~ "RUS",
    iso3c == "KAZ" & between(wandjahr, 1900, 1990) ~ "RUS",
    iso3c == "KGZ" & between(wandjahr, 1900, 1989) ~ "RUS",
    iso3c == "TJK" & between(wandjahr, 1900, 1989) ~ "RUS",
    iso3c == "TKM" & between(wandjahr, 1900, 1989) ~ "RUS",
    iso3c == "UZB" & between(wandjahr, 1900, 1989) ~ "RUS",
    # Bulgaria, Romania, Czechia, Hungary, Poland are coded throughout USSR period
    iso3c == "BLR" & between(wandjahr, 1921, 1989) ~ "RUS",
    iso3c == "EST" & between(wandjahr, 1940, 1989) ~ "RUS",
    iso3c == "LVA" & between(wandjahr, 1940, 1989) ~ "RUS",
    iso3c == "LTU" & between(wandjahr, 1940, 1989) ~ "RUS",
    iso3c == "MDA" & between(wandjahr, 1940, 1989) ~ "RUS",
    iso3c == "UKR" & between(wandjahr, 1941, 1989) ~ "RUS",
    # former Yugoslavia
    iso3c == "BIH" & between(wandjahr, 1945, 1991) ~ "SRB",
    iso3c == "XKX" & between(wandjahr, 1944, 1998) ~ "SRB",
    iso3c == "MKD" & between(wandjahr, 1912, 1990) ~ "SRB",
    iso3c == "MNE" & between(wandjahr, 1919, 1990) ~ "SRB",
    iso3c == "HRV" & between(wandjahr, 1945, 1990) ~ "SRB",
    iso3c == "SVN" & between(wandjahr, 1945, 1988) ~ "SRB",
    
    .default = iso3c
  ))

# Join
ib22_fb.df <- ib22_pol.df %>%
  filter(iso3c != "DEU") %>%
  left_join(y = vdem.df, by = c("wandjahr" = "year", "iso3c" = "country_text_id"))

## Export ----
export(ib22_fb.df, here("data", "ib22_fbpol.rds"))
export(ib22_pol.df, here("data", "ib22_pol.rds"))
