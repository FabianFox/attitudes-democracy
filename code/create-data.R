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
ib22_democ.df <- ib22.df %>%
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
  select(a_recno, a_datum, migra, 
         weight = weight_dg2,
         # Democracy items
         dgg, dwf, dgb, drm, dpu, dme, dra, dmp, dwb, dop, dmk, drp, dlp, dmf,
         # Individual variables
         age, age_cat, gender, educ, income, lang_skill = pms2,
         religion = d16,
         religion_str = d17,
         discrimination = dis2,
         # Region
         state, region,
         # Migration
         mig_gen = geb,
         wandjahr,
         ger_citizen,
         d1_1, d1_2, d1_3, d1_other, gebland, s1_sonst,
         # Transnational contacts
         fh, fhk, fa, fak, fa2_1, fa2_2, fa2_other) %>%
  mutate(
    gebland_chr = as_character(gebland),
    gebland_chr = if_else(gebland_chr == "Anderes Land eingeben", s1_sonst, gebland_chr),
    gebland_chr = str_squish(gebland_chr),
    across(c(dgg, dwf, dgb, drm, dpu, dme, dra, dmp, dwb, dop, dmk, drp, 
             dlp, dmf, lang_skill, wandjahr, religion, religion_str, discrimination,
             fh, fhk, fa, fak, wandjahr), 
           ~replace(., . %in% c(97, 98, 99997, 99998), NA_real_)),
    # Dichotomize democ items
    across(c(dgg, dwf, dgb, drm, dpu, dme, dra, dmp, dwb, dop, dmk, drp, 
             dlp, dmf), ~case_when(
               .x %in% c(0, 1) ~ 0,
               .x %in% c(2, 3) ~ 1,
               TRUE ~ NA_real_),
           .names = "{.col}_bin"),
    across(c("fh", "fa"), ~if_else(.x == 2, 0, .x)),
    timedest = year(ymd(a_datum)) - wandjahr,
    age_mig = wandjahr - (year(ymd(a_datum)) - age))

## Add V-Dem ----
# Download V-Dem
vdem.df <- vdem %>%
  tibble() %>%
  select(country_text_id, year, v2x_polyarchy, v2x_libdem, v2x_partipdem,
         v2xed_ed_dmcon, v2xed_ed_ptcon, v2xed_ptcon) 

# Add democracy index in country-of-origin at year of immigration to respondents (only first generation)
# (1) country-of-origin string to iso3c
ib22_democ.df <- ib22_democ.df %>%
  mutate(iso3c = countrycode(gebland_chr, "country.name.de", "iso3c",
                             custom_match = c("Aegypten" = "EGY", "Kosovo" = "XKX", 
                                              "Moldawien" = "MDA", "Oesterreich" = "AUT", 
                                              "Rumaenien" = "ROU", "Simbawe" = "ZWE", 
                                              "Suedafrika" = "ZAF", "Tschetschenien" = "RUS",
                                              "Tuerkei" = "TUR")))

# Join
# VDem at formative year (age: 14 years, cutoff: age 16)
ib22_f14year.df <- ib22_democ.df %>%
  mutate(
    # Year when individual turned 14
    formative_year = year(ymd(a_datum)) - age + 14,
    # Migration before or after turning 16
    formative_before_mig = case_when(
      formative_year + 2 - wandjahr > 0 ~ "no",   # migrated before turning 16 --> exclude
      formative_year + 2 - wandjahr <= 0 ~ "yes", # migrated after turning 16 --> include
      TRUE ~ NA_character_),
    # Residence year in country of origin since turning 14
    timeorig = (formative_year - wandjahr) * -1,
    iso3c = case_when(
      # former USSR 
      iso3c == "ARM" & between(formative_year, 1922, 1989) ~ "RUS", # pre 1918 Ottoman Empire but no cases in IB
      iso3c == "AZE" & between(formative_year, 1900, 1989) ~ "RUS",
      iso3c == "GEO" & between(formative_year, 1900, 1989) ~ "RUS",
      iso3c == "KAZ" & between(formative_year, 1900, 1990) ~ "RUS",
      iso3c == "KGZ" & between(formative_year, 1900, 1989) ~ "RUS",
      iso3c == "TJK" & between(formative_year, 1900, 1989) ~ "RUS",
      iso3c == "TKM" & between(formative_year, 1900, 1989) ~ "RUS",
      iso3c == "UZB" & between(formative_year, 1900, 1989) ~ "RUS",
      # Bulgaria, Romania, Czechia, Hungary, Poland are coded throughout USSR period
      iso3c == "BLR" & between(formative_year, 1921, 1989) ~ "RUS",
      iso3c == "EST" & between(formative_year, 1940, 1989) ~ "RUS",
      iso3c == "LVA" & between(formative_year, 1940, 1989) ~ "RUS",
      iso3c == "LTU" & between(formative_year, 1940, 1989) ~ "RUS",
      iso3c == "MDA" & between(formative_year, 1940, 1989) ~ "RUS",
      iso3c == "UKR" & between(formative_year, 1941, 1989) ~ "RUS",
      # former Yugoslavia
      iso3c == "BIH" & between(formative_year, 1945, 1991) ~ "SRB",
      iso3c == "XKX" & between(formative_year, 1944, 1998) ~ "SRB",
      iso3c == "MKD" & between(formative_year, 1912, 1990) ~ "SRB",
      iso3c == "MNE" & between(formative_year, 1919, 1990) ~ "SRB",
      iso3c == "HRV" & between(formative_year, 1945, 1990) ~ "SRB",
      iso3c == "SVN" & between(formative_year, 1945, 1988) ~ "SRB",
      .default = iso3c),
    iso3c = case_when(
      formative_before_mig == "no" ~ "DEU",
      formative_before_mig == "yes" ~ iso3c,
      TRUE ~ iso3c)) %>%
  filter(iso3c != "DEU",
         formative_before_mig != "no") %>%
  left_join(y = vdem.df, by = c("formative_year" = "year", "iso3c" = "country_text_id"))

# VDem at formative year (age: 14 years, cutoff: age 17)
ib22_f17year.df <- ib22_democ.df %>%
  mutate(
    # Year when individual turned 14
    formative_year = year(ymd(a_datum)) - age + 14,
    # Migration before or after turning 17
    formative_before_mig = case_when(
      formative_year + 3 - wandjahr > 0 ~ "no",   # migrated before turning 17 --> exclude
      formative_year + 3 - wandjahr <= 0 ~ "yes", # migrated after turning 17 --> include
      TRUE ~ NA_character_),
    # Residence year in country of origin since turning 14
    timeorig = (formative_year - wandjahr) * -1,
    iso3c = case_when(
      # former USSR 
      iso3c == "ARM" & between(formative_year, 1922, 1989) ~ "RUS", # pre 1918 Ottoman Empire but no cases in IB
      iso3c == "AZE" & between(formative_year, 1900, 1989) ~ "RUS",
      iso3c == "GEO" & between(formative_year, 1900, 1989) ~ "RUS",
      iso3c == "KAZ" & between(formative_year, 1900, 1990) ~ "RUS",
      iso3c == "KGZ" & between(formative_year, 1900, 1989) ~ "RUS",
      iso3c == "TJK" & between(formative_year, 1900, 1989) ~ "RUS",
      iso3c == "TKM" & between(formative_year, 1900, 1989) ~ "RUS",
      iso3c == "UZB" & between(formative_year, 1900, 1989) ~ "RUS",
      # Bulgaria, Romania, Czechia, Hungary, Poland are coded throughout USSR period
      iso3c == "BLR" & between(formative_year, 1921, 1989) ~ "RUS",
      iso3c == "EST" & between(formative_year, 1940, 1989) ~ "RUS",
      iso3c == "LVA" & between(formative_year, 1940, 1989) ~ "RUS",
      iso3c == "LTU" & between(formative_year, 1940, 1989) ~ "RUS",
      iso3c == "MDA" & between(formative_year, 1940, 1989) ~ "RUS",
      iso3c == "UKR" & between(formative_year, 1941, 1989) ~ "RUS",
      # former Yugoslavia
      iso3c == "BIH" & between(formative_year, 1945, 1991) ~ "SRB",
      iso3c == "XKX" & between(formative_year, 1944, 1998) ~ "SRB",
      iso3c == "MKD" & between(formative_year, 1912, 1990) ~ "SRB",
      iso3c == "MNE" & between(formative_year, 1919, 1990) ~ "SRB",
      iso3c == "HRV" & between(formative_year, 1945, 1990) ~ "SRB",
      iso3c == "SVN" & between(formative_year, 1945, 1988) ~ "SRB",
      .default = iso3c),
    iso3c = case_when(
      formative_before_mig == "no" ~ "DEU",
      formative_before_mig == "yes" ~ iso3c,
      TRUE ~ iso3c)) %>%
  filter(iso3c != "DEU",
         formative_before_mig != "no") %>%
  left_join(y = vdem.df, by = c("formative_year" = "year", "iso3c" = "country_text_id"))

## Export ----
# All respondents
export(ib22_democ.df, here("data", "ib22_democ.rds"))

# VDem by formative years (cutoff: 16)
export(ib22_f14year.df, here("data", "ib22_f14year.rds"))

# VDem by formative years (cutoff: 17)
export(ib22_f17year.df, here("data", "ib22_f17year.rds"))

