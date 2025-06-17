# Analysis (ESS data) ----

# Setup ----
# Load/install pkgs
# ------------------------------------------------------------------------------------------------ #
if(!require("xfun")) install.packages("xfun")
xfun::pkg_attach2("tidyverse", "rio", "hrbrthemes", "fixest", "modelsummary", "marginaleffects", "lme4",
                  "conflicted", "lubridate", "here", "Cairo", "Hmisc", "gt", "gtExtras", "patchwork",
                  "vdemdata")

# Declare package preference
conflicts_prefer(dplyr::filter(),
                 dplyr::select(),
                 dplyr::summarize(),
                 tidyr::expand())

# Load fonts
extrafont::loadfonts()

# Functions
# Weighted 95%-conf
# from: https://stackoverflow.com/questions/37973240/how-to-create-a-confidence-interval-for-a-weighted-average-of-areas-under-the-ro
weighted.ttest.ci <- function(x, weights, conf.level = 0.95) {
  require(Hmisc)
  nx <- length(x)
  df <- nx - 1
  vx <- wtd.var(x, weights, normwt = FALSE) ## From Hmisc
  mx <- weighted.mean(x, weights, na.rm = TRUE)
  stderr <- sqrt(vx/nx)
  tstat <- mx/stderr ## not mx - mu
  alpha <- 1 - conf.level
  cint <- qt(1 - alpha/2, df)
  cint <- tstat + c(-cint, cint)
  cint * stderr
}

# Load data ----
# ------------------------------------------------------------------------------------------------ #
# All respondents
ess_democ.orig <- tibble(import(here("data", "mergeddataset_V_ESS_14_16_nativesfirstgen.dta"))) 

# Add quantiles of Vdem
ess_democ.df <- ess_democ.orig %>%
  mutate(across(matches(c("v2x_polyarchy_4nat", "v2xed_ed_dmcon_4nat")),
                ~factor(tolower(sjlabelled::as_character(.)),
                levels = c("non-migrant", "low", "rather low", "rather high", "high"))))

# Description ----
## Summary statistics ----
### Boxplot (App. Fig. 1b) ----
demo_boxplot_ess.fig <- ess_democ.df %>%
  mutate(first_gen = factor(first_gen, 
                            levels = c(1, 0), 
                            labels = c("First-generation", "Non-migrant"))) %>%
  ggplot(aes(x = first_gen, y = demo1, weight = anweight)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "", y = "Democratic Values", title = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"))

### DV: Single items (App. Tab. 2b) ----
items_democ_ess.gt <- ess_democ.df %>%
  group_by(first_gen = factor(first_gen, 
                              levels = c(1, 0), 
                              labels = c("First-generation", "Non-migrant")), .add = TRUE) %>%
  reframe(across(c(demo1, fairelc, dfprtal, medcrgv, rghmgpr, cttresa),
                 list(mean = ~wtd.mean(.x, weights = anweight, na.rm = TRUE),
                      sd = ~sqrt(wtd.var(.x, weights = anweight, na.rm = TRUE)),
                      min = ~min(.x, na.rm = TRUE),
                      max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = 2:25,
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(variable = case_match(variable,
                               "demo1" ~ "Democratic values",
                               "fairelc" ~ "Fair elections",
                               "dfprtal" ~ "Party competition",
                               "medcrgv" ~ "Free media",
                               "rghmgpr" ~ "Protected minorities",
                               "cttresa" ~ "Equality courts")) %>%
  filter(variable != "Democratic values") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  gt() %>%
  fmt_number(decimals = 2, drop_trailing_zeros = TRUE)

### Frequency plots ----
demo_items_ess_freqplot.fig <- ess_democ.df %>%
  mutate(first_gen = factor(first_gen, 
                            levels = c(1, 0), 
                            labels = c("First-generation", "Non-migrant"))) %>%
  select(first_gen, fairelc, dfprtal, medcrgv, rghmgpr, cttresa, anweight) %>%
  na.omit() %>%
  pivot_longer(cols = c(fairelc, dfprtal, medcrgv, rghmgpr, cttresa), 
               names_to = "item", values_to = "response") %>%
  mutate(item = case_match(item,
                           "fairelc" ~ "Fair elections",
                           "dfprtal" ~ "Party competition",
                           "medcrgv" ~ "Free media",
                           "rghmgpr" ~ "Protected minorities",
                           "cttresa" ~ "Equality courts")) %>%
  count(response, first_gen, item, wt = anweight) %>%
  mutate(p = n / sum(n), .by = c(item, first_gen),
         response = case_match(response, 
                               0 ~ "0 - Not at all important",
                               1 ~ "1", 2 ~ "2", 3 ~ "3", 4 ~ "4", 5 ~ "5", 
                               6 ~ "6", 7 ~ "7", 8 ~ "8", 9 ~ "9",
                               10 ~ "10 - Extremely important"),
         response = factor(response, 
                           levels = c("10 - Extremely important",
                                      seq(9, 1, by = -1),
                                      "0 - Not at all important"))) %>%
  ggplot(aes(x = p, y = first_gen, fill = response)) +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_text(aes(
    label = ifelse(p >= .05, 
                   str_replace(as.character(round(p * 100, digit = 1))
                               , "[.]", ","), "")), 
    family = "Roboto Condensed", size = 6, colour = "gold",
    position = position_stack(vjust = .5)) +
  facet_wrap(~item) +
  scico::scale_fill_scico_d(palette = "cork",
                            guide = guide_legend(reverse = TRUE, nrow = 1)) +
  scale_x_percent() +
  labs(x = "", y = "", fill = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = "bottom", 
        legend.text = element_text(size = 12),
        legend.justification = "left")

### by VDEM ----
#### Plots ----
##### Composite score ----
# Electoral democracy index (v2x_polyarchy)
poly_ess.fig <- ess_democ.df %>%
  filter(!is.na(demo1), !is.na(v2x_polyarchy_4nat)) %>%
  ggplot(aes(x = fct_rev(v2x_polyarchy_4nat), y = demo1, weight = anweight)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  labs(x = "", y = "Democratic Values", title = "Electoral Democracy Index") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14))

# Democratic indoctrination content in education (v2xed_ed_dmcon)
dmcon_ess.fig <- ess_democ.df %>%
  filter(!is.na(demo1), !is.na(v2xed_ed_dmcon_4nat)) %>%
  ggplot(aes(x = fct_rev(v2xed_ed_dmcon_4nat), y = demo1, weight = anweight)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  labs(x = "", y = "Democratic Values", title = "Democratic Indoctrination Index") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14))

# Combine (Figure 2)
democ_vdem_ess.fig <- poly_ess.fig + dmcon_ess.fig

##### Single items ----
# Appendix: Figure 3c and 3d
# Electoral democracy index (v2x_polyarchy)
poly_item.fig <- ess_democ.df %>%
  filter(if_all(c(fairelc, dfprtal, medcrgv, rghmgpr, cttresa, v2x_polyarchy_4nat, anweight), ~!is.na(.x))) %>%
  pivot_longer(cols = c(fairelc, dfprtal, medcrgv, rghmgpr, cttresa), 
               names_to = "item", values_to = "response") %>%
  mutate(item = case_match(item,
                           "fairelc" ~ "Fair elections",
                           "dfprtal" ~ "Party competition",
                           "medcrgv" ~ "Free media",
                           "rghmgpr" ~ "Protected minorities",
                           "cttresa" ~ "Equality courts")) %>%
  count(response, v2x_polyarchy_4nat, item, wt = anweight) %>%
  mutate(p = n / sum(n), .by = c(item, v2x_polyarchy_4nat),
         response = case_match(response, 
                               0 ~ "0 - Not at all important",
                               1 ~ "1", 2 ~ "2", 3 ~ "3", 4 ~ "4", 5 ~ "5", 
                               6 ~ "6", 7 ~ "7", 8 ~ "8", 9 ~ "9",
                               10 ~ "10 - Extremely important"),
         response = factor(response, 
                           levels = c("10 - Extremely important",
                                      seq(9, 1, by = -1),
                                      "0 - Not at all important"))) %>%
  ggplot(aes(x = p, y = fct_rev(v2x_polyarchy_4nat), fill = response)) +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_text(aes(
    label = ifelse(p >= .05, 
                   str_replace(as.character(round(p * 100, digit = 1))
                               , "[.]", ","), "")), 
    family = "Roboto Condensed", size = 6, colour = "gold",
    position = position_stack(vjust = .5)) +
  facet_wrap(~item) +
  scico::scale_fill_scico_d(palette = "cork",
                            guide = guide_legend(reverse = TRUE, nrow = 1)) +
  scale_x_percent() +
  labs(x = "", y = "", fill = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = "bottom", 
        legend.text = element_text(size = 12),
        legend.justification = "left")

# Democratic indoctrination content in education (v2xed_ed_dmcon)
dmcon_item.fig <- ess_democ.df %>%
  filter(if_all(c(fairelc, dfprtal, medcrgv, rghmgpr, cttresa, v2xed_ed_dmcon_4nat, anweight), ~!is.na(.x))) %>%
  pivot_longer(cols = c(fairelc, dfprtal, medcrgv, rghmgpr, cttresa), 
               names_to = "item", values_to = "response") %>%
  mutate(item = case_match(item,
                           "fairelc" ~ "Fair elections",
                           "dfprtal" ~ "Party competition",
                           "medcrgv" ~ "Free media",
                           "rghmgpr" ~ "Protected minorities",
                           "cttresa" ~ "Equality courts")) %>%
  count(response, v2xed_ed_dmcon_4nat, item, wt = anweight) %>%
  mutate(p = n / sum(n), .by = c(item, v2xed_ed_dmcon_4nat),
         response = case_match(response, 
                               0 ~ "0 - Not at all important",
                               1 ~ "1", 2 ~ "2", 3 ~ "3", 4 ~ "4", 5 ~ "5", 
                               6 ~ "6", 7 ~ "7", 8 ~ "8", 9 ~ "9",
                               10 ~ "10 - Extremely important"),
         response = factor(response, 
                           levels = c("10 - Extremely important",
                                      seq(9, 1, by = -1),
                                      "0 - Not at all important"))) %>%
  ggplot(aes(x = p, y = fct_rev(v2xed_ed_dmcon_4nat), fill = response)) +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_text(aes(
    label = ifelse(p >= .05, 
                   str_replace(as.character(round(p * 100, digit = 1))
                               , "[.]", ","), "")), 
    family = "Roboto Condensed", size = 6, colour = "gold",
    position = position_stack(vjust = .5)) +
  facet_wrap(~item) +
  scico::scale_fill_scico_d(palette = "cork",
                            guide = guide_legend(reverse = TRUE, nrow = 1)) +
  scale_x_percent() +
  labs(x = "", y = "", fill = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = "bottom", 
        legend.text = element_text(size = 12),
        legend.justification = "left")

### Tables ----
# Electoral democracy index (v2x_polyarchy) (App.: Tab. 3c)
items_democ_poly_ess.gt <- ess_democ.df %>%
  filter(first_gen == 1, 
         !is.na(v2x_polyarchy_4nat)) %>%
  group_by(v2x_polyarchy_4nat) %>%
  reframe(across(c(fairelc, dfprtal, medcrgv, rghmgpr, cttresa),
                 list(mean = ~wtd.mean(.x, weights = anweight, na.rm = TRUE),
                      sd = ~sqrt(wtd.var(.x, weights = anweight, na.rm = TRUE)),
                      min = ~min(.x, na.rm = TRUE),
                      max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = 2:21,
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(variable = case_match(variable,
                               "fairelc" ~ "Fair elections",
                               "dfprtal" ~ "Party competition",
                               "medcrgv" ~ "Free media",
                               "rghmgpr" ~ "Protected minorities",
                               "cttresa" ~ "Equality courts")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  gt() %>%
  fmt_number(decimals = 2, drop_trailing_zeros = TRUE)

# Democratic indoctrination content in education (v2xed_ed_dmcon) (App. Tab. 3d)
items_democ_ind_ess.gt <- ess_democ.df %>%
  filter(first_gen == 1, 
         !is.na(v2xed_ed_dmcon_4nat)) %>%
  group_by(v2xed_ed_dmcon_4nat) %>%
  reframe(across(c(fairelc, dfprtal, medcrgv, rghmgpr, cttresa),
                 list(mean = ~wtd.mean(.x, weights = anweight, na.rm = TRUE),
                      sd = ~sqrt(wtd.var(.x, weights = anweight, na.rm = TRUE)),
                      min = ~min(.x, na.rm = TRUE),
                      max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = 2:21,
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(variable = case_match(variable,
                               "fairelc" ~ "Fair elections",
                               "dfprtal" ~ "Party competition",
                               "medcrgv" ~ "Free media",
                               "rghmgpr" ~ "Protected minorities",
                               "cttresa" ~ "Equality courts")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  gt() %>%
  fmt_number(decimals = 2, drop_trailing_zeros = TRUE)

# Prepare ----
# Reduce data to individuals with migback and rename variables
ess_democ_mod.df <- ess_democ.df %>%
  filter(first_gen == 1) %>%
  select(gender = gndr, educ = edulvlb, discrimination = discri, religion_str = religiosity, 
         democ = demo1, iso3c = country_text_id, weight = anweight, v2x_polyarchy, v2x_libdem,
         v2x_partipdem, v2xed_ed_dmcon, v2xed_ed_ptcon, v2xed_ptcon, muslim, timedest, timeorig, 
         cntry, age = agea, year, idno, livecnta, mode, implvdm, stfdem, fairelcc) %>%
  mutate(
    muslim = factor(muslim, levels = c(0, 1), 
                    labels = c("Other", "Muslim")),
    discrimination = case_match(
      discrimination,
      0 ~ "no",
      1 ~ "yes",
      .default = NA_character_),
    discrimination = factor(discrimination, 
                            levels = c("no", "yes")),
    gender = gender - 1,
    gender = factor(gender, 
                    levels = c(0, 1), 
                    labels = c("Male", "Female")),
    educ = case_match(educ,
                      c(0, 113, 129, 212, 213, 221, 222, 223) ~ "ISCED 2 and lower",
                      c(229, 311, 312, 313, 321, 322, 323) ~ "ISCED 3",
                      c(412, 413, 421, 422, 423, 510, 520) ~ "ISCED 4 and 5A/B short",
                      c(610, 620, 710, 720, 800) ~ "ISCED 5 medium and higher",
                      .default = NA_character_),
    educ = factor(educ, levels = c("ISCED 2 and lower", "ISCED 3", "ISCED 4 and 5A/B short", 
                                   "ISCED 5 medium and higher")),
    timeorig = timeorig - 14, # Count from zero since age 14 (just like in IB analysis)
    mode = case_match(
      mode,
      1 ~ "Interview: F2F (CAPI)",
      2 ~ "Interview: Video",
      3 ~ "Self completion (CAWI)",
      4 ~ "Self completion (PAPI)",
      c(1, 2) ~ "Interview",
      .default = NA_character_))

## Overview of predictors ----
# Create extra row for number of clusters
n_cluster_coo <- ess_democ_mod.df %>% 
  count(iso3c) %>%
  summarise(Clusters = "Cluster (CoO)", n_total = str_c("Total: ", length(n)), mean = mean(n), `Std. Dev.` = sd(n))

# Create extra row for number of clusters
n_cluster_cod <- ess_democ_mod.df %>% 
  count(cntry) %>%
  summarise(Clusters = "Cluster (CoD)", n_total = str_c("Total: ", length(n)), mean = mean(n), `Std. Dev.` = sd(n))

n_cluster <- n_cluster_coo %>%
  bind_rows(n_cluster_cod)

# All predictors (App. Tab. 6)
ess_predictors.tbl <- ess_democ_mod.df %>%
  select(`Democratic values` = democ, `Residence period (CoO)` = timeorig, 
         `Residence period (CoD)` = timedest, `Electoral democracy` = v2x_polyarchy, 
         `Democratic indoctrination` = v2xed_ed_dmcon, `Gender` = gender, `Education` = educ, 
         `Discrimination` = discrimination, `Muslim` = muslim, `Religiosity` = religion_str) %>%
  labelled::remove_var_label() %>%
  modelsummary::datasummary_balance(~1, data = ., output = "gt", 
                                    add_rows = n_cluster) 

## Correlations of VDem ----
# Correlation between VDem measures
vdem_ess_cor.df <- ess_democ_mod.df %>%
  select(contains("v2x"), "weight") %>%
  collapse::pwcor(X = .[], 
                  use = "pairwise.complete.obs",
                  w = .$weight) %>%
  # remove weights
  .[-7, -7]

# Table of correlations in analysis (Sample: formative years)
vdem_ess_cor.tbl <- vdem_ess_cor.df %>%
  as.data.frame(row.names = c("v2x_polyarchy", "v2x_libdem", "v2x_partipdem", "v2xed_ed_dmcon",
                              "v2xed_ed_ptcon", "v2xed_ptcon")) %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = md("Correlation table")) %>%
  tab_source_note(source_note = md("**Source**: ESS10; weighted")) %>%
  fmt_number(decimals = 3)

## by VDEM and time ----
#### Timeorig ----
democ_vdem_ess_timeorig.df <- ess_democ.df %>%
  tibble() %>%
  filter(first_gen == 1) %>%
  mutate(
    timeorig = timeorig - 14, # Count from zero since age 14 (just like in IB analysis)
    timeorig_cut = cut(timeorig, 
                       breaks = c(seq(0, 30, by = 5), Inf),
                       labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30+"),
                       right = FALSE, include.lowest = TRUE))

#### Timedest ----
democ_vdem_ess_timedest.df <- ess_democ.df %>%
  tibble() %>%
  filter(first_gen == 1) %>%
  mutate(
    timeorig = timeorig - 14, # Count from zero since age 14 (just like in IB analysis))
    timedest_cut = cut(timedest, 
                       breaks = c(seq(0, 30, by = 5), Inf),
                       labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30+"),
                       right = FALSE, include.lowest = TRUE))

#### Compute ----
# Electoral democracy index (v2x_polyarchy)
democ_poly_timeorig_ess.df <- democ_vdem_ess_timeorig.df %>%
  filter(if_all(c(demo1, v2x_polyarchy_4nat, timeorig_cut, anweight), ~!is.na(.x))) %>%
  summarise(
    mean = wtd.mean(demo1, 
                    weights = anweight,
                    na.rm = TRUE),
    conf.low = weighted.ttest.ci(demo1, 
                                 weights = anweight, 
                                 conf.level = 0.95)[1],
    conf.high = weighted.ttest.ci(demo1, 
                                  weights = anweight, 
                                  conf.level = 0.95)[2],
    .by = c(v2x_polyarchy_4nat, timeorig_cut), 
    n = n()) %>%
  rename(vdem_qnt = v2x_polyarchy_4nat)

# Democratic indoctrination content in education (v2xed_ed_dmcon)
democ_indoc_timeorig_ess.df <- democ_vdem_ess_timeorig.df %>%
  filter(if_all(c(demo1, v2xed_ed_dmcon_4nat, timeorig_cut, anweight), ~!is.na(.x))) %>%
  summarise(
    mean = wtd.mean(demo1, 
                    weights = anweight,
                    na.rm = TRUE),
    conf.low = weighted.ttest.ci(demo1, 
                                 weights = anweight, 
                                 conf.level = 0.95)[1],
    conf.high = weighted.ttest.ci(demo1, 
                                  weights = anweight, 
                                  conf.level = 0.95)[2],
    .by = c(v2xed_ed_dmcon_4nat, timeorig_cut), 
    n = n()) %>%
  rename(vdem_qnt = v2xed_ed_dmcon_4nat)

# Join
democ_timeorig_ess.df <- democ_poly_timeorig_ess.df %>%
  bind_rows(democ_indoc_timeorig_ess.df, .id = "vdem") %>%
  mutate(vdem = if_else(vdem == 1, "Electoral Democracy", "Democratic Indoctrination"))

##### Plot ---- 
# Appendix: Figure 4b
democ_timeorig_ess.fig <- democ_timeorig_ess.df %>%
  ggplot(aes(x = timeorig_cut, 
             y = mean, ymin = conf.low, ymax = conf.high, 
             group = vdem, colour = vdem)) +
  geom_pointrange() +
  geom_line() +
  facet_wrap(~vdem_qnt) +
  labs(x = "Residence period in country of origin", y = "Democratic values", 
       title = "Democratic Values by V-Dem and Residence Period",
       colour = "V-Dem:") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"), 
        axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))

##### Subsample timeorig: all, timedest >= 5 ----
# Compute
# Electoral democracy index (v2x_polyarchy)
democ_poly_timeorig_timedest05_ess.df <- democ_vdem_ess_timeorig.df %>%
  filter(if_all(c(demo1, v2x_polyarchy_4nat, timeorig_cut, timedest, anweight), ~!is.na(.x)),
         timedest <= 5) %>%
  summarise(
    mean = wtd.mean(demo1, 
                    weights = anweight,
                    na.rm = TRUE),
    conf.low = weighted.ttest.ci(demo1, 
                                 weights = anweight, 
                                 conf.level = 0.95)[1],
    conf.high = weighted.ttest.ci(demo1, 
                                  weights = anweight, 
                                  conf.level = 0.95)[2],
    .by = c(v2x_polyarchy_4nat, timeorig_cut), 
    n = n()) %>%
  rename(vdem_qnt = v2x_polyarchy_4nat)

# Democratic indoctrination content in education (v2xed_ed_dmcon)
democ_indoc_timeorig_timedest05_ess.df <- democ_vdem_ess_timeorig.df %>%
  filter(if_all(c(demo1, v2xed_ed_dmcon_4nat, timeorig_cut, timedest, anweight), ~!is.na(.x)),
         timedest <= 5) %>%
  summarise(
    mean = wtd.mean(demo1, 
                    weights = anweight,
                    na.rm = TRUE),
    conf.low = weighted.ttest.ci(demo1, 
                                 weights = anweight, 
                                 conf.level = 0.95)[1],
    conf.high = weighted.ttest.ci(demo1, 
                                  weights = anweight, 
                                  conf.level = 0.95)[2],
    .by = c(v2xed_ed_dmcon_4nat, timeorig_cut), 
    n = n()) %>%
  rename(vdem_qnt = v2xed_ed_dmcon_4nat)

# Join
democ_vdem_timeorig_timedest05_ess.df <- democ_poly_timeorig_timedest05_ess.df %>%
  bind_rows(y = democ_indoc_timeorig_timedest05_ess.df, .id = "vdem") %>%
  mutate(vdem = if_else(vdem == 1, "Electoral Democracy", "Democratic Indoctrination"))

# Plot
democ_timeorig_timedest05_ess.fig <- democ_vdem_timeorig_timedest05_ess.df %>%
  ggplot(aes(x = timeorig_cut, 
             y = mean, ymin = conf.low, ymax = conf.high, 
             group = vdem, colour = vdem)) +
  geom_pointrange() +
  geom_line() +
  facet_wrap(~vdem_qnt) +
  labs(x = "Residence period in country of origin", y = "Democratic values", 
       title = "Democratic Values by VDem and Residence Period",
       subtitle = "Respondents with less than five years of residence in country of destination",
       colour = "VDem:") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"), 
        axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))

#### VDem over time and CoO ----
# Appendix: Figure 2b
# Electoral democracy index (v2x_polyarchy)
vdem_obs_poly_ess.fig <- ess_democ_mod.df %>%
  ggplot(aes(x = year, y = v2x_polyarchy, fill = iso3c)) +
  geom_line(data = . %>% filter(!iso3c %in% c("RUS", "AFG", "USA")),
            stat = "identity", alpha = 0.2, colour = "#00BFC4") +
  geom_line(data = . %>% filter(iso3c %in% c("RUS", "AFG", "USA")), colour = "black", linetype = "dashed") +
  geom_text(data = . %>% filter(iso3c %in% c("RUS", "AFG", "USA")) %>%
              slice_max(year, by = iso3c), aes(label = iso3c), nudge_x = 1.75, 
            family = "Roboto Condensed") +
  stat_summary(data = ~mutate(., obs = n(), .by = c(iso3c, year)) %>% 
                 filter(obs >= 1 & between(year, 1950, 2016)),
               aes(group = 1), fun = mean, geom = "line", color = "#F8766D", linewidth = 1) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 20), limits = c(1941, 2020)) +
  scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1)) +
  labs(title = "Electoral Democracy over time", 
       subtitle = "", 
       x = "", y = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14))

# Main countries of origin over time
main_by_period.df <- ess_democ_mod.df %>%
  mutate(period = cut(year, breaks = c(1937, seq(1940, 2020, by = 5)), 
                      include.lowest = TRUE, include.highest = TRUE)) %>%
  summarise(across(c(v2x_polyarchy, v2xed_ed_dmcon),
                   ~mean(.x, na.rm = T), .names = "{.col}_mean"), 
            .by = c(iso3c, period),
            n = n()) %>%
  slice_max(n = 10, order_by = n, by = period) %>%
  arrange(period, n)

# Democratic indoctrination content in education (v2xed_ed_dmcon)
vdem_obs_indoc_ess.fig <- ess_democ_mod.df %>%
  ggplot(aes(x = year, y = v2xed_ed_dmcon, fill = iso3c)) +
  geom_line(data = . %>% filter(!iso3c %in% c("RUS", "AFG", "USA")),
            stat = "identity", alpha = 0.2, colour = "#00BFC4") +
  geom_line(data = . %>% filter(iso3c %in% c("RUS", "AFG", "USA")), colour = "black", linetype = "dashed") +
  geom_text(data = . %>% filter(iso3c %in% c("RUS", "AFG", "USA")) %>%
              slice_max(year, by = iso3c), aes(label = iso3c), nudge_x = 1.75, 
            family = "Roboto Condensed") +
  stat_summary(data = ~mutate(., obs = n(), .by = c(iso3c, year)) %>% 
                 filter(obs >= 1 & between(year, 1950, 2016)),
               aes(group = 1), fun = mean, geom = "line", color = "#F8766D", linewidth = 1) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 20), limits = c(1940, 2020)) +
  scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1)) +
  labs(title = "Democratic indoctrination over time", 
       subtitle = "", 
       x = "", y = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14))

# Get Map
wmap_ess.sf <- giscoR::gisco_get_countries() %>%
  mutate(
    covered_in_ess = if_else(
      ISO3_CODE %in% ess_democ_mod.df$iso3c,
      "included", "not included"))

# Missing matches
unique(ess_democ_mod.df$iso3c[which(!ess_democ_mod.df$iso3c %in% wmap_ess.sf$ISO3_CODE)])

# Create map of included countries
countries_covered_ess.map <- ggplot() +
  geom_sf(data = wmap_ess.sf[!wmap_ess.sf$CNTR_NAME == "Antarctica",], aes(fill = covered_in_ess)) +
  scale_fill_manual(values = c("included" = "#00BFC4", "not included" = "#F8766D")) +
  coord_sf(crs = sf::st_crs("EPSG:3857")) +
  labs(title = "Covered countries of origin", fill = "") +
  cowplot::theme_map() +
  theme(legend.position = "bottom")

# Patchwork to combine
vdem_obs_dev_ess.fig <- vdem_obs_poly_ess.fig | vdem_obs_indoc_ess.fig | countries_covered_ess.map

# Add Panel tags
vdem_obs_dev_ess.fig <- vdem_obs_dev_ess.fig + plot_annotation(tag_levels = 'A')

# Hierarchical modeling ----
ess_democ_mod.df <- ess_democ_mod.df %>%
  # Centering within cluster (CWC)
  mutate(across(c(timedest, timeorig, v2x_polyarchy, v2xed_ed_dmcon),
                ~ .x - mean(.x, na.rm = TRUE),
                .names = "{.col}_cwc"),
         .by = iso3c) %>%
  # Grand mean centering (CGM)
  mutate(across(c(timedest, timeorig, v2x_polyarchy, v2xed_ed_dmcon),
                ~ .x - mean(.x, na.rm = TRUE),
                .names = "{.col}_cgm")) %>%
  # Cluster means
  mutate(across(c(v2x_polyarchy, v2xed_ed_dmcon, timeorig, timedest),
                                    ~ mean(.x, na.rm = TRUE),
                                    .names = "{.col}_mean"),
                             .by = iso3c) %>%
  # Custom rescaling of timedest and timeorig
  mutate(across(c(timeorig, timedest),
                ~ .x / 10,
                .names = "{.col}_scl"),
         .by = iso3c)

# Mean of v2x_polyarchy / v2x_ed_dmcon
ess_democ_mod.df %>%
  summarise(across(c(v2x_polyarchy, v2xed_ed_dmcon), ~mean(.x, na.rm = TRUE)))

## Run models ----
model.df <- tibble(
  dv = "democ",
  type = rep(
    c("Unconditional",
      "Base", 
      "Interactions",
      "Random slope"), 2),
  iv = c(
    "1 + (1 | iso3c) + (1 | cntry)", 
    "gender + educ + timedest_scl + timeorig_scl + muslim + religion_str + v2x_polyarchy_cgm + discrimination + (1 | iso3c) + (1 | cntry)",
    "gender + educ + muslim + religion_str +  + discrimination + timedest_scl*v2x_polyarchy_cgm + timeorig_scl*v2x_polyarchy_cgm + (1 | iso3c) + (1 | cntry)",
    "gender + educ + muslim + religion_str + discrimination + timedest_scl*v2x_polyarchy_cgm + timeorig_scl*v2x_polyarchy_cgm + (1 + v2x_polyarchy_cgm + timeorig_scl | iso3c) + (1 | cntry)",
    "1 + (1 | iso3c) + (1 | cntry)", 
    "gender + educ + timedest_scl + timeorig_scl + muslim + religion_str + v2xed_ed_dmcon_cgm + discrimination + (1 | iso3c) + (1 | cntry)",
    "gender + educ + muslim + religion_str + discrimination + timedest_scl*v2xed_ed_dmcon_cgm + timeorig_scl*v2xed_ed_dmcon_cgm + (1 | iso3c) + (1 | cntry)",
    "gender + educ + muslim + religion_str + discrimination + timedest_scl*v2xed_ed_dmcon_cgm + timeorig_scl*v2xed_ed_dmcon_cgm + (1 + timeorig_scl + v2xed_ed_dmcon_cgm | iso3c) + (1 | cntry)"),
  main_iv = c(
    rep("vdem-poly", 4),
    rep("vdem-ind", 4)),
  data = "ess_democ_mod.df")

# Run models
# VDem at year of immigration
model.df <- model.df %>%
  mutate(model = pmap(list(dv, iv, data), ~lmer(
    str_c(..1, " ~ ", ..2),
    REML = TRUE,
    control = lmerControl(
      optimizer = 'optimx', optCtrl = list(method = 'nlminb')),
    weights = weight,
    data = eval(rlang::parse_expr(..3)))))

# Name list column
names(model.df$model) <- model.df$type

### Model output ----
# Using 'modelsummary' (Table 2)
mlm.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
                        models = model.df %>% pull(model),
                        output = "gt",
                        stars = TRUE,
                        estimate = "{estimate}{stars}",
                        statistic = "({std.error})",
                        coef_map = c("(Intercept)" = "Intercept",
                                     "genderFemale" = "Gender: Female",
                                     "educISCED 3" = "Education: ISCED 3\n(Ref.: ISCED 2 and lower)",
                                     "educISCED 4 and 5A/B short" = "Educ.: ISCED 4 and 5A/B short",
                                     "educISCED 5 medium and higher" = "Educ.: ISCED 5 medium and higher",
                                     "timedest_scl" = "Period of residence (Country of destination)",
                                     "timeorig_scl" = "Period of residence (Country of origin)",
                                     "muslimMuslim" = "Muslim",
                                     "religion_str" = "Religiosity",
                                     "discriminationyes" = "Discrimination: Yes\n(Ref.: No)",
                                     "v2xed_ed_dmcon_cgm" = "Political socialization (V-Dem) [CGM]",
                                     "v2x_polyarchy_cgm" = "Political socialization (V-Dem) [CGM]",
                                     "timedest_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoD) × V-Dem [CGM]",
                                     "v2xed_ed_dmcon_cgm:timedest_scl" = "Period of residence (CoD) × V-Dem [CGM]",
                                     "timeorig_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "v2xed_ed_dmcon_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "timedest_scl:v2x_polyarchy_cgm" = "Period of residence (CoD) × V-Dem [CGM]",
                                     "v2x_polyarchy_cgm:timedest_scl" = "Period of residence (CoD) × V-Dem [CGM]",
                                     "timeorig_scl:v2x_polyarchy_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "v2x_polyarchy_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "SD (Intercept iso3c)" = "SD (Intercept: CoO)",
                                     "SD (Intercept cntry)" = "SD (Intercept: CoD)",
                                     "SD (v2x_polyarchy_cgm iso3c)" = "SD (V-Dem by CoO)",
                                     "SD (v2x_polyarchy_cgm cntry)" = "SD (V-Dem by CoD)",
                                     "SD (v2xed_ed_dmcon_cgm iso3c)" = "SD (V-Dem by CoO)",
                                     "SD (v2xed_ed_dmcon_cgm cntry)" = "SD (V-Dem by CoD)",
                                     "SD (Observations)" = "SD (Observations)"),
                        gof_map = tribble(
                          ~raw, ~clean, ~fmt,
                          "nobs", "N", 0,
                          "r2,marginal", "R2 Marg.", 3,
                          "r2.conditional", "R2 Cond.", 3,
                          "aic", "AIC", 0,
                          "bic", "BIC", 0,
                          "icc", "ICC", 2,
                          "rmse", "RMSE", 2)) %>%
  tab_spanner(label = md("**Electoral Democracy**"), columns = 2:5) %>%
  tab_spanner(label = md("**Democratic Indoctrination**"), columns = 6:9) %>%
  tab_footnote(footnote = md("**Source**: ESS Wave 10; weighted"))

### ICC
var_icc <- model.df %>% 
  filter(type == "Unconditional", main_iv == "vdem-poly") %>% 
  pull(model) %>% 
  .[[1]] %>% 
  insight::get_variance()

# iso3c (13.4%)
icc_iso3 <- var_icc$var.intercept[[1]] / (var_icc$var.residual + var_icc$var.intercept[[1]] + var_icc$var.intercept[[2]])

# cntry (2%)
icc_cntry <- var_icc$var.intercept[[2]] / (var_icc$var.residual + var_icc$var.intercept[[1]] + var_icc$var.intercept[[2]])

# Total (15%)
icc_iso3 + icc_cntry

### Marginal effects ----
# Using ggeffects (analogous to marginaleffects::predictions, see below)
#### Residence period (CoD) × VDem ----
# VDem: Democratic indoctrination content in education (v2xed_ed_dmcon)
residence_vind.pred <- ggeffects::ggpredict(model = model.df %>%
                                              filter(
                                                type == "Random slope",
                                                main_iv == "vdem-ind") %>%
                                              pull(model) %>% 
                                              .[[1]], 
                                            terms = c("timedest_scl [0:4 by = .1]", 
                                                      "v2xed_ed_dmcon_cgm [-0.3:0.3 by = 0.3]"),
                                            margin = "empirical",
                                            type = "fixed") 

# VDem: Electoral democracy index (v2x_polyarchy)
residence_vpoly.pred <- ggeffects::ggpredict(model = model.df %>%
                                               filter(
                                                 type == "Random slope",
                                                 main_iv == "vdem-poly") %>%
                                               pull(model) %>% 
                                               .[[1]], 
                                             terms = c("timedest_scl [0:4 by = .1]", 
                                                       "v2x_polyarchy_cgm [-0.3:0.3 by = 0.3]"),
                                             margin = "empirical",
                                             type = "fixed") 

#### Residence period (CoO) × VDem ----
# VDem: Democratic indoctrination content in education (v2xed_ed_dmcon)
residence_coo_vind.pred <- ggeffects::ggpredict(model = model.df %>%
                                                  filter(
                                                    type == "Random slope",
                                                    main_iv == "vdem-ind") %>%
                                                  pull(model) %>% 
                                                  .[[1]], 
                                                terms = c("timeorig_scl [0:4 by = .1]", 
                                                          "v2xed_ed_dmcon_cgm [-0.3:0.3 by = 0.3]"),
                                                margin = "empirical",
                                                type = "fixed") 

# VDem: Electoral democracy index (v2x_polyarchy)
residence_coo_vpoly.pred <- ggeffects::ggpredict(model = model.df %>%
                                                   filter(
                                                     type == "Random slope",
                                                     main_iv == "vdem-poly") %>%
                                                   pull(model) %>% 
                                                   .[[1]], 
                                                 terms = c("timeorig_scl [0:4 by = .1]", 
                                                           "v2x_polyarchy_cgm [-0.3:0.3 by = 0.3]"),
                                                 margin = "empirical",
                                                 type = "fixed") 

# Residence CoO (figure 4b)
# VDem: Democratic indoctrination content in education (v2xed_ed_dmcon)
residence_comb_vind.fig <- residence_vind.pred %>%
  as.data.frame() %>%
  mutate(where = "Country of Destination") %>%
  bind_rows(residence_coo_vind.pred %>%
              as.data.frame() %>%
              mutate(where = "Country of origin")) %>%
  ggplot(aes(x = x * 10, y = predicted, ymin = conf.low, ymax = conf.high, fill = where, linetype = where)) +
  geom_line() +
  geom_ribbon(alpha = .2) + 
  scale_x_continuous(breaks = seq(0, 60, 10), labels = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(7.5, 9.5, .5), labels = seq(7.5, 9.5, .5)) +
  labs(title = 
         "Predicted democratic values by residence period and democratic indoctrination", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: ESS Wave 10; weighted data",
       x = "", y = "", 
       fill = "Residence period in: ", linetype = "Residence period in: ") +
  facet_wrap(~str_c("VDem: ", group)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

# Residence coo and Germany combined (figure 4a)
# VDem: Electoral democracy index (v2x_polyarchy)
residence_comb_vpoly.fig <- residence_vpoly.pred %>%
  as.data.frame() %>%
  mutate(where = "Country of destination") %>%
  bind_rows(residence_coo_vpoly.pred %>%
              as.data.frame() %>%
              mutate(where = "Country of origin")) %>%
  ggplot(aes(x = x * 10, y = predicted, ymin = conf.low, ymax = conf.high, fill = where, linetype = where)) +
  geom_line() +
  geom_ribbon(alpha = .2) + 
  scale_x_continuous(breaks = seq(0, 60, 10), labels = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(7.5, 9.5, .5), labels = seq(7.5, 9.5, .5)) +
  labs(title = 
         "Predicted democratic values by residence period and electoral democracy", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: ESS Wave 10; weighted data",
       x = "", y = "", 
       fill = "Residence period in: ", linetype = "Residence period in: ") +
  facet_wrap(~str_c("VDem: ", group)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

# Robustness ----
#### Variation of formative years ----
# Load data and rename variables
ess_vary_fyears.df <- tibble(
  sample = c("formative years: 12", "formative years: 16"),
  data = list(
    tibble(import(here("data", "mergeddataset_V_ESS_12_14.dta"))),
    tibble(import(here("data", "mergeddataset_V_ESS_16_18.dta"))))) %>%
  mutate(data = map(data, ~.x %>%
                      filter(first_gen == 1) %>%
                      select(gender = gndr, educ = edulvlb, discrimination = discri, religion_str = religiosity, 
                             democ = demo1, iso3c = country_text_id, weight = anweight, v2x_polyarchy, v2x_libdem,
                             v2x_partipdem, v2xed_ed_dmcon, v2xed_ed_ptcon, v2xed_ptcon, muslim, timedest, timeorig, 
                             cntry, age = agea, year, idno, livecnta, mode) %>%
                      mutate(
                        muslim = factor(muslim, levels = c(0, 1), 
                                        labels = c("Other", "Muslim")),
                        discrimination = case_match(
                          discrimination,
                          0 ~ "no",
                          1 ~ "yes",
                          .default = NA_character_),
                        discrimination = factor(discrimination, 
                                                levels = c("no", "yes")),
                        gender = gender - 1,
                        gender = factor(gender, 
                                        levels = c(0, 1), 
                                        labels = c("Male", "Female")),
                        educ = case_match(educ,
                                          c(0, 113, 129, 212, 213, 221, 222, 223) ~ "ISCED 2 and lower",
                                          c(229, 311, 312, 313, 321, 322, 323) ~ "ISCED 3",
                                          c(412, 413, 421, 422, 423, 510, 520) ~ "ISCED 4 and 5A/B short",
                                          c(610, 620, 710, 720, 800) ~ "ISCED 5 medium and higher",
                                          .default = NA_character_),
                        educ = factor(educ, levels = c("ISCED 2 and lower", "ISCED 3", "ISCED 4 and 5A/B short", 
                                                       "ISCED 5 medium and higher")),
                        timeorig = timeorig - 14, # Count from zero since age 14 (just like in IB analysis)
                        mode = case_match(
                          mode,
                          1 ~ "Interview: F2F (CAPI)",
                          2 ~ "Interview: Video",
                          3 ~ "Self completion (CAWI)",
                          4 ~ "Self completion (PAPI)",
                          c(1, 2) ~ "Interview",
                          .default = NA_character_)) %>%
                      # Centering within cluster (CWC)
                      mutate(across(c(timedest, timeorig, v2x_polyarchy, v2xed_ed_dmcon),
                                    ~ .x - mean(.x, na.rm = TRUE),
                                    .names = "{.col}_cwc"),
                             .by = iso3c) %>%
                      # Grand mean centering (CGM)
                      mutate(across(c(timedest, timeorig, v2x_polyarchy, v2xed_ed_dmcon),
                                    ~ .x - mean(.x, na.rm = TRUE),
                                    .names = "{.col}_cgm")) %>%
                      # Cluster means
                      mutate(across(c(v2x_polyarchy, v2xed_ed_dmcon, timeorig, timedest),
                                    ~ mean(.x, na.rm = TRUE),
                                    .names = "{.col}_mean"),
                             .by = iso3c) %>%
                      # Custom rescaling of timedest and timeorig
                      mutate(across(c(timeorig, timedest),
                                    ~ .x / 10,
                                    .names = "{.col}_scl"),
                             .by = iso3c)))

# Run models
model_vary_fyears.df <- tibble(
  dv = "democ",
  type = rep(
    c("Unconditional",
      "Base", 
      "Interactions",
      "Random slope"), 2),
  iv = c(
    "1 + (1 | iso3c) + (1 | cntry)", 
    "gender + educ + timedest_scl + timeorig_scl + muslim + religion_str + v2x_polyarchy_cgm + discrimination + (1 | iso3c) + (1 | cntry)",
    "gender + educ + muslim + religion_str +  + discrimination + timedest_scl*v2x_polyarchy_cgm + timeorig_scl*v2x_polyarchy_cgm + (1 | iso3c) + (1 | cntry)",
    "gender + educ + muslim + religion_str + discrimination + timedest_scl*v2x_polyarchy_cgm + timeorig_scl*v2x_polyarchy_cgm + (1 + v2x_polyarchy_cgm + timeorig_scl | iso3c) + (1 | cntry)",
    "1 + (1 | iso3c) + (1 | cntry)", 
    "gender + educ + timedest_scl + timeorig_scl + muslim + religion_str + v2xed_ed_dmcon_cgm + discrimination + (1 | iso3c) + (1 | cntry)",
    "gender + educ + muslim + religion_str + discrimination + timedest_scl*v2xed_ed_dmcon_cgm + timeorig_scl*v2xed_ed_dmcon_cgm + (1 | iso3c) + (1 | cntry)",
    "gender + educ + muslim + religion_str + discrimination + timedest_scl*v2xed_ed_dmcon_cgm + timeorig_scl*v2xed_ed_dmcon_cgm + (1 + timeorig_scl + v2xed_ed_dmcon_cgm | iso3c) + (1 | cntry)"),
  main_iv = c(
    rep("vdem-poly", 4),
    rep("vdem-ind", 4)))

# Add different samples
model_vary_fyears.df <- tibble(
  sample = c("formative years: 12", 
             "formative years: 16"),
  data = c("ess_vary_fyears.df %>% filter(sample == 'formative years: 12') %>% pull(data) %>% .[[1]]",
           "ess_vary_fyears.df %>% filter(sample == 'formative years: 16') %>% pull(data) %>% .[[1]]")) %>%
  expand_grid(model_vary_fyears.df)

# VDem at year of immigration
model_vary_fyears.df <- model_vary_fyears.df %>%
  mutate(model = pmap(list(dv, iv, data), ~lmer(
    str_c(..1, " ~ ", ..2),
    REML = TRUE,
    control = lmerControl(
      optimizer = 'optimx', optCtrl = list(method = 'nlminb')),
    weights = weight,
    data = eval(rlang::parse_expr(..3)))))

# Name list column
names(model_vary_fyears.df$model) <- model_vary_fyears.df$type

### Model output ----
# modelsummary
# Appendix: Table 7c
mlm12.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
                        models = model_vary_fyears.df %>% filter(sample == "formative years: 12") %>% pull(model),
                        output = "gt",
                        stars = TRUE,
                        estimate = "{estimate}{stars}",
                        statistic = "({std.error})",
                        coef_map = c("(Intercept)" = "Intercept",
                                     "genderFemale" = "Gender: Female",
                                     "educISCED 3" = "Education: ISCED 3\n(Ref.: ISCED 2 and lower)",
                                     "educISCED 4 and 5A/B short" = "Educ.: ISCED 4 and 5A/B short",
                                     "educISCED 5 medium and higher" = "Educ.: ISCED 5 medium and higher",
                                     "timedest_scl" = "Period of residence (Country of destination)",
                                     "timeorig_scl" = "Period of residence (Country of origin)",
                                     "muslimMuslim" = "Muslim",
                                     "religion_str" = "Religiosity",
                                     "discriminationyes" = "Discrimination: Yes\n(Ref.: No)",
                                     "v2xed_ed_dmcon_cgm" = "Political socialization (V-Dem) [CGM]",
                                     "v2x_polyarchy_cgm" = "Political socialization (V-Dem) [CGM]",
                                     "timedest_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoD) × V-Dem [CGM]",
                                     "v2xed_ed_dmcon_cgm:timedest_scl" = "Period of residence (CoD) × V-Dem [CGM]",
                                     "timeorig_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "v2xed_ed_dmcon_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "timedest_scl:v2x_polyarchy_cgm" = "Period of residence (CoD) × V-Dem [CGM]",
                                     "v2x_polyarchy_cgm:timedest_scl" = "Period of residence (CoD) × V-Dem [CGM]",
                                     "timeorig_scl:v2x_polyarchy_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "v2x_polyarchy_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "SD (Intercept iso3c)" = "SD (Intercept: CoO)",
                                     "SD (Intercept cntry)" = "SD (Intercept: CoD)",
                                     "SD (v2x_polyarchy_cgm iso3c)" = "SD (V-Dem by CoO)",
                                     "SD (v2x_polyarchy_cgm cntry)" = "SD (V-Dem by CoD)",
                                     "SD (v2xed_ed_dmcon_cgm iso3c)" = "SD (V-Dem by CoO)",
                                     "SD (v2xed_ed_dmcon_cgm cntry)" = "SD (V-Dem by CoD)",
                                     "SD (Observations)" = "SD (Observations)"),
                        gof_map = tribble(
                          ~raw, ~clean, ~fmt,
                          "nobs", "N", 0,
                          "r2,marginal", "R2 Marg.", 3,
                          "r2.conditional", "R2 Cond.", 3,
                          "aic", "AIC", 0,
                          "bic", "BIC", 0,
                          "icc", "ICC", 2,
                          "rmse", "RMSE", 2)) %>%
  tab_spanner(label = md("**Electoral Democracy**"), columns = 2:5) %>%
  tab_spanner(label = md("**Democratic Indoctrination**"), columns = 6:9) %>%
  tab_footnote(footnote = md("**Source**: ESS Wave 10; weighted"))

# Appendix: Table 7d
mlm16.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
                          models = model_vary_fyears.df %>% filter(sample == "formative years: 16") %>% pull(model),
                          output = "gt",
                          stars = TRUE,
                          estimate = "{estimate}{stars}",
                          statistic = "({std.error})",
                          coef_map = c("(Intercept)" = "Intercept",
                                       "genderFemale" = "Gender: Female",
                                       "educISCED 3" = "Education: ISCED 3\n(Ref.: ISCED 2 and lower)",
                                       "educISCED 4 and 5A/B short" = "Educ.: ISCED 4 and 5A/B short",
                                       "educISCED 5 medium and higher" = "Educ.: ISCED 5 medium and higher",
                                       "timedest_scl" = "Period of residence (Country of destination)",
                                       "timeorig_scl" = "Period of residence (Country of origin)",
                                       "muslimMuslim" = "Muslim",
                                       "religion_str" = "Religiosity",
                                       "discriminationyes" = "Discrimination: Yes\n(Ref.: No)",
                                       "v2xed_ed_dmcon_cgm" = "Political socialization (V-Dem) [CGM]",
                                       "v2x_polyarchy_cgm" = "Political socialization (V-Dem) [CGM]",
                                       "timedest_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoD) × V-Dem",
                                       "v2xed_ed_dmcon_cgm:timedest_scl" = "Period of residence (CoD) × V-Dem [CGM]",
                                       "timeorig_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                       "v2xed_ed_dmcon_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                       "timedest_scl:v2x_polyarchy_cgm" = "Period of residence (CoD) × V-Dem [CGM]",
                                       "v2x_polyarchy_cgm:timedest_scl" = "Period of residence (CoD) × V-Dem [CGM]",
                                       "timeorig_scl:v2x_polyarchy_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                       "v2x_polyarchy_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                       "SD (Intercept iso3c)" = "SD (Intercept: CoO)",
                                       "SD (Intercept cntry)" = "SD (Intercept: CoD)",
                                       "SD (v2x_polyarchy_cgm iso3c)" = "SD (V-Dem by CoO)",
                                       "SD (v2x_polyarchy_cgm cntry)" = "SD (V-Dem by CoD)",
                                       "SD (v2xed_ed_dmcon_cgm iso3c)" = "SD (V-Dem by CoO)",
                                       "SD (v2xed_ed_dmcon_cgm cntry)" = "SD (V-Dem by CoD)",
                                       "SD (Observations)" = "SD (Observations)"),
                          gof_map = tribble(
                            ~raw, ~clean, ~fmt,
                            "nobs", "N", 0,
                            "r2,marginal", "R2 Marg.", 3,
                            "r2.conditional", "R2 Cond.", 3,
                            "aic", "AIC", 0,
                            "bic", "BIC", 0,
                            "icc", "ICC", 2,
                            "rmse", "RMSE", 2)) %>%
  tab_spanner(label = md("**Electoral Democracy**"), columns = 2:5) %>%
  tab_spanner(label = md("**Democratic Indoctrination**"), columns = 6:9) %>%
  tab_footnote(footnote = md("**Source**: ESS Wave 10; weighted"))

#### Sample: Age <= 40 years ----
# Using 'model.df' created earlier
# adding sample excluding respondents <= 40 years
model40.df <- model.df %>%
  mutate(data = "ess_democ_mod.df %>% filter(age <= 40)")

##### MLM ----
# Run models
model40.df <- model40.df %>%
  mutate(model = pmap(list(dv, iv, data), ~lme4::lmer(
    str_c(..1, " ~ ", ..2),
    REML = TRUE,
    weights = weight, # or pweights_a
    data = eval(rlang::parse_expr(..3)))))

# Name list column
names(model40.df$model) <- model40.df$type

# modelsummary
mlm40.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
                        models = model40.df %>% pull(model),
                        output = "gt",
                        stars = TRUE,
                        estimate = "{estimate}{stars}",
                        statistic = "({std.error})",
                        coef_map = c("(Intercept)" = "Intercept",
                                     "genderFemale" = "Gender: Female",
                                     "educISCED 3" = "Education: ISCED 3\n(Ref.: ISCED 2 and lower)",
                                     "educISCED 4 and 5A/B short" = "Educ.: ISCED 4 and 5A/B short",
                                     "educISCED 5 medium and higher" = "Educ.: ISCED 5 medium and higher",
                                     "timedest_scl" = "Period of residence (Country of destination)",
                                     "timeorig_scl" = "Period of residence (Country of origin)",
                                     "muslimMuslim" = "Muslim",
                                     "religion_str" = "Religiosity",
                                     "discriminationyes" = "Discrimination: Yes\n(Ref.: No)",
                                     "v2xed_ed_dmcon_cgm" = "Political socialization (V-Dem) [CGM]",
                                     "v2x_polyarchy_cgm" = "Political socialization (V-Dem) [CGM]",
                                     "timedest_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoD) × V-Dem [CGM]",
                                     "v2xed_ed_dmcon_cgm:timedest_scl" = "Period of residence (CoD) × V-Dem [CGM]",
                                     "timeorig_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "v2xed_ed_dmcon_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "timedest_scl:v2x_polyarchy_cgm" = "Period of residence (CoD) × V-Dem [CGM]",
                                     "v2x_polyarchy_cgm:timedest_scl" = "Period of residence (CoD) × V-Dem [CGM]",
                                     "timeorig_scl:v2x_polyarchy_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "v2x_polyarchy_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "SD (Intercept iso3c)" = "SD (Intercept: CoO)",
                                     "SD (Intercept cntry)" = "SD (Intercept: CoD)",
                                     "SD (v2x_polyarchy_cgm iso3c)" = "SD (V-Dem by CoO)",
                                     "SD (v2x_polyarchy_cgm cntry)" = "SD (V-Dem by CoD)",
                                     "SD (v2xed_ed_dmcon_cgm iso3c)" = "SD (V-Dem by CoO)",
                                     "SD (v2xed_ed_dmcon_cgm cntry)" = "SD (V-Dem by CoD)",
                                     "SD (Observations)" = "SD (Observations)"),
                        gof_map = tribble(
                          ~raw, ~clean, ~fmt,
                          "nobs", "N", 0,
                          "r2,marginal", "R2 Marg.", 3,
                          "r2.conditional", "R2 Cond.", 3,
                          "aic", "AIC", 0,
                          "bic", "BIC", 0,
                          "icc", "ICC", 2,
                          "rmse", "RMSE", 2)) %>%
  tab_spanner(label = md("**Electoral Democracy**"), columns = 2:5) %>%
  tab_spanner(label = md("**Democratic Indoctrination**"), columns = 6:9) %>%
  tab_footnote(footnote = md("**Source**: ESS10; weighted"))

# Residence period (CoD) × VDem
# VDem: Democratic indoctrination content in education (v2xed_ed_dmcon)
residence_vind40.pred <- ggeffects::ggpredict(model = model40.df %>%
                                                filter(
                                                  type == "Random slope",
                                                  main_iv == "vdem-ind") %>%
                                                pull(model) %>% 
                                                .[[1]], 
                                              terms = c("timedest_scl [all]", 
                                                        "v2xed_ed_dmcon_cgm [-0.3:0.3 by = 0.3]"),
                                              margin = "empirical",
                                              type = "fixed")

# VDem: Electoral democracy index (v2x_polyarchy)
residence_vpoly40.pred <- ggeffects::ggpredict(model = model40.df %>%
                                                 filter(
                                                   type == "Random slope",
                                                   main_iv == "vdem-poly") %>%
                                                 pull(model) %>% 
                                                 .[[1]], 
                                               terms = c("timedest_scl [all]", 
                                                         "v2x_polyarchy_cgm [-0.3:0.3 by = 0.3]"),
                                               margin = "empirical",
                                               type = "fixed")

# Residence period (CoO) × VDem
# VDem: Democratic indoctrination content in education (v2xed_ed_dmcon)
residence_coo_vind40.pred <- ggeffects::ggpredict(model = model40.df %>%
                                                    filter(
                                                      type == "Random slope",
                                                      main_iv == "vdem-ind") %>%
                                                    pull(model) %>% 
                                                    .[[1]], 
                                                  terms = c("timeorig_scl [all]", 
                                                            "v2xed_ed_dmcon_cgm [-0.3:0.3 by = 0.3]"),
                                                  margin = "empirical",
                                                  type = "fixed")

# VDem: Electoral democracy index (v2x_polyarchy)
residence_coo_vpoly40.pred <- ggeffects::ggpredict(model = model40.df %>%
                                                     filter(
                                                       type == "Random slope",
                                                       main_iv == "vdem-poly") %>%
                                                     pull(model) %>% 
                                                     .[[1]], 
                                                   terms = c("timeorig_scl [all]", 
                                                             "v2x_polyarchy_cgm [-0.3:0.3 by = 0.3]"),
                                                   margin = "empirical",
                                                   type = "fixed")

# Residence CoO
# VDem: Democratic indoctrination content in education (v2xed_ed_dmcon)
residence_comb_vind40.fig <- residence_vind40.pred %>%
  as.data.frame() %>%
  mutate(where = "Country of Destination") %>%
  bind_rows(residence_coo_vind40.pred %>%
              as.data.frame() %>%
              mutate(where = "Country of origin")) %>%
  ggplot(aes(x = x * 10, y = predicted, ymin = conf.low, ymax = conf.high, fill = where, linetype = where)) +
  geom_line() +
  geom_ribbon(alpha = .2) + 
  scale_x_continuous(breaks = seq(0, 75, 10), labels = seq(0, 75, 10)) +
  scale_y_continuous(breaks = seq(7.5, 9.5, .5), labels = seq(7.5, 9.5, .5)) +
  labs(title = 
         "Predicted democratic values by residence period and democratic indoctrination", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: ESS 10; weighted data",
       x = "", y = "", 
       fill = "Residence period in: ", linetype = "Residence period in: ") +
  facet_wrap(~str_c("VDem: ", group)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0)) 

# Residence coo and Germany combined
residence_comb_vpoly40.fig <- residence_vpoly40.pred %>%
  as.data.frame() %>%
  mutate(where = "Country of destination") %>%
  bind_rows(residence_coo_vpoly40.pred %>%
              as.data.frame() %>%
              mutate(where = "Country of origin")) %>%
  ggplot(aes(x = x * 10, y = predicted, ymin = conf.low, ymax = conf.high, fill = where, linetype = where)) +
  geom_line() +
  geom_ribbon(alpha = .2) + 
  scale_x_continuous(breaks = seq(0, 75, 10), labels = seq(0, 75, 10)) +
  scale_y_continuous(breaks = seq(7.5, 9.5, .5), labels = seq(7.5, 9.5, .5)) +
  labs(title = 
         "Predicted democratic values by residence period and electoral democracy", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: ESS 10; weighted data",
       x = "", y = "", 
       fill = "Residence period in: ", linetype = "Residence period in: ") +
  facet_wrap(~str_c("VDem: ", group)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0)) 

##### FE (age >= 40) ----
###### Create data ----
# Create a variable that country years under autocracy
fe40_mod.df <- ess_democ_mod.df %>%
  filter(age <= 40) %>%
  nest() %>%
  mutate(data_long = map(
    data, ~.x %>%
      # Make the dataset long with one observation per year in CoO
      select(idno, iso3c, year, livecnta) %>%
      filter(!is.na(livecnta)) %>%
      rowwise() %>%
      mutate(
        year = list(seq(year, livecnta))) %>%
      unnest(year) %>%
      ungroup() %>%
      # Join categorical VDem predictor
      left_join(
        y = vdem %>% 
          select(iso3c = country_text_id, year, e_v2x_polyarchy_3C),
        by = c("year", "iso3c")) %>%
      # Count years under autocracy
      mutate(
        e_v2x_polyarchy_3C_yrs = sum(e_v2x_polyarchy_3C == 0), 
        .by = c(idno)) %>%
      distinct(idno, e_v2x_polyarchy_3C_yrs))) 

# Join variable to original data and drop data 
# in artificial long format
fe40_mod.df <- fe40_mod.df %>%
  mutate(data = map2(.x = data, .y = data_long, 
                     ~left_join(.x, .y, by = "idno"))) %>%
  select(-data_long) %>%
  unnest(data)

###### Model ---- 
fe40_mod.df <- fe40_mod.df %>%
  nest() %>%
  mutate(
    fe_model40 = map(
      .x = data,
      ~fixest::feols(
        fml = democ ~ gender + educ + muslim + religion_str + discrimination + e_v2x_polyarchy_3C_yrs + timedest | iso3c + cntry,
        weights = .x$weight, data = .x)),
    lm_model40 = map(
      .x = data, 
      ~lm(
        formula = democ ~ gender + educ + muslim + religion_str + discrimination + e_v2x_polyarchy_3C_yrs + timedest,
        weights = weight, data = .x)))

# Summarize
# Note needs models from model.R from IB data
# Appendix: Table 10
library(sandwich)
fe40.tbl <- modelsummary(models = list(
  "Clustered Standard Errors - IB" = ib_nest.df$lm_model40[[2]],
  "Clustered SE + country-FE - IB" = ib_nest.df$fe_model40[[2]], 
  "Clustered SE - ESS" = fe40_mod.df$lm_model40[[1]],
  "Clustered SE + Two-way FE - ESS" = fe40_mod.df$fe_model40[[1]]),
  vcov = list(~ iso3c,
              ~ iso3c,
              ~ iso3c + cntry,
              NULL),
  coef_map = c("e_v2x_polyarchy_3C_yrs" = "Autocratic experience in CoO [in years]",
               "timedest" = "Period of residence (CoD)"),
  gof_map = tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "N", 0,
    "r2.squared", "R2", 3,
    "adj.r.squared", "R2 Adj.", 3,
    "aic", "AIC", 0,
    "bic", "BIC", 0,
    "rmse", "RMSE", 2),
  stars = T,
  output = "gt")

## Fixed effect model ----
##### Create data ----
# Create a variable that country years under autocracy
fe_mod.df <- ess_democ_mod.df %>%
  nest() %>%
  mutate(data_long = map(
    data, ~.x %>%
      # Make the dataset long with one observation per year in CoO
      select(idno, iso3c, year, livecnta) %>%
      filter(!is.na(livecnta)) %>%
      rowwise() %>%
      mutate(
        year = list(seq(year, livecnta))) %>%
      unnest(year) %>%
      ungroup() %>%
      # Join categorical VDem predictor
      left_join(
        y = vdem %>% 
          select(iso3c = country_text_id, year, e_v2x_polyarchy_3C),
        by = c("year", "iso3c")) %>%
      # Count years under autocracy
      mutate(
        e_v2x_polyarchy_3C_yrs = sum(e_v2x_polyarchy_3C == 0), 
        .by = c(idno)) %>%
      distinct(idno, e_v2x_polyarchy_3C_yrs))) 

# Join variable to original data and drop data 
# in artificial long format
fe_mod.df <- fe_mod.df %>%
  mutate(data = map2(.x = data, .y = data_long, 
                     ~left_join(.x, .y, by = "idno"))) %>%
  select(-data_long) %>%
  unnest(data)

##### Descriptive ----
fe_mod.df %>%
  filter(!is.na(e_v2x_polyarchy_3C_yrs)) %>%
  summarise(mu = wtd.mean(democ, na.rm = T, weights = weight), .by = "e_v2x_polyarchy_3C_yrs",
            n = n()) %>%
  ggplot(aes(x = e_v2x_polyarchy_3C_yrs, y = mu)) +
  geom_point(aes(size = n)) +
  geom_smooth(method = "lm")

##### Model ---- 
fe_mod.df <- fe_mod.df %>%
  nest() %>%
  mutate(
    fe_model = map(
      .x = data,
      ~fixest::feols(
        fml = democ ~ gender + educ + muslim + religion_str + discrimination + e_v2x_polyarchy_3C_yrs + timedest | iso3c + cntry,
        weights = .x$weight, data = .x)),
    lm_model = map(
      .x = data, 
      ~lm(
        formula = democ ~ gender + educ + muslim + religion_str + discrimination + e_v2x_polyarchy_3C_yrs + timedest,
        weights = weight, data = .x)))

# Predictions
fe_mod.df <- fe_mod.df %>%
  mutate(pred = map(
    fe_model, ~predictions(.x, newdata = datagrid(
      e_v2x_polyarchy_3C_yrs = seq(0, 45, 1))) %>%
      ggplot(., aes(x = e_v2x_polyarchy_3C_yrs, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_line() +
      geom_ribbon(alpha = .1) + 
      theme_ipsum()))

# Summarize
# Note needs models from model.R from IB data
# Appendix: Table 9
library(sandwich)
fe.tbl <- modelsummary(models = list(
  "Clustered Standard Errors - IB" = ib_nest.df$lm_model[[2]],
  "Clustered SE + country-FE - IB" = ib_nest.df$fe_model[[2]], 
  "Clustered SE - ESS" = fe_mod.df$lm_model[[1]],
  "Clustered SE + Two-way FE - ESS" = fe_mod.df$fe_model[[1]]),
  vcov = list(~ iso3c,
              ~ iso3c,
              ~ iso3c + cntry,
              NULL),
  coef_map = c("e_v2x_polyarchy_3C_yrs" = "Autocratic experience in CoO [in years]",
               "timedest" = "Period of residence (CoD)"),
  gof_map = tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "N", 0,
    "r2.squared", "R2", 3,
    "adj.r.squared", "R2 Adj.", 3,
    "aic", "AIC", 0,
    "bic", "BIC", 0,
    "rmse", "RMSE", 2),
  stars = T,
  output = "gt")

## Importance: Living in a democracy ----
# by first-gen, non-migrant
democ_importance.df <- ess_democ.df %>%
  filter(!is.na(implvdm)) %>%
  mutate(first_gen = factor(first_gen, 
                            levels = c(1, 0), 
                            labels = c("First-generation", "Non-migrant"))) %>%
  count(implvdm, first_gen, wt = anweight) %>%
  mutate(p = n / sum(n), .by = first_gen)

# Plot
democ_importance.fig <- democ_importance.df %>%
  ggplot(aes(x = first_gen, y = p, fill = fct_rev(factor(implvdm, 
                                                 levels = seq(0, 10, 1),
                                                 labels = c("0 - Not at all important",
                                                            seq(1, 9, 1),
                                                            "10 - Extremely important"))))) +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_text(aes(
    label = ifelse(p >= .05, 
                   str_replace(as.character(round(p * 100, digit = 1))
                               , "[.]", ","), "")), 
    family = "Roboto Condensed", size = 6, colour = "white",
    position = position_stack(vjust = .5)) +
  coord_flip() +
  scale_y_percent() +
  scico::scale_fill_scico_d(palette = "cork",
                            guide = guide_legend(reverse = TRUE, nrow = 1)) +
  labs(x = "", y = "", fill = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = "bottom", 
        legend.text = element_text(size = 12),
        legend.justification = "left")

# Table
democ_importance.tbl <- democ_importance.df %>%
  mutate(implvdm = factor(implvdm, 
                          levels = seq(0, 10, 1),
                          labels = c("0 - Not at all important",
                                     seq(1, 9, 1),
                                     "10 - Extremely important"))) %>%
  select(Importance = implvdm, first_gen, p) %>%
  pivot_wider(names_from = first_gen, values_from = p) %>%
  gt::gt() %>%
  gt::fmt_percent(where(is.numeric))

# Correlation: 
# - stfdem ("How satisfied with the way democracy works in country") 
# - implvdm ("How important for you to live in democratically governed country")
# - fairelcc ("In country national elections are free and fair")
democ_cor.df <- ess_democ.df %>%
  mutate(first_gen = factor(first_gen, 
                            levels = c(1, 0), 
                            labels = c("First-generation", "Non-migrant"))) %>%
  select(first_gen, stfdem, implvdm, fairelcc, anweight) %>%
  na.omit() %>%
  group_by(first_gen) %>%
  nest() %>%
  mutate(cor = map(data, ~collapse::pwcor(X = .[],
                               use = "pairwise.complete.obs",
                               w = .x$anweight) %>%
                     # remove weights
                     .[-4, -4]))
           
# Correlation: stfdem/demo1 
democ_x_stfdem_cor.df <- ess_democ.df %>%
  mutate(first_gen = factor(first_gen, 
                            levels = c(1, 0), 
                            labels = c("First-generation", "Non-migrant"))) %>%
  select(first_gen, stfdem, demo1, anweight) %>%
  na.omit() %>%
  group_by(first_gen) %>%
  nest() %>%
  mutate(cor = map(data, ~weights::wtd.cor(.x$stfdem, .x$demo1, weight = .x$anweight))) 

#### Model (importance of democracy) ----
# Using 'model.df' created earlier
# adding sample excluding respondents that don't assign importance to living in a democracy
model_imp_democ.df <- model.df %>%
  mutate(data = "ess_democ_mod.df %>% filter(implvdm >= 6)")

# Run models
model_imp_democ.df <- model_imp_democ.df %>%
  mutate(model = pmap(list(dv, iv, data), ~lme4::lmer(
    str_c(..1, " ~ ", ..2),
    REML = TRUE,
    weights = weight, # or pweights_a
    data = eval(rlang::parse_expr(..3)))))

# Name list column
names(model_imp_democ.df$model) <- model_imp_democ.df$type

# modelsummary
# Appendix: Table 8
mlm_imp_democ.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
                          models = model_imp_democ.df %>% pull(model),
                          output = "gt",
                          stars = TRUE,
                          estimate = "{estimate}{stars}",
                          statistic = "({std.error})",
                          coef_map = c("(Intercept)" = "Intercept",
                                       "genderFemale" = "Gender: Female",
                                       "educISCED 3" = "Education: ISCED 3\n(Ref.: ISCED 2 and lower)",
                                       "educISCED 4 and 5A/B short" = "Educ.: ISCED 4 and 5A/B short",
                                       "educISCED 5 medium and higher" = "Educ.: ISCED 5 medium and higher",
                                       "timedest_scl" = "Period of residence (Country of destination)",
                                       "timeorig_scl" = "Period of residence (Country of origin)",
                                       "muslimMuslim" = "Muslim",
                                       "religion_str" = "Religiosity",
                                       "discriminationyes" = "Discrimination: Yes\n(Ref.: No)",
                                       "v2xed_ed_dmcon_cgm" = "Political socialization (V-Dem) [CGM]",
                                       "v2x_polyarchy_cgm" = "Political socialization (V-Dem) [CGM]",
                                       "timedest_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoD) × V-Dem [CGM]",
                                       "v2xed_ed_dmcon_cgm:timedest_scl" = "Period of residence (CoD) × V-Dem [CGM]",
                                       "timeorig_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                       "v2xed_ed_dmcon_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                       "timedest_scl:v2x_polyarchy_cgm" = "Period of residence (CoD) × V-Dem [CGM]",
                                       "v2x_polyarchy_cgm:timedest_scl" = "Period of residence (CoD) × V-Dem [CGM]",
                                       "timeorig_scl:v2x_polyarchy_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                       "v2x_polyarchy_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                       "SD (Intercept iso3c)" = "SD (Intercept: CoO)",
                                       "SD (Intercept cntry)" = "SD (Intercept: CoD)",
                                       "SD (v2x_polyarchy_cgm iso3c)" = "SD (V-Dem by CoO)",
                                       "SD (v2x_polyarchy_cgm cntry)" = "SD (V-Dem by CoD)",
                                       "SD (v2xed_ed_dmcon_cgm iso3c)" = "SD (V-Dem by CoO)",
                                       "SD (v2xed_ed_dmcon_cgm cntry)" = "SD (V-Dem by CoD)",
                                       "SD (Observations)" = "SD (Observations)"),
                          gof_map = tribble(
                            ~raw, ~clean, ~fmt,
                            "nobs", "N", 0,
                            "r2,marginal", "R2 Marg.", 3,
                            "r2.conditional", "R2 Cond.", 3,
                            "aic", "AIC", 0,
                            "bic", "BIC", 0,
                            "icc", "ICC", 2,
                            "rmse", "RMSE", 2)) %>%
  tab_spanner(label = md("**Electoral Democracy**"), columns = 2:5) %>%
  tab_spanner(label = md("**Democratic Indoctrination**"), columns = 6:9) %>%
  tab_footnote(footnote = md("**Source**: ESS10; weighted"))

## Mode effect ----
# Descriptive
mode_dscr.tbl <- ess_democ.df %>% 
  mutate(mode = sjlabelled::as_character(mode)) %>% 
  count(cntry, mode) %>% 
  pivot_wider(names_from = mode, values_from = n) %>% 
  gt() %>% 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = "---") %>% 
  cols_move(columns = "NA", after = 6)

# Model
mode.tbl <- ess_democ_mod.df %>%
  lm(democ ~ mode, weights = weight, data = .) %>%
  modelsummary(stars = TRUE, output = "gt",
               coef_map = c(
                 "(Intercept)" = "Intercept",
                 "modeInterview: Video" = "Interview: Video\n[Ref.: Interview: F2F (CAPI)]",
                 "modeSelf completion (CAWI)" = "Self completion (CAWI)",
                 "modeSelf completion (PAPI)" = "Self completion (PAPI)"),
               vcov = ~cntry,
               gof_map = tribble(
                 ~raw, ~clean, ~fmt,
                 "nobs", "N", 0,
                 "r2.squaredl", "R2", 3,
                 "aic", "AIC", 0,
                 "bic", "BIC", 0))

# Export ----
## Main text ----
# Figure 2
ggsave(here("figure", "ESS", "Fig2-democ_by_vdem_ess.png"), plot = democ_vdem_ess.fig,
       dpi = "retina", device = ragg::agg_png(),
       width = 25, height = 15, units = "cm")

# Table 2
# MLM results (age: 14, cut off: 16)
gtsave(mlm.tbl, filename = "./figure/ESS/MLM_results.rtf")

# Figure 4a
# Electoral democracy
ggsave(here("figure", "ESS", "Fig4a-residence_x_vdem-poly_ess.png"), plot = residence_comb_vpoly.fig,
       dpi = "retina", device = ragg::agg_png(), 
       width = 25, height = 14, units = "cm")

# Figure 4b
# Democratic indoctrination
ggsave(here("figure", "ESS", "Fig4b-residence_x_vdem-ind_ess.png"), plot = residence_comb_vind.fig,
       dpi = "retina", device = ragg::agg_png(), 
       width = 25, height = 14, units = "cm")

## Appendix ----
# Figure 1b
ggsave(here("figure", "ESS", "Democ-boxplot-ESS.png"), plot = demo_boxplot_ess.fig,
       dpi = 300, device = ragg::agg_png(),
       width = 25, height = 15, units = "cm")

# Table 2b
gtsave(items_democ_ess.gt, filename = here("figure", "ESS", "Demo-items.rtf"))

# Table 2c (see: )

# Table 3c
gtsave(items_democ_poly_ess.gt, filename = here("figure", "ESS", "Demo-VDem-Poly_ESS.rtf"))

# Table 3d
gtsave(items_democ_ind_ess.gt, filename = here("figure", "ESS", "Demo-VDem-Ind_ESS.rtf"))

# Table 4b
gtsave(vdem_ess_cor.tbl, filename = here("figure", "ESS", "VDem-Correlation_ESS.rtf"))

# Table 6
gtsave(ess_predictors.tbl, filename = here("figure", "ESS", "Overview-Predictors_ESS.rtf"))

# Figure 2b
ggsave(here("figure", "ESS", "VDem-Development-CoO-Coverage_ESS.png"), plot = vdem_obs_dev_ess.fig,
       dpi = 300, device = ragg::agg_png(),
       width = 35, height = 20, units = "cm")

# Figure 3c
# Single items: Frequency plot
ggsave(here("figure", "ESS", "Single-Items-Freqplot-Poly-ESS.png"), plot = poly_item.fig,
       dpi = 300, device = ragg::agg_png(), bg = "white",
       width = 50, height = 20, units = "cm")

# Figure 3d
ggsave(here("figure", "ESS", "Single-Items-Freqplot-Dmcon-ESS.png"), plot = dmcon_item.fig,
       dpi = 300, device = ragg::agg_png(), bg = "white",
       width = 50, height = 20, units = "cm")

# Figure 4b
ggsave(here("figure", "ESS", "residence_vdem_timeorig_ess.png"), plot = democ_timeorig_ess.fig,
       dpi = 300, device = ragg::agg_png, bg = "white",
       width = 25, height = 14, units = "cm")

# Table 7c
# MLM results (formative years: 12)
gtsave(mlm12.tbl, filename = "./figure/ESS/MLM_results-Age12.rtf")

# Table 7d
# MLM results (formative years: 16)
gtsave(mlm16.tbl, filename = "./figure/ESS/MLM_results-Age16.rtf")

# Table 8
gtsave(mlm_imp_democ.tbl, filename = "./figure/ESS/MLM_result_importance_democracy.rtf")

# Table 9
gtsave(fe.tbl, filename = "./figure/IB/Fixed-Effect-Results.rtf")

# Table 10
gtsave(fe40.tbl, filename = "./figure/IB/Fixed-Effect-Results-Age40.rtf")

# Single items: Frequency plot
ggsave(here("figure", "ESS", "Single-items-Freqplot-ESS.png"), plot = demo_items_ess_freqplot.fig,
       dpi = 300, device = ragg::agg_png(), bg = "white",
       width = 50, height = 20, units = "cm")