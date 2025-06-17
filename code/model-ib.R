# Analysis ----
# Data created in create-data.R

# Setup ----
# Load/install pkgs
# ------------------------------------------------------------------------------------------------ #
if(!require("xfun")) install.packages("xfun")
xfun::pkg_attach2("tidyverse", "rio", "hrbrthemes", "fixest", "modelsummary", "marginaleffects", 
                  "lme4", "CR2", "conflicted", "lubridate", "here", "Cairo", "Hmisc", "gt", 
                  "gtExtras", "patchwork", "vdemdata", "svglite")

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
ib22_democ.df <- import(here("data", "ib22_democ.rds"))

# Formative years (age: 12, cutoff: 14)
ib22_f12year.df <- import(here("data", "ib22_f12year.rds"))

# Formative years (age: 14, cutoff: 16)
ib22_f14year.df <- import(here("data", "ib22_f14year.rds"))

# Formative years (age: 16, cutoff: 18)
ib22_f16year.df <- import(here("data", "ib22_f16year.rds"))

# Independent variables: dwf, dpu, dmk, drm, dgg, dme, dra
# Add mean score of independent variables
ib_nest.df <- tibble(
  sample = c("total", "formative years: 12", "formative years: 14", "formative years: 16"),
  data = list(ib22_democ.df, ib22_f12year.df, ib22_f14year.df, ib22_f16year.df)) %>%
  mutate(data = map(data, ~.x %>%
                      mutate(democ = rowMeans(across(c(dwf, dpu, dmk, drm, dgg)), 
                                              na.rm = FALSE))))

#### Create factor variables ----
ib_nest.df <- ib_nest.df %>%
  mutate(data = map(data, ~.x %>%
                      mutate(
                        muslim = case_match(
                          religion,
                          2 ~ 1,
                          c(1, 3, 4) ~ 0,
                          TRUE ~ NA_integer_),
                        muslim = factor(muslim, levels = c(0, 1), 
                                        labels = c("Other", "Muslim")),
                        religion = case_match(
                          religion,
                          1 ~ "Christian",
                          2 ~ "Muslim",
                          3 ~ "Other",
                          4 ~ "No religion",
                          TRUE ~ NA_character_),
                        religion = factor(religion,
                                          levels = c("Christian", "Muslim", 
                                                     "Other", "No religion")),
                        discrimination = case_match(
                          discrimination,
                          0 ~ "not at all",
                          1 ~ "low",
                          2 ~ "high",
                          3 ~ "very high",
                          TRUE ~ NA_character_),
                        discrimination = factor(discrimination, 
                                                levels = c("not at all", "low", 
                                                           "high", "very high")),
                        gender = factor(gender, 
                                            levels = c(0, 1), 
                                            labels = c("Male", "Female")),
                        fh = factor(fh, 
                                    levels = c(0, 1), 
                                    labels = c("No", "Yes")))))

# Description ----
# ------------------------------------------------------------------------------------------------ #
## Summary statistics ----
### Mean ----
mean.df <- ib_nest.df %>%
  filter(sample == "total") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    mig_type = case_when(
      migra == 1 ~ "natives",
      migra != 1 & iso3c != "DEU" ~ "first generation",
      migra != 1 & iso3c == "DEU" ~ "second generation",
      TRUE ~ NA_character_)) %>%
  filter(if_all(c(democ, mig_type, weight), ~!is.na(.)))

# Compute mean
mean.tbl <- mean.df %>%
  group_by(mig_type) %>%
  summarise(
    mean = wtd.mean(democ, 
                     weights = weight,
                     na.rm = TRUE),
    conf.low = weighted.ttest.ci(democ, 
                                  weights = weight, 
                                  conf.level = 0.95)[1],
    conf.high = weighted.ttest.ci(democ, 
                                   weights = weight, 
                                   conf.level = 0.95)[2],
    n = n()) %>% 
  ungroup() %>%
  left_join(y = mean.df %>% 
              group_by(mig_type) %>%
              summarize(data = list(democ), .groups = "drop"), by = "mig_type")

# Table 
mean.gt <- mean.tbl %>%
  mutate(
    across(starts_with("conf"), ~format(round(., 2), nsmall = 2)),
    "95%CI" = str_c("[", conf.low, ", ", conf.high, "]"),
    mig_type = factor(mig_type, 
                      levels = c("natives", "first generation", "second generation"),
                      labels = c("Natives", "First Generation", "Second Generation"))) %>%
  select(-starts_with("conf")) %>%
  arrange(mig_type) %>%
  gt() %>%
  gt_plt_dist(data, type = "histogram") %>%
  fmt_number(columns = c("mean", "95%CI"),
             decimals = 2) %>%
  cols_align(
    align = "left",
    columns = "mig_type") %>%
  cols_align_decimal() %>%
  cols_label(
    mig_type = "",
    mean = "Mean",
    `95%CI` = "95%-CI",
    n = "N",
    data = "Distribution") %>%
  tab_header(title = md("Importance of democracy")) %>%
  tab_source_note(source_note = md("**Source**: SVR-Integrationsbarometer 2022; weighted")) %>%
  tab_spanner(label = "Index",
              columns = c("mean", "95%CI")) %>%
  tab_spanner(label = "95")  

### Boxplot (App.: Fig. 1a) ----
demo_boxplot_ib.fig <- ib_nest.df %>%
  filter(sample == "total") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    mig_type = case_when(
      migra == 1 | migra != 1 & iso3c == "DEU" ~ "Non-migrant",
      migra != 1 & iso3c != "DEU" ~ "First-generation",
      TRUE ~ NA_character_)) %>%
  ggplot(aes(x = mig_type, y = democ, weight = weight)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "", y = "Democratic Values", title = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"))

### DV: Single items (App.: Tab. 2a) ----
items_democ.gt <- ib_nest.df %>%
  filter(sample == "total") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    mig_type = case_when(
      migra == 1 | migra != 1 & iso3c == "DEU" ~ "Non-migrant",
      migra != 1 & iso3c != "DEU" ~ "First-generation",
      TRUE ~ NA_character_)) %>%
  group_by(first_gen = factor(mig_type, 
                              labels = c("First-generation", "Non-migrant")), 
           .add = TRUE) %>%
  reframe(across(c(dwf, dpu, dmk, drm, dgg),
                 list(mean = ~wtd.mean(.x, weights = weight, na.rm = TRUE),
                      sd = ~sqrt(wtd.var(.x, weights = weight, na.rm = TRUE)),
                      min = ~min(.x, na.rm = TRUE),
                      max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = 2:21,
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(variable = case_match(variable,
                               "dwf" ~ "Fair elections",
                               "dpu" ~ "Party competition",
                               "dmk" ~ "Free media",
                               "drm" ~ "Protected minorities",
                               "dgg" ~ "Equality courts")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  gt() %>%
  fmt_number(decimals = 2, drop_trailing_zeros = TRUE)

### Frequency plots ----
demo_items_ib_freqplot.fig <- ib_nest.df %>%
  filter(sample == "total") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    mig_type = case_when(
      migra == 1 | migra != 1 & iso3c == "DEU" ~ "Non-migrant",
      migra != 1 & iso3c != "DEU" ~ "First-generation",
      TRUE ~ NA_character_)) %>%
  select(mig_type, dwf, dpu, dmk, drm, dgg, weight) %>%
  na.omit() %>%
  pivot_longer(cols = c(dwf, dpu, dmk, drm, dgg), names_to = "item", values_to = "response") %>%
  mutate(item = case_match(item,
                           "dwf" ~ "Fair elections",
                           "dpu" ~ "Party competition",
                           "dmk" ~ "Free media",
                           "drm" ~ "Protected minorities",
                           "dgg" ~ "Equality courts")) %>%
  count(response, mig_type, item, wt = weight) %>%
  mutate(p = n / sum(n), .by = c(item, mig_type),
         response = case_match(response, 
                               0 ~ "Not important at all",
                               1 ~ "Rather unimportant",
                               2 ~ "Rather important",
                               3 ~ "Very important"),
         response = factor(response, 
                           levels = c("Very important",
                                      "Rather important",
                                      "Rather unimportant",
                                      "Not important at all"))) %>%
  ggplot(aes(x = p, y = mig_type, fill = response)) +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_text(aes(label = ifelse(p >= .03, 
                               str_replace(as.character(round(p * 100, digit = 1))
                                           , "[.]", ","), "")), 
            family = "Roboto Condensed", size = 5, colour = "white", position = position_stack(vjust = .5)) +
  facet_wrap(~item) +
  scico::scale_fill_scico_d(palette = "cork",
                            guide = guide_legend(reverse = TRUE)) +
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
democ_vdem.df <- ib_nest.df %>%
  filter(sample == "formative years: 14") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    across(starts_with("v2x"), 
           ~factor(case_when(. <= 0.25 ~ "low",
                             . > 0.25 & . <= 0.5 ~ "rather low",
                             . > 0.5 & . <= 0.75 ~ "rather high",
                             . > 0.75 ~ "high",
                             .default = NA_character_),
                   levels = c("non-migrant", "low", "rather low", "rather high", "high")),
                .names = "{.col}_cut"))

# Add non-migrants
democ_vdem_nonmig.df <- ib_nest.df %>%
  filter(sample == "total") %>% 
  pull(data) %>%
  .[[1]] %>%
  filter(migra == 1, a_recno != 210084983) %>% # Remove single duplicated individual (wandjahr == 1945)
  mutate(
    v2x_polyarchy_cut = factor(ifelse(migra == 1, "non-migrant", NA_character_),
                        levels = c("non-migrant", "low", "rather low", "rather high", "high")),
    v2xed_ed_dmcon_cut = factor(ifelse(migra == 1, "non-migrant", NA_character_),
                             levels = c("non-migrant", "low", "rather low", "rather high", "high")))

# Electoral democracy index (v2x_polyarchy)
poly.fig <- democ_vdem.df %>%
  bind_rows(democ_vdem_nonmig.df) %>%
  filter(!is.na(democ), !is.na(v2x_polyarchy_cut)) %>%
  ggplot(aes(x = fct_rev(v2x_polyarchy_cut), y = democ, weight = weight)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "", y = "Democratic Values", title = "Electoral Democracy Index") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14))

# Democratic indoctrination content in education (v2xed_ed_dmcon)
dmcon.fig <- democ_vdem.df %>%
  bind_rows(democ_vdem_nonmig.df) %>%
  filter(!is.na(democ), !is.na(v2xed_ed_dmcon_cut)) %>%
  ggplot(aes(x = fct_rev(v2xed_ed_dmcon_cut), y = democ, weight = weight)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "", y = "Democratic Values", title = "Democratic Indoctrination Index") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14))

# Combine (Figure 1)
democ_vdem.fig <- poly.fig + dmcon.fig

##### Single items ----
# Appendix: Figure 3a
# Electoral democracy index (v2x_polyarchy)
poly_item.fig <- democ_vdem.df %>%
  bind_rows(democ_vdem_nonmig.df) %>%
  filter(if_all(c(dwf, dpu, dmk, drm, dgg, v2x_polyarchy_cut, weight), ~!is.na(.x))) %>%
  pivot_longer(cols = c(dwf, dpu, dmk, drm, dgg), names_to = "item", values_to = "response") %>%
  mutate(item = case_match(item,
                           "dwf" ~ "Fair elections",
                           "dpu" ~ "Party competition",
                           "dmk" ~ "Free media",
                           "drm" ~ "Protected minorities",
                           "dgg" ~ "Equality courts")) %>%
  count(response, v2x_polyarchy_cut, item, wt = weight) %>%
  mutate(p = n / sum(n), .by = c(item, v2x_polyarchy_cut),
         response = case_match(response, 
                               0 ~ "Not important at all",
                               1 ~ "Rather unimportant",
                               2 ~ "Rather important",
                               3 ~ "Very important"),
         response = factor(response, 
                           levels = c("Very important",
                                      "Rather important",
                                      "Rather unimportant",
                                      "Not important at all"))) %>%
  ggplot(aes(x = p, y = fct_rev(v2x_polyarchy_cut), fill = response)) +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_text(aes(label = ifelse(p >= .03, 
                               str_replace(as.character(round(p * 100, digit = 1))
                                           , "[.]", ","), "")), 
            family = "Roboto Condensed", size = 5, colour = "white", position = position_stack(vjust = .5)) +
  facet_wrap(~item) +
  scico::scale_fill_scico_d(palette = "cork",
                            guide = guide_legend(reverse = TRUE)) +
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
dmcon_item.fig <- democ_vdem.df %>%
  bind_rows(democ_vdem_nonmig.df) %>%
  filter(if_all(c(dwf, dpu, dmk, drm, dgg, v2xed_ed_dmcon_cut, weight), ~!is.na(.x))) %>%
  pivot_longer(cols = c(dwf, dpu, dmk, drm, dgg), names_to = "item", values_to = "response") %>%
  mutate(item = case_match(item,
                           "dwf" ~ "Fair elections",
                           "dpu" ~ "Party competition",
                           "dmk" ~ "Free media",
                           "drm" ~ "Protected minorities",
                           "dgg" ~ "Equality courts")) %>%
  count(response, v2xed_ed_dmcon_cut, item, wt = weight) %>%
  mutate(p = n / sum(n), .by = c(item, v2xed_ed_dmcon_cut),
         response = case_match(response, 
                               0 ~ "Not important at all",
                               1 ~ "Rather unimportant",
                               2 ~ "Rather important",
                               3 ~ "Very important"),
         response = factor(response, 
                           levels = c("Very important",
                                      "Rather important",
                                      "Rather unimportant",
                                      "Not important at all"))) %>%
  ggplot(aes(x = p, y = fct_rev(v2xed_ed_dmcon_cut), fill = response)) +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_text(aes(label = ifelse(p >= .03, 
                               str_replace(as.character(round(p * 100, digit = 1))
                                           , "[.]", ","), "")), 
            family = "Roboto Condensed", size = 5, colour = "white", position = position_stack(vjust = .5)) +
  facet_wrap(~item) +
  scico::scale_fill_scico_d(palette = "cork",
                            guide = guide_legend(reverse = TRUE)) +
  scale_x_percent() +
  labs(x = "", y = "", fill = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = "bottom", 
        legend.text = element_text(size = 12),
        legend.justification = "left")

#### Tables ----
# Electoral democracy index (v2x_polyarchy) (App.: Tab. 3a)
items_democ_poly_ib.gt <- ib_nest.df %>%
  filter(sample == "formative years: 14") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    across(starts_with("v2x"), 
           ~factor(case_when(. <= 0.25 ~ "low",
                             . > 0.25 & . <= 0.5 ~ "rather low",
                             . > 0.5 & . <= 0.75 ~ "rather high",
                             . > 0.75 ~ "high",
                             .default = NA_character_),
                   levels = c("low", "rather low", "rather high", "high")),
           .names = "{.col}_cut")) %>%
  filter(!is.na(v2x_polyarchy_cut)) %>%
  group_by(v2x_polyarchy_cut) %>%
  reframe(across(c(dwf, dpu, dmk, drm, dgg),
                 list(mean = ~wtd.mean(.x, weights = weight, na.rm = TRUE),
                      sd = ~sqrt(wtd.var(.x, weights = weight, na.rm = TRUE)),
                      min = ~min(.x, na.rm = TRUE),
                      max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = 2:21,
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(variable = case_match(variable,
                               "dwf" ~ "Fair elections",
                               "dpu" ~ "Party competition",
                               "dmk" ~ "Free media",
                               "drm" ~ "Protected minorities",
                               "dgg" ~ "Equality courts")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  gt() %>%
  fmt_number(decimals = 2, drop_trailing_zeros = TRUE)

# Democratic indoctrination content in education (v2xed_ed_dmcon) (App.: Tab. 3b)
items_democ_ind_ib.gt <- ib_nest.df %>%
  filter(sample == "formative years: 14") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(
    across(starts_with("v2x"), 
           ~factor(case_when(. <= 0.25 ~ "low",
                             . > 0.25 & . <= 0.5 ~ "rather low",
                             . > 0.5 & . <= 0.75 ~ "rather high",
                             . > 0.75 ~ "high",
                             .default = NA_character_),
                   levels = c("low", "rather low", "rather high", "high")),
           .names = "{.col}_cut")) %>%
  filter(!is.na(v2xed_ed_dmcon_cut)) %>%
  group_by(v2xed_ed_dmcon_cut) %>%
  reframe(across(c(dwf, dpu, dmk, drm, dgg),
                 list(mean = ~wtd.mean(.x, weights = weight, na.rm = TRUE),
                      sd = ~sqrt(wtd.var(.x, weights = weight, na.rm = TRUE)),
                      min = ~min(.x, na.rm = TRUE),
                      max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = 2:21,
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(variable = case_match(variable,
                               "dwf" ~ "Fair elections",
                               "dpu" ~ "Party competition",
                               "dmk" ~ "Free media",
                               "drm" ~ "Protected minorities",
                               "dgg" ~ "Equality courts")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  gt() %>%
  fmt_number(decimals = 2, drop_trailing_zeros = TRUE)

### Overview of predictors ----
# Create extra row for number of clusters
n_cluster <- ib_nest.df %>%
  filter(sample == "formative years: 14") %>%
  pull(data) %>%
  .[[1]] %>%
  count(iso3c) %>%
  summarise(Clusters = "Cluster (CoO)", n_total = str_c("Total: ", length(n)), 
            mean = mean(n), 
            `Std. Dev.` = sd(n))

# All predictors (App.: Tab. 6)
ib_predictors.tbl <- ib_nest.df %>%
  filter(sample == "formative years: 14") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(educ = fct_recode(educ, low = "niedrig", medium = "mittel", high = "hoch", "in school" = "Schüler")) %>%
  select(`Democratic values` = democ, `Residence period (CoO)` = timeorig, 
         `Residence period (CoD)` = timedest, `Electoral democracy` = v2x_polyarchy, 
         `Democratic indoctrination` = v2xed_ed_dmcon, `Gender` = gender, `Education` = educ, 
         `Discrimination` = discrimination, `Muslim` = muslim, `Religiosity` = religion_str) %>%
  labelled::remove_var_label() %>%
  modelsummary::datasummary_balance(~1, data = ., output = "gt", 
                                    add_rows = n_cluster) 

### Correlation between VDem measures ----
vdem_cor.df <- ib_nest.df %>%
  filter(sample %in% c("formative years: 14", "formative years: 17")) %>%
  mutate(cor.tbl = map(data, ~.x %>%
                         select("v2x_polyarchy", "v2x_libdem", "v2x_partipdem",
                                "v2xed_ed_dmcon",	"v2xed_ed_ptcon",	"v2xed_ptcon", "weight") %>%
                         collapse::pwcor(X = .[], 
                                         use = "pairwise.complete.obs",
                                         w = .$weight) %>%
                         # remove weights
                         .[-7, -7])) 

# Table of correlations in analysis (Sample: formative years) (App.: Tab. 4a)
vdem_cor.tbl <- vdem_cor.df %>%
  filter(sample == "formative years: 14") %>%
  pull(cor.tbl) %>%
  .[[1]] %>%
  as.data.frame(row.names = c("v2x_polyarchy", "v2x_libdem", "v2x_partipdem", "v2xed_ed_dmcon",
                              "v2xed_ed_ptcon", "v2xed_ptcon")) %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = md("Correlation table")) %>%
  tab_source_note(source_note = md("**Source**: SVR-Integrationsbarometer 2022; weighted")) %>%
  fmt_number(decimals = 3)

### by VDEM and time ----
#### Timeorig ----
# Needs democ_vdem.df, see: line 213ff.
democ_vdem_timeorig.df <- democ_vdem.df %>%
  tibble() %>%
  mutate(timeorig_cut = cut(timeorig, 
                            breaks = c(seq(0, 30, by = 5), Inf),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30+"),
                            right = FALSE, include.lowest = TRUE)) 

#### Timedest ----
democ_vdem_timedest.df <- democ_vdem.df %>%
  tibble() %>%
  mutate(timedest_cut = cut(timedest, 
                            breaks = c(seq(0, 30, by = 5), Inf),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30+"),
                            right = FALSE, include.lowest = TRUE))

#### Compute ----
# Electoral democracy index (v2x_polyarchy)
democ_poly_timeorig.df <- democ_vdem_timeorig.df %>%
  filter(if_all(c(democ, v2x_polyarchy_cut, timeorig_cut, weight), ~!is.na(.x))) %>%
  summarise(
    mean = wtd.mean(democ, 
                    weights = weight,
                    na.rm = TRUE),
    conf.low = weighted.ttest.ci(democ, 
                                 weights = weight, 
                                 conf.level = 0.95)[1],
    conf.high = weighted.ttest.ci(democ, 
                                  weights = weight, 
                                  conf.level = 0.95)[2],
    .by = c(v2x_polyarchy_cut, timeorig_cut), 
    n = n()) %>%
  rename(vdem_qnt = v2x_polyarchy_cut)

# Democratic indoctrination content in education (v2xed_ed_dmcon)
democ_indoc_timeorig.df <- democ_vdem_timeorig.df %>%
  filter(if_all(c(democ, v2xed_ed_dmcon_cut, timeorig_cut, weight), ~!is.na(.x))) %>%
  summarise(
    mean = wtd.mean(democ, 
                    weights = weight,
                    na.rm = TRUE),
    conf.low = weighted.ttest.ci(democ, 
                                 weights = weight, 
                                 conf.level = 0.95)[1],
    conf.high = weighted.ttest.ci(democ, 
                                  weights = weight, 
                                  conf.level = 0.95)[2],
    .by = c(v2xed_ed_dmcon_cut, timeorig_cut), 
    n = n()) %>%
  rename(vdem_qnt = v2xed_ed_dmcon_cut)

# Join
democ_timeorig.df <- democ_poly_timeorig.df %>%
  bind_rows(democ_indoc_timeorig.df, .id = "vdem") %>%
  mutate(vdem = if_else(vdem == 1, "Electoral Democracy", "Democratic Indoctrination"))
  
##### Plot ---- 
# Appendix: Figure 4a
democ_timeorig.fig <- democ_timeorig.df %>%
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
democ_poly_timeorig_timedest05.df <- democ_vdem_timeorig.df %>%
  filter(if_all(c(democ, v2x_polyarchy_cut, timeorig_cut, timedest, weight), ~!is.na(.x)),
         timedest <= 5) %>%
  summarise(
    mean = wtd.mean(democ, 
                    weights = weight,
                    na.rm = TRUE),
    conf.low = weighted.ttest.ci(democ, 
                                 weights = weight, 
                                 conf.level = 0.95)[1],
    conf.high = weighted.ttest.ci(democ, 
                                  weights = weight, 
                                  conf.level = 0.95)[2],
    .by = c(v2x_polyarchy_cut, timeorig_cut), 
    n = n()) %>%
  rename(vdem_qnt = v2x_polyarchy_cut)

# Democratic indoctrination content in education (v2xed_ed_dmcon)
democ_indoc_timeorig_timedest05.df <- democ_vdem_timeorig.df %>%
  filter(if_all(c(democ, v2xed_ed_dmcon_cut, timeorig_cut, timedest, weight), ~!is.na(.x)),
         timedest <= 5) %>%
  summarise(
    mean = wtd.mean(democ, 
                    weights = weight,
                    na.rm = TRUE),
    conf.low = weighted.ttest.ci(democ, 
                                 weights = weight, 
                                 conf.level = 0.95)[1],
    conf.high = weighted.ttest.ci(democ, 
                                  weights = weight, 
                                  conf.level = 0.95)[2],
    .by = c(v2xed_ed_dmcon_cut, timeorig_cut), 
    n = n()) %>%
  rename(vdem_qnt = v2xed_ed_dmcon_cut)

# Join
democ_vdem_timeorig_timedest05.df <- democ_poly_timeorig_timedest05.df %>%
  bind_rows(y = democ_indoc_timeorig_timedest05.df, .id = "vdem") %>%
  mutate(vdem = if_else(vdem == 1, "Electoral Democracy", "Democratic Indoctrination"))

# Plot
democ_timeorig_timedest05.fig <- democ_vdem_timeorig_timedest05.df %>%
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
# Appendix: Figure 2a
# Electoral democracy index (v2x_polyarchy)
vdem_obs_poly.fig <- ib_nest.df %>%
  filter(sample == "formative years: 14") %>%
  pull(data) %>%
  .[[1]] %>%
  ggplot(aes(x = formative_year, y = v2x_polyarchy, fill = iso3c)) +
  geom_line(data = . %>% filter(!iso3c %in% c("RUS", "AFG", "USA")),
            stat = "identity", alpha = 0.2, colour = "#00BFC4") +
  geom_line(data = . %>% filter(iso3c %in% c("RUS", "AFG", "USA")), colour = "black", linetype = "dashed") +
  geom_text(data = . %>% filter(iso3c %in% c("RUS", "AFG", "USA")) %>%
              slice_max(formative_year, by = iso3c), aes(label = iso3c), nudge_x = 1.75, 
            family = "Roboto Condensed") +
  stat_summary(data = ~mutate(., obs = n(), .by = c(iso3c, formative_year)) %>% filter(obs >= 3),
               aes(group = 1), fun = mean, geom = "line", color = "#F8766D", linewidth = 1) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 20)) +
  scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1)) +
  labs(title = "Electoral Democracy over time", 
       subtitle = "", 
       x = "", y = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14))

# Main countries of origin over time
main_by_period.df <- ib_nest.df %>%
  filter(sample == "formative years: 14") %>%
  pull(data) %>%
  .[[1]] %>%
  mutate(period = cut(formative_year, breaks = c(1937, seq(1940, 2020, by = 5)), 
                      include.lowest = TRUE, include.highest = TRUE)) %>%
  summarise(across(c(v2x_polyarchy, v2xed_ed_dmcon),
                   ~mean(.x, na.rm = T), .names = "{.col}_mean"), 
            .by = c(iso3c, period),
            n = n()) %>%
  slice_max(n = 10, order_by = n, by = period) %>%
  arrange(period, n)

# Democratic indoctrination content in education (v2xed_ed_dmcon)
vdem_obs_indoc.fig <- ib_nest.df %>%
  filter(sample == "formative years: 14") %>%
  pull(data) %>%
  .[[1]] %>%
  ggplot(aes(x = formative_year, y = v2xed_ed_dmcon, fill = iso3c)) +
  geom_line(data = . %>% filter(!iso3c %in% c("RUS", "AFG", "USA")),
            stat = "identity", alpha = 0.2, colour = "#00BFC4") +
  geom_line(data = . %>% filter(iso3c %in% c("RUS", "AFG", "USA")), colour = "black", linetype = "dashed") +
  geom_text(data = . %>% filter(iso3c %in% c("RUS", "AFG", "USA")) %>%
              slice_max(formative_year, by = iso3c), aes(label = iso3c), nudge_x = 1.75,
            family = "Roboto Condensed") +
  stat_summary(data = ~mutate(., obs = n(), .by = c(iso3c, formative_year)) %>% filter(obs >= 3),
               aes(group = 1), fun = mean, geom = "line", color = "#F8766D", linewidth = 1) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 20)) +
  scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1)) +
  labs(title = "Democratic indoctrination over time", 
       subtitle = "", 
       x = "", y = "") +
  theme_ipsum(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14)) +
  coord_cartesian(clip = 'off')

# Get Map
wmap.sf <- giscoR::gisco_get_countries() %>%
  mutate(
    covered_in_ib = if_else(
      ISO3_CODE %in% c(ib_nest.df[ib_nest.df$sample == "formative years: 14",]$data[[1]]$iso3c),
      "included", "not included"))

# Missing matches
unique(ib_nest.df$data[[3]]$iso3c[which(!ib_nest.df$data[[3]]$iso3c %in% wmap.sf$ISO3_CODE)])

# Create map of included countries
countries_covered.map <- ggplot() +
  geom_sf(data = wmap.sf[!wmap.sf$CNTR_NAME == "Antarctica",], aes(fill = covered_in_ib)) +
  scale_fill_manual(values = c("included" = "#00BFC4", "not included" = "#F8766D")) +
  coord_sf(crs = sf::st_crs("EPSG:3857")) +
  labs(title = "Covered countries of origin", fill = "") +
  cowplot::theme_map() +
  theme(legend.position = "bottom")

# Use 'patchwork' to combine
vdem_obs_dev_ib.fig <- vdem_obs_poly.fig | vdem_obs_indoc.fig | countries_covered.map

# Add Panel tags
vdem_obs_dev_ib.fig <- vdem_obs_dev_ib.fig + plot_annotation(tag_levels = 'A')

# Hierachical model ---- 
### Centering predictors
ib_nest.df <- ib_nest.df %>%
  filter(sample %in% str_c("formative years: ", c("12", "14", "16"))) %>%
  # Centering within cluster (CWC)
  mutate(data = map(data, ~ .x %>%
                      mutate(across(c(timedest, timeorig, v2x_polyarchy, v2xed_ed_dmcon),
                                    ~ .x - mean(.x, na.rm = TRUE),
                                    .names = "{.col}_cwc"),
                             .by = iso3c))) %>%
  # Grand mean centering (CGM)
  mutate(data = map(data, ~ .x %>%
                      mutate(across(c(timedest, timeorig, v2x_polyarchy, v2xed_ed_dmcon),
                                    ~ .x - mean(.x, na.rm = TRUE),
                                    .names = "{.col}_cgm")))) %>%
  # Cluster means
  mutate(data = map(data, ~ .x %>%
                      mutate(across(c(v2x_polyarchy, v2xed_ed_dmcon, timeorig, timedest),
                                    ~ mean(.x, na.rm = TRUE),
                                    .names = "{.col}_mean"),
                             .by = iso3c))) %>%
  # Custom rescaling of timedest and timeorig
  mutate(data = map(data, ~ .x %>%
                      mutate(across(c(timeorig, timedest),
                                    ~ .x / 10,
                                    .names = "{.col}_scl"),
                             .by = iso3c)))

# Mean of v2x_polyarchy / v2x_ed_dmcon
ib_nest.df[ib_nest.df$sample == "formative years: 14",]$data %>%
  .[[1]] %>%
  summarise(across(c(v2x_polyarchy, v2xed_ed_dmcon), ~mean(.x, na.rm = TRUE)))

### Rescale weights ----
ib_nest.df <- ib_nest.df %>%
  mutate(data = map(data, ~datawizard::rescale_weights(.x %>% 
                                            filter(if_all(c(iso3c, weight), 
                                                          ~!is.na(.))), 
                                          by = "iso3c", 
                                          probability_weights = "weight")))

## Run models ----
model.df <- tibble(
  dv = "democ",
  type = rep(
    c("Unconditional",
      "Base", 
      "Interactions",
      "Random slope"), 2),
  iv = c(
    "1 + (1 | iso3c)", 
    "gender + educ + timedest_scl + timeorig_scl + muslim + religion_str + v2x_polyarchy_cgm + discrimination + (1 | iso3c)",
    "gender + educ + muslim + religion_str + discrimination + timedest_scl*v2x_polyarchy_cgm + timeorig_scl*v2x_polyarchy_cgm + (1 | iso3c)",
    "gender + educ + muslim + religion_str + discrimination + timedest_scl*v2x_polyarchy_cgm + timeorig_scl*v2x_polyarchy_cgm + (1 + v2x_polyarchy_cgm + timeorig_scl + timedest_scl | iso3c)",
    "1 + (1 | iso3c)", 
    "gender + educ + timedest_scl + timeorig_scl + muslim + religion_str + v2xed_ed_dmcon_cgm + discrimination + (1 | iso3c)",
    "gender + educ + muslim + religion_str + discrimination + timedest_scl*v2xed_ed_dmcon_cgm + timeorig_scl*v2xed_ed_dmcon_cgm + (1 | iso3c)",
    "gender + educ + muslim + religion_str + discrimination + timedest_scl*v2xed_ed_dmcon_cgm + timeorig_scl*v2xed_ed_dmcon_cgm + (1 + timeorig_scl + timedest_scl + v2xed_ed_dmcon_cgm | iso3c)"),
  main_iv = c(
    rep("vdem-poly", 4),
    rep("vdem-ind", 4)))

# Add different samples
model.df <- tibble(
  sample = c("formative years: 12", 
             "formative years: 14",
             "formative years: 16"),
  data = c("ib_nest.df %>% filter(sample == 'formative years: 12') %>% pull(data) %>% .[[1]]",
           "ib_nest.df %>% filter(sample == 'formative years: 14') %>% pull(data) %>% .[[1]]",
           "ib_nest.df %>% filter(sample == 'formative years: 16') %>% pull(data) %>% .[[1]]")) %>%
  expand_grid(model.df)

# Run models
# VDem at year of immigration
model.df <- model.df %>%
  mutate(model = pmap(list(dv, iv, data), ~lme4::lmer(
    str_c(..1, " ~ ", ..2),
    REML = TRUE,
    control = lmerControl(
      optimizer = 'optimx', optCtrl = list(method = 'nlminb')),
    weights = weight, # or rescaled_weights_a for scaled weights
    data = eval(rlang::parse_expr(..3)))))

# Name list column
names(model.df$model) <- model.df$type

# Add robust SE using CR2::robust_mixed
# See: https://francish.net/post/robust-standard-errors/
model.df <- model.df %>%
  mutate(robust = map(model, ~robust_mixed(.x, type = "CR0")))

### Model output ----
# Using 'modelsummary' (Table 1)
mlm.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
             models = model.df %>% filter(sample == "formative years: 14") %>% pull(model), # pull(robust) for robust SE
             stars = TRUE,
             estimate = "{estimate}{stars}",
             statistic = "({std.error})",
             output = "gt",
             coef_map = c("(Intercept)" = "Intercept",
                          "genderFemale" = "Gender: Female",
                          "educmittel" = "Education: medium\n(Ref.: low)",
                          "educhoch" = "Educ.: high",
                          "educSchüler" = "Educ.: in school",
                          "timedest_scl" = "Period of residence (Germany)",
                          "timeorig_scl" = "Period of residence (Country of origin)",
                          "muslimMuslim" = "Muslim",
                          "refugeeYes" = "Refugee",
                          "religion_str" = "Religiosity",
                          "discriminationlow" = "Discrimination: low\n(Ref.: no at all)",
                          "discriminationhigh" = "Dis.: high",
                          "discriminationvery high" = "Dis.: very high",
                          "v2xed_ed_dmcon_cgm" = "Political socialization [CGM] (V-Dem)",
                          "v2x_polyarchy_cgm" = "Political socialization [CGM] (V-Dem)",
                          "timedest_scl:v2xed_ed_dmcon_cgm" = "Period of residence (Germany) × V-Dem [CGM]",
                          "v2xed_ed_dmcon_cgm:timedest_scl" = "Period of residence (Germany) × V-Dem [CGM]",
                          "timeorig_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                          "v2xed_ed_dmcon_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                          "timedest_scl:v2x_polyarchy_cgm" = "Period of residence (Germany) × V-Dem [CGM]",
                          "v2x_polyarchy_cgm:timedest_scl" = "Period of residence (Germany) × V-Dem [CGM]",
                          "timeorig_scl:v2x_polyarchy_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                          "v2x_polyarchy_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                          "SD (Intercept iso3c)" = "SD (Intercept: CoO)",
                          "SD (v2xed_ed_dmcon_cgm iso3c)" = "SD (V-Dem by CoO)",
                          "SD (v2x_polyarchy_cgm iso3c)" = "SD (V-Dem by CoO)",
                          "SD (timedest_scl iso3c)" = "SD (Period of residence (Germany) by CoO)",
                          "SD (timeorig_scl iso3c)" = "SD (Period of residence (CoO) by CoO)",
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
  tab_footnote(footnote = md("**Source**: SVR-Integrationsbarometer 2022; weighted"))

### Marginal effects ----
# Using ggeffects (analogous to marginaleffects::predictions, see below)
#### Residence period (DEU) × VDem ----
# VDem: Democratic indoctrination content in education (v2xed_ed_dmcon)
residence_vind.pred <- ggeffects::ggpredict(model = model.df %>%
                                         filter(
                                           sample == "formative years: 14",
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
                                                 sample == "formative years: 14",
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
                                           sample == "formative years: 14",
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
                                                    sample == "formative years: 14",
                                                    type == "Random slope",
                                                    main_iv == "vdem-poly") %>%
                                                  pull(model) %>% 
                                                  .[[1]], 
                                                terms = c("timeorig_scl [0:4 by = .1]", 
                                                          "v2x_polyarchy_cgm [-0.3:0.3 by = 0.3]"),
                                                margin = "empirical",
                                                type = "fixed") 

# Residence in CoO and in Germany combined (figure 3a)
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
  scale_y_continuous(limits = c(2.0, 3.5), breaks = seq(2.0, 3.0, .2), labels = seq(2.0, 3.0, 0.2)) +
  labs(title = 
         "Predicted democratic values by residence period and electoral democracy", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: Integration Barometer 2022; weighted data",
       x = "", y = "", 
       fill = "Residence period in: ", linetype = "Residence period in: ") +
  facet_wrap(~str_c("VDem: ", group)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

# Residence in CoO and in Germany combined (Figure 3b)
# VDem: Democratic indoctrination content in education (v2xed_ed_dmcon)
residence_comb_vind.fig <- residence_vind.pred %>%
  as.data.frame() %>%
  mutate(where = "Country of destination") %>%
  bind_rows(residence_coo_vind.pred %>%
              as.data.frame() %>%
              mutate(where = "Country of origin")) %>%
  ggplot(aes(x = x * 10, y = predicted, ymin = conf.low, ymax = conf.high, fill = where, linetype = where)) +
  geom_line() +
  geom_ribbon(alpha = .2) + 
  scale_x_continuous(breaks = seq(0, 60, 10), labels = seq(0, 60, 10)) +
  scale_y_continuous(limits = c(2, 3.5), breaks = seq(2.0, 3.0, .2), labels = seq(2.0, 3.0, 0.2)) +
  labs(title = 
         "Predicted democratic values by residence period and democratic indoctrination", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: Integration Barometer 2022; weighted data",
       x = "", y = "", 
       fill = "Residence period in: ", linetype = "Residence period in: ") +
  facet_wrap(~str_c("VDem: ", group)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

### Robustness ----
#### Variation of formative years ----
# Appendix: Table 7a
mlm12.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
                        models = model.df %>% filter(sample == "formative years: 12") %>% pull(model),
                        stars = TRUE,
                        estimate = "{estimate}{stars}",
                        statistic = "({std.error})",
                        output = "gt",
                        coef_map = c("(Intercept)" = "Intercept",
                                     "genderFemale" = "Gender: Female",
                                     # "age" = "Age",
                                     "educmittel" = "Education: medium\n(Ref.: low)",
                                     "educhoch" = "Educ.: high",
                                     "educSchüler" = "Educ.: in school",
                                     "timedest_scl" = "Period of residence (Germany)",
                                     "timeorig_scl" = "Period of residence (Country of origin)",
                                     "muslimMuslim" = "Muslim",
                                     "refugeeYes" = "Refugee",
                                     "religion_str" = "Religiosity",
                                     "discriminationlow" = "Discrimination: low\n(Ref.: no at all)",
                                     "discriminationhigh" = "Dis.: high",
                                     "discriminationvery high" = "Dis.: very high",
                                     "v2xed_ed_dmcon_cgm" = "Political socialization [CGM] (V-Dem)",
                                     "v2x_polyarchy_cgm" = "Political socialization [CGM] (V-Dem)",
                                     "timedest_scl:v2xed_ed_dmcon_cgm" = "Period of residence (Germany) × V-Dem [CGM]",
                                     "v2xed_ed_dmcon_cgm:timedest_scl" = "Period of residence (Germany) × V-Dem [CGM]",
                                     "timeorig_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "v2xed_ed_dmcon_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "timedest_scl:v2x_polyarchy_cgm" = "Period of residence (Germany) × V-Dem [CGM]",
                                     "v2x_polyarchy_cgm:timedest_scl" = "Period of residence (Germany) × V-Dem [CGM]",
                                     "timeorig_scl:v2x_polyarchy_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "v2x_polyarchy_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "SD (Intercept iso3c)" = "SD (Intercept: CoO)",
                                     "SD (v2xed_ed_dmcon_cgm iso3c)" = "SD (V-Dem by CoO)",
                                     "SD (v2x_polyarchy_cgm iso3c)" = "SD (V-Dem by CoO)",
                                     "SD (timedest_scl iso3c)" = "SD (Period of residence (Germany) by CoO)",
                                     "SD (timeorig_scl iso3c)" = "SD (Period of residence (CoO) by CoO)",
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
  tab_footnote(footnote = md("**Source**: SVR-Integrationsbarometer 2022; weighted"))

# # Appendix: Table 7b
mlm16.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
                        models = model.df %>% filter(sample == "formative years: 16") %>% pull(model),
                        stars = TRUE,
                        estimate = "{estimate}{stars}",
                        statistic = "({std.error})",
                        output = "gt",
                        coef_map = c("(Intercept)" = "Intercept",
                                     "genderFemale" = "Gender: Female",
                                     # "age" = "Age",
                                     "educmittel" = "Education: medium\n(Ref.: low)",
                                     "educhoch" = "Educ.: high",
                                     "educSchüler" = "Educ.: in school",
                                     "timedest_scl" = "Period of residence (Germany)",
                                     "timeorig_scl" = "Period of residence (Country of origin)",
                                     "muslimMuslim" = "Muslim",
                                     "refugeeYes" = "Refugee",
                                     "religion_str" = "Religiosity",
                                     "discriminationlow" = "Discrimination: low\n(Ref.: no at all)",
                                     "discriminationhigh" = "Dis.: high",
                                     "discriminationvery high" = "Dis.: very high",
                                     "v2xed_ed_dmcon_cgm" = "Political socialization [CGM] (V-Dem)",
                                     "v2x_polyarchy_cgm" = "Political socialization [CGM] (V-Dem)",
                                     "timedest_scl:v2xed_ed_dmcon_cgm" = "Period of residence (Germany) × V-Dem [CGM]",
                                     "v2xed_ed_dmcon_cgm:timedest_scl" = "Period of residence (Germany) × V-Dem [CGM]",
                                     "timeorig_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "v2xed_ed_dmcon_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "timedest_scl:v2x_polyarchy_cgm" = "Period of residence (Germany) × V-Dem [CGM]",
                                     "v2x_polyarchy_cgm:timedest_scl" = "Period of residence (Germany) × V-Dem [CGM]",
                                     "timeorig_scl:v2x_polyarchy_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "v2x_polyarchy_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "SD (Intercept iso3c)" = "SD (Intercept: CoO)",
                                     "SD (v2xed_ed_dmcon_cgm iso3c)" = "SD (V-Dem by CoO)",
                                     "SD (v2x_polyarchy_cgm iso3c)" = "SD (V-Dem by CoO)",
                                     "SD (timedest_scl iso3c)" = "SD (Period of residence (Germany) by CoO)",
                                     "SD (timeorig_scl iso3c)" = "SD (Period of residence (CoO) by CoO)",
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
  tab_footnote(footnote = md("**Source**: SVR-Integrationsbarometer 2022; weighted"))

#### Sample: Age <= 40 years ----
##### MLM ----
# Using 'model.df' created earlier
# adding sample excluding respondents <= 40 years
model40.df <- 
  tibble(
    sample = c("formative years: 14 + age < 40"),
    data = "ib_nest.df %>% filter(sample == 'formative years: 14') %>% pull(data) %>% .[[1]] %>% filter(age <= 40)"
  ) %>%
  expand_grid(model.df %>% 
                distinct(dv, type, iv, main_iv))

# Run models
model40.df <- model40.df %>%
  mutate(model = pmap(list(dv, iv, data), ~lme4::lmer(
    str_c(..1, " ~ ", ..2),
    REML = TRUE,
    control = lmerControl(
      optimizer = 'optimx', optCtrl = list(method = 'nlminb')),
    weights = weight, # or pweights_a for scaled weights
    data = eval(rlang::parse_expr(..3)))))

# Name list column
names(model40.df$model) <- model40.df$type

# Add robust SE using CR2::robust_mixed
# See: https://francish.net/post/robust-standard-errors/
model40.df <- model40.df %>%
  mutate(robust = map(model, ~robust_mixed(.x, type = "CR0")))

# Output
# modelsummary
mlm40.tbl <- modelsummary(title = md("**Multilevel Regression Model for Importance of Democracy**"),
                        models = model40.df %>% filter(sample == "formative years: 14 + age < 40") %>% pull(model), # pull(robust) for robust SE
                        stars = TRUE,
                        estimate = "{estimate}{stars}",
                        statistic = "({std.error})",
                        output = "gt",
                        coef_map = c("(Intercept)" = "Intercept",
                                     "genderFemale" = "Gender: Female",
                                     # "age" = "Age",
                                     "educmittel" = "Education: medium\n(Ref.: low)",
                                     "educhoch" = "Educ.: high",
                                     "educSchüler" = "Educ.: in school",
                                     "timedest_scl" = "Period of residence (Germany)",
                                     "timeorig_scl" = "Period of residence (Country of origin)",
                                     "muslimMuslim" = "Muslim",
                                     "refugeeYes" = "Refugee",
                                     "religion_str" = "Religiosity",
                                     "discriminationlow" = "Discrimination: low\n(Ref.: no at all)",
                                     "discriminationhigh" = "Dis.: high",
                                     "discriminationvery high" = "Dis.: very high",
                                     "v2xed_ed_dmcon_cgm" = "Political socialization [CGM] (V-Dem)",
                                     "v2x_polyarchy_cgm" = "Political socialization [CGM] (V-Dem)",
                                     "timedest_scl:v2xed_ed_dmcon_cgm" = "Period of residence (Germany) × V-Dem [CGM]",
                                     "v2xed_ed_dmcon_cgm:timedest_scl" = "Period of residence (Germany) × V-Dem [CGM]",
                                     "timeorig_scl:v2xed_ed_dmcon_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "v2xed_ed_dmcon_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "timedest_scl:v2x_polyarchy_cgm" = "Period of residence (Germany) × V-Dem [CGM]",
                                     "v2x_polyarchy_cgm:timedest_scl" = "Period of residence (Germany) × V-Dem [CGM]",
                                     "timeorig_scl:v2x_polyarchy_cgm" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "v2x_polyarchy_cgm:timeorig_scl" = "Period of residence (CoO) × V-Dem [CGM]",
                                     "SD (Intercept iso3c)" = "SD (Intercept: CoO)",
                                     "SD (v2xed_ed_dmcon_cgm iso3c)" = "SD (V-Dem by CoO)",
                                     "SD (v2x_polyarchy_cgm iso3c)" = "SD (V-Dem by CoO)",
                                     "SD (timedest_scl iso3c)" = "SD (Period of residence (Germany) by CoO)",
                                     "SD (timeorig_scl iso3c)" = "SD (Period of residence (CoO) by CoO)",
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
  tab_footnote(footnote = md("**Source**: SVR-Integrationsbarometer 2022; weighted"))

# Marginal effect plot 
# Using 'ggeffects'
#### Residence period (DEU) × VDem ----
# VDem: Democratic indoctrination content in education (v2xed_ed_dmcon)
residence_vind40.pred <- ggeffects::ggpredict(model = model40.df %>%
                                              filter(
                                                sample == "formative years: 14 + age < 40",
                                                type == "Random slope",
                                                main_iv == "vdem-ind") %>%
                                              pull(model) %>% 
                                              .[[1]], 
                                              terms = c("timedest_scl [all]", 
                                                        "v2xed_ed_dmcon_cgm [-0.35:0.35 by = 0.35]"),
                                              margin = "empirical",
                                              type = "fixed")

# VDem: Electoral democracy index (v2x_polyarchy)
residence_vpoly40.pred <- ggeffects::ggpredict(model = model40.df %>%
                                               filter(
                                                 sample == "formative years: 14 + age < 40",
                                                 type == "Random slope",
                                                 main_iv == "vdem-poly") %>%
                                               pull(model) %>% 
                                               .[[1]], 
                                               terms = c("timedest_scl [all]", 
                                                         "v2x_polyarchy_cgm [-0.35:0.35 by = 0.35]"),
                                               margin = "empirical",
                                               type = "fixed") 

#### Residence period (CoO) × VDem ----
# VDem: Democratic indoctrination content in education (v2xed_ed_dmcon)
residence_coo_vind40.pred <- ggeffects::ggpredict(model = model40.df %>%
                                                  filter(
                                                    sample == "formative years: 14 + age < 40",
                                                    type == "Random slope",
                                                    main_iv == "vdem-ind") %>%
                                                  pull(model) %>% 
                                                  .[[1]], 
                                                  terms = c("timeorig_scl [all]", 
                                                            "v2xed_ed_dmcon_cgm [-0.35:0.35 by = 0.35]"),
                                                  margin = "empirical",
                                                  type = "fixed") 

# VDem: Electoral democracy index (v2x_polyarchy)
residence_coo_vpoly40.pred <- ggeffects::ggpredict(model = model40.df %>%
                                                   filter(
                                                     sample == "formative years: 14 + age < 40",
                                                     type == "Random slope",
                                                     main_iv == "vdem-poly") %>%
                                                   pull(model) %>% 
                                                   .[[1]], 
                                                   terms = c("timeorig_scl [all]", 
                                                             "v2x_polyarchy_cgm [-0.35:0.35 by = 0.35]"),
                                                   margin = "empirical",
                                                   type = "fixed") 

# Residence in CoO and in Germany combined
# VDem: Democratic indoctrination content in education (v2xed_ed_dmcon)
residence_comb_vind40.fig <- residence_vind40.pred %>%
  as.data.frame() %>%
  mutate(where = "Country of destination") %>%
  bind_rows(residence_coo_vind40.pred %>%
              as.data.frame() %>%
              mutate(where = "Country of origin")) %>%
  ggplot(aes(x = x * 10, y = predicted, ymin = conf.low, ymax = conf.high, fill = where, linetype = where)) +
  geom_line() +
  geom_ribbon(alpha = .2) + 
  scale_x_continuous(breaks = seq(0, 60, 10), labels = seq(0, 60, 10)) +
  scale_y_continuous(limits = c(2, 3.5), breaks = seq(2.0, 3.0, .2), labels = seq(2.0, 3.0, 0.2)) +
  labs(title = 
         "Predicted democratic values by residence period and democratic indoctrination", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: Integration Barometer 2022; weighted data",
       x = "", y = "", 
       fill = "Residence period in: ", linetype = "Residence period in: ") +
  facet_wrap(~str_c("VDem: ", group)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0)) 

# Residence in CoO and in Germany combined
# VDem: Electoral democracy index (v2x_polyarchy)
residence_comb_vpoly40.fig <- residence_vpoly40.pred %>%
  as.data.frame() %>%
  mutate(where = "Country of destination") %>%
  bind_rows(residence_coo_vpoly40.pred %>%
              as.data.frame() %>%
              mutate(where = "Country of origin")) %>%
  ggplot(aes(x = x * 10, y = predicted, ymin = conf.low, ymax = conf.high, fill = where, linetype = where)) +
  geom_line() +
  geom_ribbon(alpha = .2) + 
  scale_x_continuous(breaks = seq(0, 60, 10), labels = seq(0, 60, 10)) +
  scale_y_continuous(limits = c(2, 3.5), breaks = seq(2, 3.0, .2), labels = seq(2, 3.0, 0.2)) +
  labs(title = 
         "Predicted democratic values by residence period and electoral democracy", 
       subtitle = "Holding covariates constant (at mean or reference category)",
       caption = "Source: Integration Barometer 2022; weighted data",
       x = "", y = "", 
       fill = "Residence period in: ", linetype = "Residence period in: ") +
  facet_wrap(~str_c("VDem: ", group)) +
  cowplot::theme_minimal_hgrid() +
  theme(plot.caption = element_text(hjust = 0)) 

# Fixed effect model ----
## Create data ----
# Create a variable that country years under autocracy
ib_nest.df <- ib_nest.df %>%
  filter(sample %in% str_c("formative years: ", c(12, 14, 16))) %>%
  mutate(data_long = map(
    data, ~.x %>%
      # Make the dataset long with one observation per year in CoO
      select(a_recno, iso3c, formative_year, wandjahr) %>%
      rowwise() %>%
      mutate(
        year = list(seq(formative_year, wandjahr))) %>%
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
        .by = c(a_recno)) %>%
      distinct(a_recno, e_v2x_polyarchy_3C_yrs))) 

# Join variable to original data and drop data 
# in artificial long format
ib_nest.df <- ib_nest.df %>%
  mutate(data = map2(.x = data, .y = data_long, 
                     ~left_join(.x, .y, by = "a_recno"))) %>%
  select(-data_long)

## Descriptive ----
ib_nest.df %>%
  filter(sample == "formative years: 14") %>%
  pull(data) %>%
  .[[1]] %>%
  filter(!is.na(e_v2x_polyarchy_3C_yrs)) %>%
  summarise(mu = wtd.mean(democ, na.rm = T, weights = weight), .by = "e_v2x_polyarchy_3C_yrs",
            n = n()) %>%
  ggplot(aes(x = e_v2x_polyarchy_3C_yrs, y = mu)) +
  geom_point(aes(size = n)) +
  geom_smooth(method = "lm")

## Model ---- 
ib_nest.df <- ib_nest.df %>%
  mutate(
    fe_model = map(
      .x = data,
      ~lm(
        democ ~ gender + educ + muslim + religion_str + discrimination + e_v2x_polyarchy_3C_yrs + timedest + iso3c,
        weights = weight, data = .x)),
    lm_model = map(
      .x = data, 
      ~lm(democ ~ gender + educ + muslim + religion_str + discrimination + e_v2x_polyarchy_3C_yrs + timedest,
      weights = weight, data = .x)))

# Predictions
ib_nest.df <- ib_nest.df %>%
  mutate(pred = map(
    fe_model, ~predictions(.x, newdata = datagrid(
      e_v2x_polyarchy_3C_yrs = seq(0, 45, 1))) %>%
      ggplot(., aes(x = e_v2x_polyarchy_3C_yrs, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_line() +
      geom_ribbon(alpha = .1) + 
      theme_ipsum()))

# Summarize
# Note needs models from model-ess.R from ESS
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

## Robustness ----
# Restrict to sample with age <= 40 years
ib_nest.df <- ib_nest.df %>%
  mutate(
    fe_model40 = map(
      .x = data,
      ~lm(
        democ ~ gender + educ + muslim + religion_str + discrimination + e_v2x_polyarchy_3C_yrs + timedest + iso3c,
        weights = weight, data = .x %>% filter(age <= 40))),
    lm_model40 = map(
      .x = data, 
      ~lm(democ ~ gender + educ + muslim + religion_str + discrimination + e_v2x_polyarchy_3C_yrs + timedest,
          weights = weight, data = .x %>% filter(age <= 40))))

# Predictions
ib_nest.df <- ib_nest.df %>%
  mutate(pred40 = map(
    fe_model40, ~predictions(.x, newdata = datagrid(
      e_v2x_polyarchy_3C_yrs = seq(0, 25, 1))) %>%
      ggplot(., aes(x = e_v2x_polyarchy_3C_yrs, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_line() +
      geom_ribbon(alpha = .1) + 
      theme_ipsum()))

# Summarize
# Note needs models from model-ess.R from ESS
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

# Export ----
## Main text ----
# Figure 1
ggsave(here("figure", "IB", "Fig1-Boxplot-VDem.png"), plot = democ_vdem.fig,
       dpi = "retina", device = ragg::agg_png, bg = "white",
       width = 25, height = 14, units = "cm")

# Figure 3a
# Electoral democracy
ggsave(here("figure", "IB", "Fig3a-residence_x_vdem-poly.png"), plot = residence_comb_vpoly.fig,
       dpi = 300, device = ragg::agg_png, bg = "white",
       width = 25, height = 14, units = "cm")

# Figure 3b
# Democratic indoctrination
ggsave(here("figure", "IB", "Fig3b-residence_x_vdem-ind.png"), plot = residence_comb_vind.fig,
       dpi = 300, device = ragg::agg_png, bg = "white",
       width = 25, height = 14, units = "cm")

# Table 1
# MLM results (age: 14, cut off: 16)
gtsave(mlm.tbl, filename = "./figure/IB/MLM_results.rtf")

## Appendix ----
# Figure 1a
ggsave(here("figure", "IB", "Democ-boxplot-IB.png"), plot = demo_boxplot_ib.fig,
       dpi = 300, device = ragg::agg_png(),
       width = 25, height = 15, units = "cm")

# Table 2a
gtsave(items_democ.gt, filename = here("figure", "IB", "Demo-items_IB.rtf"))

# Table 3a
gtsave(items_democ_poly_ib.gt, filename = here("figure", "IB", "Demo-VDem-Poly_IB.rtf"))

# Table 3b
gtsave(items_democ_ind_ib.gt, filename = here("figure", "IB", "Demo-VDem-Ind_IB.rtf"))

# Table 4a
gtsave(vdem_cor.tbl, filename = "./figure/VDem-correlation.png")

# Table 6 (left panel: Integration Barometer)
gtsave(ib_predictors.tbl, filename = here("figure", "IB", "Overview-Predictors_IB.rtf"))

# Figure 2a
ggsave(here("figure", "IB", "VDem-Development-CoO-Coverage.png"), plot = vdem_obs_dev_ib.fig,
       dpi = 300, device = ragg::agg_png(),
       width = 35, height = 20, units
       = "cm")

# Figure 3a
# Single items by VDem
ggsave(here("figure", "IB", "Single-items-VDEM-Polyarchy_Freqplot-IB.png"), plot = poly_item.fig,
       dpi = 300, device = ragg::agg_png(), bg = "white",
       width = 55, height = 20, units = "cm")

# Figure 3b
ggsave(here("figure", "IB", "Single-items-VDEM-Dmcon_Freqplot-IB.png"), plot = dmcon_item.fig,
       dpi = 300, device = ragg::agg_png(), bg = "white",
       width = 55, height = 20, units = "cm")


# Figure 4a
ggsave(here("figure", "IB", "residence_vdem_timeorig.png"), plot = democ_timeorig.fig,
       dpi = 300, device = ragg::agg_png, bg = "white",
       width = 25, height = 14, units = "cm")

# Table 7a
gtsave(mlm12.tbl, filename = "./figure/IB/MLM_results-Age12.rtf")

# Table 7b
gtsave(mlm16.tbl, filename = "./figure/IB/MLM_results-Age16.rtf")

# Table 9
gtsave(fe.tbl, filename = "./figure/IB/Fixed-Effect-Results.rtf")

# Table 10
gtsave(fe40.tbl, filename = "./figure/IB/Fixed-Effect-Results-Age40.rtf")

# Additional analyses
# Single items: Frequency plot
ggsave(here("figure", "IB", "Single-items-Freqplot-IB.png"), plot = demo_items_ib_freqplot.fig,
       dpi = 300, device = ragg::agg_png(), bg = "white",
       width = 50, height = 20, units = "cm")